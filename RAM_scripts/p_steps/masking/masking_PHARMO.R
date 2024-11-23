# masks values less than 5 => if value is less than 5 then it will be show as 5 
library(data.table)

# Path to PHARMO data
input_folder<-"C:/Users/mgamb/Desktop/DAP_results/g_output/medicines_counts"
output_folder<-"C:/Users/mgamb/Desktop/DAP_results/g_output/masked"

# List all files in this directory
med_files<-list.files(input_folder,pattern=".rds",recursive=TRUE,full.names=TRUE)

# For each file check if it has the columns YM, N and Freq. If so check the values of the columns and apply masking 
for(file in med_files){
  # Read the .rds file as a data.table
  data<-as.data.table(readRDS(file))
  
  # Check if the required columns "YM", "N", and "Freq" are present
  if (all(c("YM", "N", "Freq", "rates") %in% colnames(data))) {
    
    # Check if any values in "N" or "Freq" are less than 5, and replace them with 5
    data[N<5|Freq<5,masked:=1]
    data[N<5,N:=5][Freq<5,Freq:=5]
    
    # Incidence, Prevalence and General Concomittance * 1000
    if (basename(file)=="ALL_RAM_incidence_counts.rds" | 
        basename(file)=="ALL_RAM_prevalence_counts.rds" | 
        basename(file)=="ALL_RAM_general_concomit_USERS_counts.rds" |
        basename(file)=="ALL_Retinoid_incidence_counts.rds" |
        basename(file)=="ALL_Retinoid_prevalence_counts.rds"){
      
      data[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*1000]
      # Discontinued and Switcher * 100
    } else if(basename(file)=="ALL_RAM_discontinued_counts.rds" |
              basename(file)=="ALL_RAM_switcher_1_counts.rds" | 
              basename(file)=="ALL_RAM_switcher_2_counts.rds" | 
              basename(file)=="ALL_Retinoid_discontinued_counts.rds"){
      
      data[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*100]
      # Contra, Teratogenic and Stratified counts * 1
    } else {
      data[,rates:=round(as.numeric(N)/as.numeric(Freq),5)]
    }
    
    # Save the modified data to the output folder with the same name but "masked" appended
    output_file<-file.path(output_folder,paste0(basename(file), "_masked.rds"))
    saveRDS(data, output_file)
  }
  # For individual ATC counts 
  if (all(c("ATC.RAM", "pre", "post") %in% colnames(data))) {
    data[,masked_pre:=0][,masked_post:=0]
  
    # Check if values are below 5
    data[pre<5,masked_pre:=1][pre<5,pre:=5]
    data[post<5,masked_post:=1][post<5,post:=5]
    # Save the modified data to the output folder with the same name but "masked" appended
    output_file<-file.path(output_folder,paste0(basename(file), "_masked.rds"))
    saveRDS(data, output_file)
  }
}


