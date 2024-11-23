# Load necessary library
library(stringr)
library(data.table)

# Set working directory to the parent folder containing the region folders
setwd("C:/Users/mgamb/Desktop/BIFAP_results")

# Create folders
# Main folders (g_intermediate, g_output)
invisible(ifelse(!dir.exists("Renamed"), dir.create("Renamed"), FALSE))

# Create Vector with Regions
regions<-c("AR","AS","CA","CL","CM","CN","MA","MU","NA")

# Rename all files in medicines folder and save into folder called Renamed
for (region in regions) {
  # Get the region name from the directory
  region_name<-basename(region)
  # List all .rds files in the current region directory
  rds_files<-list.files(path=paste0("C:/Users/mgamb/Desktop/BIFAP_results/",region,"/g_output/medicines_counts"),pattern="\\.rds$",full.names=TRUE,recursive=TRUE)
  # Loop through each file in the current region directory
  for (file_path in rds_files) {
    # Extract the file name
    file_name<-basename(file_path)
    # Construct the new file name
    new_file_name<-str_replace(file_name,"\\.rds$",paste0("_",region_name,".rds"))
    # Construct the new file path
    new_file_path<-file.path(paste0("C:/Users/mgamb/Desktop/BIFAP_results/Renamed/"),new_file_name)
    # Rename the file
    file.rename(file_path,new_file_path)
  }
}

# We need to combined similarly named files together

# Set the directory containing your files
directory<-"C:/Users/mgamb/Desktop/BIFAP_results/Renamed/"

# List all files in the directory
files<-list.files(directory,full.names = TRUE)

# Extract the base name without region and extension
file_info <- data.table(
  filepath=files,
  filename=basename(files),
  base_name=sub("_..$","", tools::file_path_sans_ext(basename(files)))
)

# Group files by their base name
grouped_files<-split(file_info$filepath,file_info$base_name)

# For each group bind and read in the files 
for (name in names(grouped_files)) {
  print(paste("Processing group:", name))
  # Get the list of files in the current group
  file_list<-grouped_files[[name]]
  # Read and bind files using do.call and lapply
  combined_data<-do.call(rbind,lapply(file_list,readRDS))
  
  # Check if the required columns "YM", "N", and "Freq" are present
  if (all(c("YM", "N", "Freq") %in% colnames(combined_data))) {
    
    # Keep only columns YM,N,Freq
    combined_data<-combined_data[,c("YM","N","Freq")]
    # Sum up N and Freq by Year
    combined_data[,N_all:=sum(N),by=list(YM)][,Freq_all:=sum(Freq),by=list(YM)]
    # Drop Columns Y and Freq
    combined_data<-combined_data[,-c("N","Freq")]
    # Keep 1 row per YM
    combined_data<-unique(combined_data,by=c("YM"))
    
    # Rates: Incidence, Prevalence and General Concomittance * 1000
    if (name=="PC_RAM_incidence_counts" | 
        name=="PC_Retinoid_prevalence_counts" | 
        name=="PC_RAM_general_concomit_USERS_counts" |
        name=="PC_Retinoid_incidence_counts.rds_masked" |
        name=="PC_Retinoid_prevalence_counts.rds_masked.rds" |
        name=="PC_HOSP_RAM_incidence_counts.rds" | 
        name=="PC_HOSP_Retinoid_prevalence_counts.rds" | 
        name=="PC_RAM_HOSP_general_concomit_USERS_counts.rds" |
        name=="PC_HOSP_Retinoid_incidence_counts.rds_masked.rds" |
        name=="PC_HOSP_Retinoid_prevalence_counts.rds_masked.rds"){
      
      # Calculate Rates
      combined_data[,rates:=round(as.numeric(N_all)/as.numeric(Freq_all),5)][is.nan(rates)|is.na(rates),rates:=0][,rates:=rates*1000]
      
    } else if(name=="PC_Retinoid_discontinued_counts.rds" |
              name=="PC_RAM_switcher_1_counts.rds" | 
              name=="PC_RAM_switcher_2_counts.rds" | 
              name=="PC_Retinoid_discontinued_counts.rds_masked.rds" |
              name=="PC_HOSP_Retinoid_discontinued_counts.rds" |
              name=="PC_HOSP_RAM_switcher_1_counts.rds" | 
              name=="PC_HOSP_RAM_switcher_2_counts.rds" | 
              name=="PC_HOSP_Retinoid_discontinued_counts.rds_masked.rds") {
      
      # Calculate Rates
      combined_data[,rates:=round(as.numeric(N_all)/as.numeric(Freq_all),5)][is.nan(rates)|is.na(rates),rates:=0][,rates:=rates*100]
      
    } else {
      
      # Calculate Rates
      combined_data[,rates:=round(as.numeric(N_all)/as.numeric(Freq_all),5)][is.nan(rates)|is.na(rates),rates:=0]
      
    }
    
    combined_data[,masked:=0]
    # Create folder for combined records
    invisible(ifelse(!dir.exists("Pooled"), dir.create("Pooled"), FALSE))
    pooled_dir<-"C:/Users/mgamb/Desktop/BIFAP_results/Pooled/"
    # Save or further process totals for YM, N, Freq files
    saveRDS(combined_data, file.path(pooled_dir, paste0(name, "_pooled.rds")))
    
  } else if(all(c("ATC.RAM") %in% names(combined_data))){
    # Sum up N and Freq by Year
    combined_data[,pre_all:=sum(pre),by=list(ATC.RAM)][,post_all:=sum(post),by=list(ATC.RAM)]
    # Drop columns you don't need 
    combined_data<-combined_data[,-c("pre","post")]
    # Keep 1 row per YM
    combined_data<-unique(combined_data,by=c("ATC.RAM"))
    # Create folder for combined records
    invisible(ifelse(!dir.exists("Pooled"), dir.create("Pooled"), FALSE))
    pooled_dir<-"C:/Users/mgamb/Desktop/BIFAP_results/Pooled/"
    # Save or further process totals for YM, N, Freq files
    saveRDS(combined_data, file.path(pooled_dir, paste0(name, "_pooled.rds")))
    
  } else if (name=="PC_RAM_generalconcomit_RECORDS_counts" |
             name=="PC_HOSP_RAM_generalconcomit_RECORDS_counts"){
    
    # Keep only columns YM,N,Freq
    combined_data<-combined_data[,c("YM","N")]
    # Sum up N and Freq by Year
    combined_data[,N_all:=sum(N),by=list(YM)]
    # Drop Columns Y and Freq
    combined_data<-combined_data[,-c("N")]
    # Keep 1 row per YM
    combined_data<-unique(combined_data,by=c("YM"))
    
    # Create folder for combined records
    invisible(ifelse(!dir.exists("Pooled"), dir.create("Pooled"), FALSE))
    pooled_dir<-"C:/Users/mgamb/Desktop/BIFAP_results/Pooled/"
    # Save or further process totals for YM, N, Freq files
    saveRDS(combined_data, file.path(pooled_dir, paste0(name, "_pooled.rds")))
    
  } else {
    print(paste("Not Processed: ", name))

  }
}

