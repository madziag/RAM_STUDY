# Clean up
rm(list= grep("^RAM|baseline|df|flowchart|^retinoid_prev|^retinoid_study|^age_at", ls(), value = TRUE))
# Create folders for renamed and pooled data 
invisible(ifelse(!dir.exists(paste0(projectFolder, "/renamed")), dir.create(paste0(projectFolder, "/renamed")), FALSE))
invisible(ifelse(!dir.exists(paste0(projectFolder, "/pooled")), dir.create(paste0(projectFolder, "/pooled")), FALSE))
invisible(ifelse(!dir.exists(paste0(projectFolder, "/baselinetables")), dir.create(paste0(projectFolder, "/baselinetables")), FALSE))

#####################################################################################################################
################################################ LOAD DATA SETS #####################################################
#####################################################################################################################

# Load RAM patient data 
# Rename all files in medicines folder and save into folder called Renamed
for (region in regions) {
  # Get the region name from the directory
  region_name<-basename(region)
  # List all .rds files in the current region directory
  rds_files<-list.files(path=paste0(projectFolder,"/",region,"/g_intermediate/tmp/medications/"),pattern="altmed",full.names=TRUE,recursive=TRUE)
  # Loop through each file in the current region directory
  for (file_path in rds_files) {
    # Extract the file name
    file_name<-basename(file_path)
    # Construct the new file name
    new_file_name<-str_replace(file_name,"\\.rds$",paste0("_",region_name,".rds"))
    # Construct the new file path
    new_file_path<-file.path(paste0(projectFolder,"/renamed"),new_file_name)
    # Rename the file
    file.copy(file_path,new_file_path)
    
  }
}

# Load Retinoid Prevalent Patient Data
# Rename all files in medicines folder and save into folder called Renamed
for (region in regions) {
  # Get the region name from the directory
  region_name<-basename(region)
  # List all .rds files in the current region directory
  rds_files<-list.files(path=paste0(projectFolder,"/",region,"/g_intermediate/counts_dfs/retinoid_counts/"),pattern="Retinoid_prevalence_data",full.names=TRUE,recursive=TRUE)
  # Loop through each file in the current region directory
  for (file_path in rds_files) {
    # Extract the file name
    file_name<-basename(file_path)
    # Construct the new file name
    new_file_name<-str_replace(file_name,"\\.rds$",paste0("_",region_name,".rds"))
    # Construct the new file path
    new_file_path<-file.path(paste0(projectFolder,"/renamed"),new_file_name)
    # Rename the file
    file.copy(file_path,new_file_path)
    
  }
}

# Load Retinoid Study Population 
# Rename all files in medicines folder and save into folder called Renamed
for (region in regions) {
  # Get the region name from the directory
  region_name<-basename(region)
  # List all .rds files in the current region directory
  rds_files<-list.files(path=paste0(projectFolder,"/",region,"/g_intermediate/populations/"),pattern="retinoid_study_population",full.names=TRUE,recursive=TRUE)
  # Loop through each file in the current region directory
  for (file_path in rds_files) {
    # Extract the file name
    file_name<-basename(file_path)
    # Construct the new file name
    new_file_name<-str_replace(file_name,"\\.rds$",paste0("_",region_name,".rds"))
    # Construct the new file path
    new_file_path<-file.path(paste0(projectFolder,"/renamed"),new_file_name)
    # Rename the file
    file.copy(file_path,new_file_path)
  }
}

#####################################################################################################################
#################################### POOL DATA SETS FROM ALL REGIONS ################################################
#####################################################################################################################

# List all files in the directory
renamed_files<-list.files(paste0(projectFolder,"/renamed"),full.names = TRUE)

# Extract the base name without region and extension
file_info <- data.table(
  filepath=renamed_files,
  filename=basename(renamed_files),
  base_name=sub("_..$","", tools::file_path_sans_ext(basename(renamed_files)))
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
  # Save or further process totals for YM, N, Freq files
  saveRDS(combined_data, file.path(paste0(projectFolder,"/pooled/", name, "_pooled.rds")))
}


#####################################################################################################################
######################### KEEP FIRST EVER USE OF RETINOID PER PATIENT  ##########################################
#####################################################################################################################

# Reduce Retinoid Prevalence File to have one row per patient ID - only the earliest intance of Retinoid use
retinoid_prev_files<-list.files(paste0(projectFolder,"/pooled"),pattern= "Retinoid_prevalence_data")

for(file in retinoid_prev_files){
  pop <-gsub("_Retinoid_prevalence_data_pooled.rds", "", file)
  # Read in file
  retinoid_prev_data<-as.data.table(readRDS(paste0(projectFolder,"/pooled/",file)))
  # Rename episode start column
  setnames(retinoid_prev_data, old=c("episode.start", "episode.day"), new=c("episode.start.retinoid", "episode.day.retinoid"))
  # Create subset where we have only the first retinoid use per person id
  retinoid_first_use<-retinoid_prev_data[,.SD[which.min(episode.day.retinoid)],by=person_id]
  # Rename variable based on population
  if(pop=="PC")      {PC_retinoid_first_use      <-retinoid_first_use}
  if(pop=="PC_HOSP") {PC_HOSP_retinoid_first_use <-retinoid_first_use}
  if(pop=="ALL")     {ALL_retinoid_first_use     <-retinoid_first_use}
}

#####################################################################################################################
######################### CREATE DATA FOR EACH BASELINE TABLE #############################################
#####################################################################################################################
# Get all retinoid study populations
retinoid_study_pop_files<-list.files(paste0(projectFolder,"/pooled"),pattern= "retinoid_study_population")

for(file in retinoid_study_pop_files){
  # Get population prefix
  pop <-gsub("_retinoid_study_population_pooled.rds","", file)
  # Read in file
  study_pop<-as.data.table(readRDS(paste0(projectFolder,"/pooled/",file)))
  # Check if there is at least one row of data
  if(nrow(study_pop)>0){
    # Correct date format
    study_pop[,entry_date:=as.IDate(entry_date,"%Y%m%d")][,exit_date:=as.IDate(exit_date,"%Y%m%d")][,birth_date:=as.IDate(birth_date,"%Y%m%d")]
    # Creates column in study population: fu_dur_days
    study_pop[,fu_dur_days:=exit_date-entry_date]
    # Creates age variable in study population = entry_date - birth date  (# Rounds down)
    study_pop[,age_at_entry_date:=floor((entry_date-birth_date)/365.25)]
    # Create age groups
    study_pop[age_at_entry_date>=12&age_at_entry_date<21,age_groups:="12-20.99"]
    study_pop[age_at_entry_date>=21&age_at_entry_date<31,age_groups:="21-30.99"]
    study_pop[age_at_entry_date>=31&age_at_entry_date<41,age_groups:="31-40.99"]
    study_pop[age_at_entry_date>=41&age_at_entry_date<56,age_groups:="41-55.99"]
    study_pop[is.na(age_groups),age_groups:="Not in range"]
    # Remove any duplicates
    study_pop<-unique(study_pop, by=c("person_id","birth_date"))

    if(pop=="PC"){
      # Rename variable based on population
      PC_study_pop<-study_pop
      # Load all RAMs into 1 data frame
      PC_RAM_meds<-as.data.table(do.call(rbind,lapply(paste0(projectFolder,"/pooled/", list.files(paste0(projectFolder,"/pooled/"), pattern="PC_altmed")), readRDS)))
      # Merge RAM's with Retinoid first use
      PC_RAM_meds<-PC_retinoid_first_use[,c("person_id","ATC.retinoid","episode.day.retinoid","episode.start.retinoid")][PC_RAM_meds,on=.(person_id)]
      # Create subset which doesn't take retinoid use into consideration
      # PC_RAM_meds_all<-PC_RAM_meds
      # Keep records where RAM date is after retinoid date and RAM date falls within entry and exit dates
      PC_RAM_meds_with_prior_retinoid_use<-PC_RAM_meds[Date>=episode.day.retinoid & Date>=entry_date-90 & Date<=exit_date,c("person_id","ATC.retinoid","Code","Date","episode.day.retinoid")]
      # Rename column names
      setnames(PC_RAM_meds_with_prior_retinoid_use, old=c("Code", "Date"), new=c("ATC.RAM", "RAM.rxdate"))
      # Merge with study pop to get baseline characteristics
      PC_RAM_meds_with_prior_retinoid_use<-PC_study_pop[,c("person_id","birth_date","entry_date","exit_date","fu_dur_days","age_at_entry_date","age_groups")][PC_RAM_meds_with_prior_retinoid_use,on=.(person_id)]
      # Create subsets for each of the indications - based on the Retinoid not RAM
      PC_RAM_meds_with_prior_retinoid_use_psoriasis<-PC_RAM_meds_with_prior_retinoid_use[ATC.retinoid=="D05BB02",][,indication:="psoriasis"]
      PC_RAM_meds_with_prior_retinoid_use_acne<-PC_RAM_meds_with_prior_retinoid_use[ATC.retinoid=="D10BA01",][,indication:="acne"]
      PC_RAM_meds_with_prior_retinoid_use_dermatitis<-PC_RAM_meds_with_prior_retinoid_use[ATC.retinoid=="D11AH04",][,indication:="dermatitis"]
      # Remove duplicates for each group
      PC_RAM_meds_with_prior_retinoid_use                   <-unique(PC_RAM_meds_with_prior_retinoid_use,by="person_id")
      PC_RAM_meds_with_prior_retinoid_use_psoriasis_unique  <-unique(PC_RAM_meds_with_prior_retinoid_use_psoriasis,by="person_id")
      PC_RAM_meds_with_prior_retinoid_use_acne_unique       <-unique(PC_RAM_meds_with_prior_retinoid_use_acne,by="person_id")
      PC_RAM_meds_with_prior_retinoid_use_dermatitis_unique <-unique(PC_RAM_meds_with_prior_retinoid_use_dermatitis,by="person_id")

      # # Data for baseline not taking retinoid use into consideration
      # # Keep records where RAM date is after retinoid date and RAM date falls within entry and exit dates
      # PC_RAM_meds_all<-PC_RAM_meds_all[Date>=entry_date-90 & Date<=exit_date,c("person_id","ATC.retinoid","Code","Date","episode.day.retinoid")]
      # # Rename column names
      # setnames(PC_RAM_meds_all, old=c("Code", "Date"), new=c("ATC.RAM", "RAM.rxdate"))
      # # Merge with study pop to get baseline characteristics
      # PC_RAM_meds_all<-PC_study_pop[,c("person_id","birth_date","entry_date","exit_date","fu_dur_days","age_at_entry_date","age_groups")][PC_RAM_meds_all,on=.(person_id)]
      # # Remove any RAMs which do not have a retinoid record attached to it
      # PC_RAM_meds_all<-PC_RAM_meds_all[!is.na(episode.day.retinoid),]
    }

    if(pop=="PC_HOSP"){
      # Rename variable based on population
      PC_HOSP_study_pop<-study_pop
      # Load all RAMs into 1 data frame
      PC_HOSP_RAM_meds<-as.data.table(do.call(rbind,lapply(paste0(projectFolder,"/pooled/", list.files(paste0(projectFolder,"/pooled/"), pattern="PC_HOSP_altmed")), readRDS)))
      # Merge RAM's with Retinoid first use
      PC_HOSP_RAM_meds<-PC_HOSP_retinoid_first_use[,c("person_id","ATC.retinoid","episode.day.retinoid","episode.start.retinoid")][PC_HOSP_RAM_meds,on=.(person_id)]
      # Create subset which doesn't take retinoid use into consideration
      # PC_HOSP_RAM_meds_all<-PC_HOSP_RAM_meds
      # Keep records where RAM date is after retinoid date and RAM date falls within entry and exit dates
      PC_HOSP_RAM_meds_with_prior_retinoid_use<-PC_HOSP_RAM_meds[Date>=episode.day.retinoid & Date>=entry_date-90 & Date<=exit_date,c("person_id","ATC.retinoid","Code","Date","episode.day.retinoid")]
      # Rename column names
      setnames(PC_HOSP_RAM_meds_with_prior_retinoid_use, old=c("Code", "Date"), new=c("ATC.RAM", "RAM.rxdate"))
      # Merge with study pop to get baseline characteristics
      PC_HOSP_RAM_meds_with_prior_retinoid_use<-PC_HOSP_study_pop[,c("person_id","birth_date","entry_date","exit_date","fu_dur_days","age_at_entry_date","age_groups")][PC_HOSP_RAM_meds_with_prior_retinoid_use,on=.(person_id)]
      # Create subsets for each of the indications - based on the Retinoid not RAM
      PC_HOSP_RAM_meds_with_prior_retinoid_use_psoriasis<-PC_HOSP_RAM_meds_with_prior_retinoid_use[ATC.retinoid=="D05BB02",][,indication:="psoriasis"]
      PC_HOSP_RAM_meds_with_prior_retinoid_use_acne<-PC_HOSP_RAM_meds_with_prior_retinoid_use[ATC.retinoid=="D10BA01",][,indication:="acne"]
      PC_HOSP_RAM_meds_with_prior_retinoid_use_dermatitis<-PC_HOSP_RAM_meds_with_prior_retinoid_use[ATC.retinoid=="D11AH04",][,indication:="dermatitis"]
      # Remove duplicates for each group
      PC_HOSP_RAM_meds_with_prior_retinoid_use                   <-unique(PC_HOSP_RAM_meds_with_prior_retinoid_use,by="person_id")
      PC_HOSP_RAM_meds_with_prior_retinoid_use_psoriasis_unique  <-unique(PC_HOSP_RAM_meds_with_prior_retinoid_use_psoriasis,by="person_id")
      PC_HOSP_RAM_meds_with_prior_retinoid_use_acne_unique       <-unique(PC_HOSP_RAM_meds_with_prior_retinoid_use_acne,by="person_id")
      PC_HOSP_RAM_meds_with_prior_retinoid_use_dermatitis_unique <-unique(PC_HOSP_RAM_meds_with_prior_retinoid_use_dermatitis,by="person_id")

      # # Data for baseline not taking retinoid use into consideration
      # # Keep records where RAM date is after retinoid date and RAM date falls within entry and exit dates
      # PC_HOSP_RAM_meds_all<-PC_HOSP_RAM_meds_all[Date>=entry_date-90 & Date<=exit_date,c("person_id","ATC.retinoid","Code","Date","episode.day.retinoid")]
      # # Rename column names
      # setnames(PC_HOSP_RAM_meds_all, old=c("Code", "Date"), new=c("ATC.RAM", "RAM.rxdate"))
      # # Merge with study pop to get baseline characteristics
      # PC_HOSP_RAM_meds_all<-PC_HOSP_study_pop[,c("person_id","birth_date","entry_date","exit_date","fu_dur_days","age_at_entry_date","age_groups")][PC_HOSP_RAM_meds_all,on=.(person_id)]
      # # Remove any RAMs which do not have a retinoid record attached to it
      # PC_HOSP_RAM_meds_all<-PC_HOSP_RAM_meds_all[!is.na(episode.day.retinoid),]
    }
  } else {
    print(paste0("There is no study population for subpop: ", pop))
  }
}


#####################################################################################################################
######################### CREATE LIST FOR BASELINE TABLES #############################################
#####################################################################################################################

# Create a list of all the data you want baseline tables for
Pops_for_baseline_tables<-list(PC_study_pop,
                               PC_RAM_meds_with_prior_retinoid_use,
                               PC_RAM_meds_with_prior_retinoid_use_psoriasis_unique,
                               PC_RAM_meds_with_prior_retinoid_use_acne_unique,
                               PC_RAM_meds_with_prior_retinoid_use_dermatitis_unique,
                               # PC_RAM_meds_all,

                               PC_HOSP_study_pop,
                               PC_HOSP_RAM_meds_with_prior_retinoid_use,
                               PC_HOSP_RAM_meds_with_prior_retinoid_use_psoriasis_unique,
                               PC_HOSP_RAM_meds_with_prior_retinoid_use_acne_unique,
                               PC_HOSP_RAM_meds_with_prior_retinoid_use_dermatitis_unique,
                               # PC_HOSP_RAM_meds_all
)



names(Pops_for_baseline_tables)<-c("PC_Study_Population",
                                   "PC_RAM_with_prior_retinoid_use",
                                   "PC_RAM_with_prior_retinoid_use_Psoriasis",
                                   "PC_RAM_with_prior_retinoid_use_Acne",
                                   "PC_RAM_with_prior_retinoid_use_Dermatitis",
                                   # "PC_RAM_all",

                                   "PC_HOSP_Study_Population",
                                   "PC_HOSP_RAM_with_prior_retinoid_use",
                                   "PC_HOSP_RAM_with_prior_retinoid_use_Psoriasis",
                                   "PC_HOSP_RAM_with_prior_retinoid_use_Acne",
                                   "PC_HOSP_RAM_with_prior_retinoid_use_Dermatitis"
                                   # "PC_HOSP_RAM_all"
)


################################################################################################################
########################################### CREATE BASELINE TABLES #############################################
################################################################################################################

# For each of the Populations
for (i in 1:length(Pops_for_baseline_tables)){

  df<-Pops_for_baseline_tables[[i]]

  if(nrow(df>0)){
    ################## BASELINE ALL POPULATION ########################
    # Calculates median of followup in years
    fu_median     <- median(df$fu_dur_days)/365.25
    fu_IQR        <- IQR(df$fu_dur_days)/365.25
    fu_min        <- min(df$fu_dur_days)/365.25
    fu_max        <- max(df$fu_dur_days)/365.25
    max_exit_date <- max(df$exit_date)
    # fu_SD
    age_at_ID_mean <-mean(df$age_at_entry_date)
    age_at_ID_SD   <-sd(df$age_at_entry_date)

    # Calculations
    age_at_ID_12_20.99_count <- sum(df$age_groups == "12-20.99")
    age_at_ID_21_30.99_count <- sum(df$age_groups == "21-30.99")
    age_at_ID_31_40.99_count <- sum(df$age_groups == "31-40.99")
    age_at_ID_41_55.99_count <- sum(df$age_groups == "41-55.99")
    # Calculates percentages
    age_at_ID_12_20.99_perc  <- (age_at_ID_12_20.99_count/nrow(df)) * 100
    age_at_ID_21_30.99_perc  <- (age_at_ID_21_30.99_count/nrow(df)) * 100
    age_at_ID_31_40.99_perc  <- (age_at_ID_31_40.99_count/nrow(df)) * 100
    age_at_ID_41_55.99_perc  <- (age_at_ID_41_55.99_count/nrow(df)) * 100

    # Create dataframe
    names<-c("Follow-up, years - median",
             "Follow-up, years - IQR",
             "Follow-up, years - min",
             "Follow-up, years - max",
             "Max exit date",
             "Age at index date (study entry) - mean",
             "Age at index date (study entry) - sd",
             "12.0-20.99 years_count",
             "12.0-20.99 years_perc",
             "21.0-30.99 years_count",
             "21.0-30.99 years_perc",
             "31.0-40.99 years_count",
             "31.0-40.99 years_perc",
             "41.0-55.99 years_count",
             "41.0-55.99 years_perc")

    values<-c(as.character(round(fu_median,1)),
              as.character(round(fu_IQR,1)),
              as.character(round(fu_min,2)),
              as.character(round(fu_max,2)),
              as.character(max_exit_date),
              as.character(round(age_at_ID_mean,1)),
              as.character(round(age_at_ID_SD,1)),
              as.character(age_at_ID_12_20.99_count),
              as.character(round(age_at_ID_12_20.99_perc,1)),
              as.character(age_at_ID_21_30.99_count),
              as.character(round(age_at_ID_21_30.99_perc,1)),
              as.character(age_at_ID_31_40.99_count),
              as.character(round(age_at_ID_31_40.99_perc,1)),
              as.character(age_at_ID_41_55.99_count),
              as.character(round(age_at_ID_41_55.99_perc),1))

    # Creates baseline table
    baseline<-data.table(names,values)
    # print statement
    print(paste0("Saving baseline table: ", names(Pops_for_baseline_tables[i])))
    # Save baseline table
    saveRDS(baseline, paste0(projectFolder,"/baselinetables/", names(Pops_for_baseline_tables[i]),".rds"))
  }
}




