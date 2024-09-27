# Clean up
rm(list= grep("^RAM|baseline|df|flowchart|^retinoid_prev|^retinoid_study|^age_at", ls(), value = TRUE))
# Create folders for renamed and pooled data 
invisible(ifelse(!dir.exists(paste0(projectFolder, "/renamed")), dir.create(paste0(projectFolder, "/renamed")), FALSE))
invisible(ifelse(!dir.exists(paste0(projectFolder, "/pooled")), dir.create(paste0(projectFolder, "/pooled")), FALSE))
invisible(ifelse(!dir.exists(paste0(projectFolder, "/baselinetables")), dir.create(paste0(projectFolder, "/baselinetables")), FALSE))

#####################################################################################################################
################################################ LOAD DATA SETS #####################################################
#####################################################################################################################
# Load Retinoid Study Population 
# Rename all files in medicines folder and save into folder called Renamed
for (region in regions) {
  # Get the region name from the directory
  region_name<-basename(region)
  # List all .rds files in the current region directory
  rds_files<-list.files(path=paste0(projectFolder,"/",region,"/g_intermediate/populations/"),pattern="study_population",full.names=TRUE,recursive=TRUE)
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

# Load RAM patient data 
# Rename all files in medicines folder and save into folder called Renamed
for (region in regions) {
  # Get the region name from the directory
  region_name<-basename(region)
  # List all .rds files in the current region directory
  rds_files<-list.files(path=paste0(projectFolder,"/",region,"/g_intermediate/tmp/medications/"),pattern="altmed|Retinoid",full.names=TRUE,recursive=TRUE)
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


##############################################################################################
##############################################################################################
##############################################################################################
# Function to check if file exists
load_if_exists<-function(file_path){if(file.exists(file_path)){return(as.data.table(readRDS(file_path)))}else{print(paste("File does not exist:", file_path))}}


##############################################################################################
########################################## PC SUBPOP #########################################
##############################################################################################
# load data
PC_study_population<-load_if_exists(paste0(projectFolder,"/pooled/PC_study_population_pooled.rds"))
PC_retinoid_study_population<-load_if_exists(paste0(projectFolder,"/pooled/PC_retinoid_study_population_pooled.rds"))
PC_retinoid_meds<-load_if_exists(paste0(projectFolder,"/pooled/PC_Retinoid_MEDS_pooled.rds"))
PC_retinoid_prevalence<-load_if_exists(paste0(projectFolder,"/pooled/PC_Retinoid_prevalence_data_pooled.rds"))
PC_RAM_meds<-as.data.table(do.call(rbind,lapply(paste0(paste0(projectFolder,"/pooled/"),list.files(paste0(projectFolder, "/pooled/"),pattern="PC_altmed")),readRDS)))

pop_prefix<-"PC"
### WOCBP ###
# data assignments
study_population<-PC_study_population
retinoid_study_population<-PC_retinoid_study_population
retinoid_meds<-PC_retinoid_meds
retinoid_prevalence_data<-PC_retinoid_prevalence
RAM_meds_WOCBP<-PC_RAM_meds

# clean up
study_population[,entry_date:=as.IDate(entry_date,"%Y%m%d")][,exit_date:=as.IDate(exit_date,"%Y%m%d")][,birth_date:=as.IDate(birth_date,"%Y%m%d")]
# create column for follow up duration 
study_population[,fu_dur_days:=exit_date-entry_date]
# create column for age_groups - based on age at entry into study date
study_population[,age_at_entry_date:=floor((entry_date-birth_date)/365.25)]
study_population[,age_groups:=ifelse(study_population[,age_at_entry_date>= 12&age_at_entry_date<21],"12-20.99", 
                                     ifelse(study_population[,age_at_entry_date>=21&age_at_entry_date<31],"21-30.99",
                                            ifelse(study_population[,age_at_entry_date>=31&age_at_entry_date<41],"31-40.99",
                                                   ifelse(study_population[,age_at_entry_date>=41&age_at_entry_date<56],"41-55.99","Not in range"))))]
# get one row per patient 
pop_WOCBP<-unique(study_population[,c("person_id","exit_date","fu_dur_days","age_at_entry_date","age_groups")],by="person_id")


### RETINOID USERS IN WOCBP ###
# retinoid prescriptions need to fall between entry into study and exit out of study dates - we use entry_date -90 because prescriptions could have been started before entry into study date and be actively used as we go into the study period 
retinoid_meds<-retinoid_meds[Date>=entry_date-90 & Date<=exit_date,]
# get one row per patient 
retinoid_users<-as.data.table(unique(retinoid_meds[,c("person_id")], by = c("person_id")))
# create a subset of the study population who are retinoid users 
pop_retinoidusers<-pop_WOCBP[person_id%in%retinoid_users$person_id,]


### RAM USERS IN WOCBP ###
# RAM prescriptions need to fall between entry into study and exit out of study dates - we use entry_date -90 because prescriptions could have been started before entry into study date and be actively used as we go into the study period 
RAM_meds_WOCBP<-RAM_meds_WOCBP[Date>=entry_date-90 & Date<=exit_date,]
# get one row per patient 
RAM_in_WOCBP_users<-as.data.table(unique(RAM_meds_WOCBP[,c("person_id","Date")], by = c("person_id")))
# create a subset of the study population who are retinoid users 
pop_RAMusers_in_WOCBP<-pop_WOCBP[person_id%in%RAM_in_WOCBP_users$person_id,]

if(nrow(retinoid_study_population)>0){
  ### RAM USERS IN RETINOID STUDY POPULATION ###
  # we need to get the earliest date of retinoid use 
  retinoid_prevalence_data_first<-retinoid_prevalence_data[order(episode.start),.SD[1],by=person_id]
  # merge retinoid prevalence data with RAM users in WOCBP - we need RAM meds that do not occur in retinoid users or after a first retinoid prescription 
  RAM_meds_in_retinoidusers<-merge(retinoid_prevalence_data_first[,c("person_id","ATC.retinoid","episode.start")],RAM_meds_WOCBP[,c("person_id","Code","Date")],by="person_id")
  # Remove any prescriptions where RAM prescription date occurs before retinoid episode start date
  RAM_meds_in_retinoidusers<-RAM_meds_in_retinoidusers[Date>=episode.start,]
  # get one row per patient 
  pop_RAMusers_in_retinoidpop<-as.data.table(unique(RAM_meds_in_retinoidusers,by=c("person_id")))
  # Merge with pop_WOCBP to get newly created variables 
  pop_RAMusers_in_retinoidpop<-merge(pop_RAMusers_in_retinoidpop,pop_WOCBP,by="person_id")
  
  ### RAM USERS IN RETINOID STUDY POPULATION - PER INDICATION ###
  pop_RAMusers_in_retinoidpop_acne<-pop_RAMusers_in_retinoidpop[ATC.retinoid=="D10BA01",]
  pop_RAMusers_in_retinoidpop_derm<-pop_RAMusers_in_retinoidpop[ATC.retinoid=="D11AH04",]
  pop_RAMusers_in_retinoidpop_psor<-pop_RAMusers_in_retinoidpop[ATC.retinoid=="D05BB02",]
}

# Create list for populations if they exist
Pops_for_baseline_tables <- list()

# Define the names of datasets
dataset_names <- c("pop_WOCBP", 
                   "pop_retinoidusers",
                   "pop_RAMusers_in_WOCBP",
                   "pop_RAMusers_in_retinoidpop",
                   "pop_RAMusers_in_retinoidpop_acne",
                   "pop_RAMusers_in_retinoidpop_derm",
                   "pop_RAMusers_in_retinoidpop_psor")

# Check for existence and add to the list
for (name in dataset_names) {
  if (exists(name)) {
    Pops_for_baseline_tables[[name]] <- get(name)
  } else {
    cat(paste(name, "does not exist.\n"))
  }
}

# Create names only for those that exist in the list
names_for_populations <- c("1_WOCBP", 
                           "2_retinoidUsers",
                           "3_RAMUsersInWOCBP",
                           "4_RAMUsersInRetinoidPop",
                           "5_RAMUsersInRetinoidPopAcne", 
                           "6_RAMUsersInRetinoidPopDermatitis",
                           "7_RAMUsersInRetinoidPopPsoriasis")

# Filter the names to match the actual datasets in the list
existing_names <- names_for_populations[seq_along(Pops_for_baseline_tables)]

# Assign names only if they exist
names(Pops_for_baseline_tables) <- existing_names

# Create baseline tables for each population
for (i in seq_along(Pops_for_baseline_tables)) {
  df <- Pops_for_baseline_tables[[i]]
  
  if (nrow(df) > 0) {  # Ensure df has rows
    ################## BASELINE ALL POPULATION ########################
    # Calculates median of followup in years 
    fu_median     <- median(df$fu_dur_days) / 365.25
    fu_IQR        <- IQR(df$fu_dur_days) / 365.25
    fu_min        <- min(df$fu_dur_days) / 365.25
    fu_max        <- max(df$fu_dur_days) / 365.25
    max_exit_date <- max(df$exit_date, na.rm = TRUE)
    
    # fu_SD
    age_at_ID_mean <- mean(df$age_at_entry_date, na.rm = TRUE)
    age_at_ID_SD   <- sd(df$age_at_entry_date, na.rm = TRUE)
    
    # Calculations 
    age_at_ID_12_20.99_count <- sum(df$age_groups == "12-20.99", na.rm = TRUE) 
    age_at_ID_21_30.99_count <- sum(df$age_groups == "21-30.99", na.rm = TRUE)
    age_at_ID_31_40.99_count <- sum(df$age_groups == "31-40.99", na.rm = TRUE) 
    age_at_ID_41_55.99_count <- sum(df$age_groups == "41-55.99", na.rm = TRUE)
    
    # Calculates percentages
    age_at_ID_12_20.99_perc  <- (age_at_ID_12_20.99_count / nrow(df)) * 100
    age_at_ID_21_30.99_perc  <- (age_at_ID_21_30.99_count / nrow(df)) * 100
    age_at_ID_31_40.99_perc  <- (age_at_ID_31_40.99_count / nrow(df)) * 100
    age_at_ID_41_55.99_perc  <- (age_at_ID_41_55.99_count / nrow(df)) * 100
    
    # Create dataframe for results
    names <- c("Follow-up, years - median",
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
    
    values <- c(as.character(round(fu_median, 1)),
                as.character(round(fu_IQR, 1)),
                as.character(round(fu_min, 2)),
                as.character(round(fu_max, 2)),
                as.character(max_exit_date),
                as.character(round(age_at_ID_mean, 1)),
                as.character(round(age_at_ID_SD, 1)),
                as.character(age_at_ID_12_20.99_count),
                as.character(round(age_at_ID_12_20.99_perc, 1)),
                as.character(age_at_ID_21_30.99_count),
                as.character(round(age_at_ID_21_30.99_perc, 1)),
                as.character(age_at_ID_31_40.99_count),
                as.character(round(age_at_ID_31_40.99_perc, 1)),
                as.character(age_at_ID_41_55.99_count),
                as.character(round(age_at_ID_41_55.99_perc, 1)))
    
    # Creates baseline table
    baseline <- data.table(names, values)
    
    # Saves files
    print(paste0("Saving baseline table: ", pop_prefix, "_", names(Pops_for_baseline_tables)[i]))
    saveRDS(baseline, file.path(paste0(projectFolder, "/baselinetables/",pop_prefix, "_", names(Pops_for_baseline_tables)[i], "_baseline.rds")))
  } else {
    print(paste("Skipping", names(Pops_for_baseline_tables)[i], "- no rows to process."))
  }
}



# List of dataset file names (assuming they are saved as .rds files with the same names)
file_names <- c(paste0(pop_prefix,"_1_WOCBP_baseline.rds"), 
                paste0(pop_prefix,"_2_retinoidUsers_baseline.rds"),
                paste0(pop_prefix,"_3_RAMUsersInWOCBP_baseline.rds"),
                paste0(pop_prefix,"_4_RAMUsersInRetinoidPop_baseline.rds"),
                paste0(pop_prefix,"_5_RAMUsersInRetinoidPopAcne_baseline.rds"), 
                paste0(pop_prefix,"_6_RAMUsersInRetinoidPopDermatitis_baseline.rds"),
                paste0(pop_prefix,"_7_RAMUsersInRetinoidPopPsoriasis_baseline.rds"))

# Corresponding column names for values
value_col_names <- c("WOCBP", 
                     "retinoidUsers", 
                     "RAMUsersInWOCBP", 
                     "RAMUsersInRetinoidPop", 
                     "RAMUsersInRetinoidPopAcne", 
                     "RAMUsersInRetinoidPopDermatitis", 
                     "RAMUsersInRetinoidPopPsoriasis")

# Initialize an empty list to hold the datasets
datasets <- list()

# Read the datasets, rename the value column, and store them in a list
for (i in seq_along(file_names)) {
  file_path <- file.path(paste0(projectFolder, "/baselinetables/"), file_names[i])
  
  # Check if the file exists before trying to read it
  if (file.exists(file_path)) {
    df <- readRDS(file_path)
    colnames(df)[colnames(df) == "values"] <- value_col_names[i]
    datasets[[i]] <- df  # Add the dataframe to the list
  } else {
    # warning(paste("File not found:", file_path))  # Optional: print a warning for missing files
    print(paste0("No baseline tables for",file_path))
  }
}

# Remove any NULL entries from the list (in case some datasets weren't read)
datasets <- datasets[!sapply(datasets, is.null)]

# Combine datasets by merging them on the 'names' column, preserving row order
if (length(datasets) > 0) {  # Check if there are datasets to combine
  combined_data <- Reduce(function(x, y) {
    merge(x, y, by = "names", all = TRUE, sort = FALSE)
  }, datasets)
  
  # Set the order of combined_data based on the original order of the first dataset
  baselinetables_combined <- combined_data[match(datasets[[1]]$names, combined_data$names), ]
} else {
  baselinetables_combined <- data.frame(names = character(), stringsAsFactors = FALSE)  # Empty data frame if no datasets exist
}

saveRDS(baselinetables_combined, paste0(projectFolder, "/baselinetables/", pop_prefix, "_combined_baseline_tables.rds"))

# # Print Nrows
# # List of dataset names
# dataset_names <- c("pop_WOCBP",
#                    "pop_retinoidusers",
#                    "pop_RAMusers_in_WOCBP",
#                    "pop_RAMusers_in_retinoidpop",
#                    "pop_RAMusers_in_retinoidpop_acne",
#                    "pop_RAMusers_in_retinoidpop_derm",
#                    "pop_RAMusers_in_retinoidpop_psor")
# 
# # Loop through each dataset name
# for (name in dataset_names) {
#   if (exists(name)) {  # Check if the dataset exists in the global environment
#     cat(paste(name, "has", nrow(get(name)), "rows.\n"))  # Print the number of rows
#   } else {
#     cat(paste(name, "does not exist.\n"))  # Optional: Print a message if it does not exist
#   }
# }


rm(list = grep("pop_RAMusers_in_retinoidpop|pop_WOCBP|pop_retinoidusers|pop_RAMusers_in_WOCBP|pop_RAMusers_in_retinoidpop_acne|pop_RAMusers_in_retinoidpop_derm|pop_RAMusers_in_retinoidpop_psor", ls(), value = TRUE))


##############################################################################################
########################################## PC_HOSP SUBPOP #########################################
##############################################################################################
# load data
#PC_HOSP subpop
PC_HOSP_study_population<-load_if_exists(paste0(projectFolder,"/pooled/PC_HOSP_study_population_pooled.rds"))
PC_HOSP_retinoid_study_population<-load_if_exists(paste0(projectFolder,"/pooled/PC_HOSP_retinoid_study_population_pooled.rds"))
PC_HOSP_retinoid_meds<-load_if_exists(paste0(projectFolder,"/pooled/PC_HOSP_Retinoid_MEDS_pooled.rds"))
PC_HOSP_retinoid_prevalence<-load_if_exists(paste0(projectFolder,"/pooled/PC_HOSP_Retinoid_prevalence_data_pooled.rds"))
PC_HOSP_RAM_meds<-as.data.table(do.call(rbind,lapply(paste0(paste0(projectFolder,"/pooled/"),list.files(paste0(projectFolder, "/pooled/"),pattern="PC_HOSP_altmed")),readRDS)))

pop_prefix<-"PC_HOSP"
### WOCBP ###
# data assignments
study_population<-PC_HOSP_study_population
retinoid_study_population<-PC_HOSP_retinoid_study_population
retinoid_meds<-PC_HOSP_retinoid_meds
retinoid_prevalence_data<-PC_HOSP_retinoid_prevalence
RAM_meds_WOCBP<-PC_HOSP_RAM_meds

# clean up
study_population[,entry_date:=as.IDate(entry_date,"%Y%m%d")][,exit_date:=as.IDate(exit_date,"%Y%m%d")][,birth_date:=as.IDate(birth_date,"%Y%m%d")]
# create column for follow up duration 
study_population[,fu_dur_days:=exit_date-entry_date]
# create column for age_groups - based on age at entry into study date
study_population[,age_at_entry_date:=floor((entry_date-birth_date)/365.25)]
study_population[,age_groups:=ifelse(study_population[,age_at_entry_date>= 12&age_at_entry_date<21],"12-20.99", 
                                     ifelse(study_population[,age_at_entry_date>=21&age_at_entry_date<31],"21-30.99",
                                            ifelse(study_population[,age_at_entry_date>=31&age_at_entry_date<41],"31-40.99",
                                                   ifelse(study_population[,age_at_entry_date>=41&age_at_entry_date<56],"41-55.99","Not in range"))))]
# get one row per patient 
pop_WOCBP<-unique(study_population[,c("person_id","exit_date","fu_dur_days","age_at_entry_date","age_groups")],by="person_id")


### RETINOID USERS IN WOCBP ###
# retinoid prescriptions need to fall between entry into study and exit out of study dates - we use entry_date -90 because prescriptions could have been started before entry into study date and be actively used as we go into the study period 
retinoid_meds<-retinoid_meds[Date>=entry_date-90 & Date<=exit_date,]
# get one row per patient 
retinoid_users<-as.data.table(unique(retinoid_meds[,c("person_id")], by = c("person_id")))
# create a subset of the study population who are retinoid users 
pop_retinoidusers<-pop_WOCBP[person_id%in%retinoid_users$person_id,]


### RAM USERS IN WOCBP ###
# RAM prescriptions need to fall between entry into study and exit out of study dates - we use entry_date -90 because prescriptions could have been started before entry into study date and be actively used as we go into the study period 
RAM_meds_WOCBP<-RAM_meds_WOCBP[Date>=entry_date-90 & Date<=exit_date,]
# get one row per patient 
RAM_in_WOCBP_users<-as.data.table(unique(RAM_meds_WOCBP[,c("person_id","Date")], by = c("person_id")))
# create a subset of the study population who are retinoid users 
pop_RAMusers_in_WOCBP<-pop_WOCBP[person_id%in%RAM_in_WOCBP_users$person_id,]

if(nrow(retinoid_study_population)>0){
  ### RAM USERS IN RETINOID STUDY POPULATION ###
  # we need to get the earliest date of retinoid use 
  retinoid_prevalence_data_first<-retinoid_prevalence_data[order(episode.start),.SD[1],by=person_id]
  # merge retinoid prevalence data with RAM users in WOCBP - we need RAM meds that do not occur in retinoid users or after a first retinoid prescription 
  RAM_meds_in_retinoidusers<-merge(retinoid_prevalence_data_first[,c("person_id","ATC.retinoid","episode.start")],RAM_meds_WOCBP[,c("person_id","Code","Date")],by="person_id")
  # Remove any prescriptions where RAM prescription date occurs before retinoid episode start date
  RAM_meds_in_retinoidusers<-RAM_meds_in_retinoidusers[Date>=episode.start,]
  # get one row per patient 
  pop_RAMusers_in_retinoidpop<-as.data.table(unique(RAM_meds_in_retinoidusers,by=c("person_id")))
  # Merge with pop_WOCBP to get newly created variables 
  pop_RAMusers_in_retinoidpop<-merge(pop_RAMusers_in_retinoidpop,pop_WOCBP,by="person_id")
  
  ### RAM USERS IN RETINOID STUDY POPULATION - PER INDICATION ###
  pop_RAMusers_in_retinoidpop_acne<-pop_RAMusers_in_retinoidpop[ATC.retinoid=="D10BA01",]
  pop_RAMusers_in_retinoidpop_derm<-pop_RAMusers_in_retinoidpop[ATC.retinoid=="D11AH04",]
  pop_RAMusers_in_retinoidpop_psor<-pop_RAMusers_in_retinoidpop[ATC.retinoid=="D05BB02",]
}

# Create list for populations if they exist
Pops_for_baseline_tables <- list()

# Define the names of datasets
dataset_names <- c("pop_WOCBP", 
                   "pop_retinoidusers",
                   "pop_RAMusers_in_WOCBP",
                   "pop_RAMusers_in_retinoidpop",
                   "pop_RAMusers_in_retinoidpop_acne",
                   "pop_RAMusers_in_retinoidpop_derm",
                   "pop_RAMusers_in_retinoidpop_psor")

# Check for existence and add to the list
for (name in dataset_names) {
  if (exists(name)) {
    Pops_for_baseline_tables[[name]] <- get(name)
  } else {
    cat(paste(name, "does not exist.\n"))
  }
}

# Create names only for those that exist in the list
names_for_populations <- c("1_WOCBP", 
                           "2_retinoidUsers",
                           "3_RAMUsersInWOCBP",
                           "4_RAMUsersInRetinoidPop",
                           "5_RAMUsersInRetinoidPopAcne", 
                           "6_RAMUsersInRetinoidPopDermatitis",
                           "7_RAMUsersInRetinoidPopPsoriasis")

# Filter the names to match the actual datasets in the list
existing_names <- names_for_populations[seq_along(Pops_for_baseline_tables)]

# Assign names only if they exist
names(Pops_for_baseline_tables) <- existing_names

# Create baseline tables for each population
for (i in seq_along(Pops_for_baseline_tables)) {
  df <- Pops_for_baseline_tables[[i]]
  
  if (nrow(df) > 0) {  # Ensure df has rows
    ################## BASELINE ALL POPULATION ########################
    # Calculates median of followup in years 
    fu_median     <- median(df$fu_dur_days) / 365.25
    fu_IQR        <- IQR(df$fu_dur_days) / 365.25
    fu_min        <- min(df$fu_dur_days) / 365.25
    fu_max        <- max(df$fu_dur_days) / 365.25
    max_exit_date <- max(df$exit_date, na.rm = TRUE)
    
    # fu_SD
    age_at_ID_mean <- mean(df$age_at_entry_date, na.rm = TRUE)
    age_at_ID_SD   <- sd(df$age_at_entry_date, na.rm = TRUE)
    
    # Calculations 
    age_at_ID_12_20.99_count <- sum(df$age_groups == "12-20.99", na.rm = TRUE) 
    age_at_ID_21_30.99_count <- sum(df$age_groups == "21-30.99", na.rm = TRUE)
    age_at_ID_31_40.99_count <- sum(df$age_groups == "31-40.99", na.rm = TRUE) 
    age_at_ID_41_55.99_count <- sum(df$age_groups == "41-55.99", na.rm = TRUE)
    
    # Calculates percentages
    age_at_ID_12_20.99_perc  <- (age_at_ID_12_20.99_count / nrow(df)) * 100
    age_at_ID_21_30.99_perc  <- (age_at_ID_21_30.99_count / nrow(df)) * 100
    age_at_ID_31_40.99_perc  <- (age_at_ID_31_40.99_count / nrow(df)) * 100
    age_at_ID_41_55.99_perc  <- (age_at_ID_41_55.99_count / nrow(df)) * 100
    
    # Create dataframe for results
    names <- c("Follow-up, years - median",
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
    
    values <- c(as.character(round(fu_median, 1)),
                as.character(round(fu_IQR, 1)),
                as.character(round(fu_min, 2)),
                as.character(round(fu_max, 2)),
                as.character(max_exit_date),
                as.character(round(age_at_ID_mean, 1)),
                as.character(round(age_at_ID_SD, 1)),
                as.character(age_at_ID_12_20.99_count),
                as.character(round(age_at_ID_12_20.99_perc, 1)),
                as.character(age_at_ID_21_30.99_count),
                as.character(round(age_at_ID_21_30.99_perc, 1)),
                as.character(age_at_ID_31_40.99_count),
                as.character(round(age_at_ID_31_40.99_perc, 1)),
                as.character(age_at_ID_41_55.99_count),
                as.character(round(age_at_ID_41_55.99_perc, 1)))
    
    # Creates baseline table
    baseline <- data.table(names, values)
    
    # Saves files
    print(paste0("Saving baseline table: ", pop_prefix, "_", names(Pops_for_baseline_tables)[i]))
    saveRDS(baseline, file.path(paste0(projectFolder, "/baselinetables/",pop_prefix, "_", names(Pops_for_baseline_tables)[i], "_baseline.rds")))
  } else {
    print(paste("Skipping", names(Pops_for_baseline_tables)[i], "- no rows to process."))
  }
}



# List of dataset file names (assuming they are saved as .rds files with the same names)
file_names <- c(paste0(pop_prefix,"_1_WOCBP_baseline.rds"), 
                paste0(pop_prefix,"_2_retinoidUsers_baseline.rds"),
                paste0(pop_prefix,"_3_RAMUsersInWOCBP_baseline.rds"),
                paste0(pop_prefix,"_4_RAMUsersInRetinoidPop_baseline.rds"),
                paste0(pop_prefix,"_5_RAMUsersInRetinoidPopAcne_baseline.rds"), 
                paste0(pop_prefix,"_6_RAMUsersInRetinoidPopDermatitis_baseline.rds"),
                paste0(pop_prefix,"_7_RAMUsersInRetinoidPopPsoriasis_baseline.rds"))

# Corresponding column names for values
value_col_names <- c("WOCBP", 
                     "retinoidUsers", 
                     "RAMUsersInWOCBP", 
                     "RAMUsersInRetinoidPop", 
                     "RAMUsersInRetinoidPopAcne", 
                     "RAMUsersInRetinoidPopDermatitis", 
                     "RAMUsersInRetinoidPopPsoriasis")

# Initialize an empty list to hold the datasets
datasets <- list()

# Read the datasets, rename the value column, and store them in a list
for (i in seq_along(file_names)) {
  file_path <- file.path(paste0(projectFolder, "/baselinetables/"), file_names[i])
  
  # Check if the file exists before trying to read it
  if (file.exists(file_path)) {
    df <- readRDS(file_path)
    colnames(df)[colnames(df) == "values"] <- value_col_names[i]
    datasets[[i]] <- df  # Add the dataframe to the list
  } else {
    # warning(paste("File not found:", file_path))  # Optional: print a warning for missing files
    print(paste0("No baseline tables for",file_path))
  }
}

# Remove any NULL entries from the list (in case some datasets weren't read)
datasets <- datasets[!sapply(datasets, is.null)]

# Combine datasets by merging them on the 'names' column, preserving row order
if (length(datasets) > 0) {  # Check if there are datasets to combine
  combined_data <- Reduce(function(x, y) {
    merge(x, y, by = "names", all = TRUE, sort = FALSE)
  }, datasets)
  
  # Set the order of combined_data based on the original order of the first dataset
  baselinetables_combined <- combined_data[match(datasets[[1]]$names, combined_data$names), ]
} else {
  baselinetables_combined <- data.frame(names = character(), stringsAsFactors = FALSE)  # Empty data frame if no datasets exist
}

saveRDS(baselinetables_combined, paste0(projectFolder, "/baselinetables/", pop_prefix, "_combined_baseline_tables.rds"))

# # Print Nrows
# # List of dataset names
# dataset_names <- c("pop_WOCBP",
#                    "pop_retinoidusers",
#                    "pop_RAMusers_in_WOCBP",
#                    "pop_RAMusers_in_retinoidpop",
#                    "pop_RAMusers_in_retinoidpop_acne",
#                    "pop_RAMusers_in_retinoidpop_derm",
#                    "pop_RAMusers_in_retinoidpop_psor")
# 
# # Loop through each dataset name
# for (name in dataset_names) {
#   if (exists(name)) {  # Check if the dataset exists in the global environment
#     cat(paste(name, "has", nrow(get(name)), "rows.\n"))  # Print the number of rows
#   } else {
#     cat(paste(name, "does not exist.\n"))  # Optional: Print a message if it does not exist
#   }
# }


rm(list = grep("pop_RAMusers_in_retinoidpop|pop_WOCBP|pop_retinoidusers|pop_RAMusers_in_WOCBP|pop_RAMusers_in_retinoidpop_acne|pop_RAMusers_in_retinoidpop_derm|pop_RAMusers_in_retinoidpop_psor", ls(), value = TRUE))


