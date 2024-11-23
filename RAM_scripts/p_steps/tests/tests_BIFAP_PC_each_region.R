# clean up
rm(list=ls())

# Load libraries
library(data.table)

# load datasets 
# Function to check if file exists
load_if_exists<-function(file_path){if(file.exists(file_path)){return(as.data.table(readRDS(file_path)))}else{print(paste("File does not exist:", file_path))}}

# Set paths (to where g_output is found)
g_output_dir<-"C:/Users/mgamb/Desktop/DAP_results/g_output"

# Paths to folders 
obj1_dir<-paste0(g_output_dir,"/medicines_counts/RAM_Objective_1")
obj2_dir<-paste0(g_output_dir,"/medicines_counts/RAM_Objective_2")
obj3_dir<-paste0(g_output_dir,"/medicines_counts/RAM_Objective_3")
obj4_dir<-paste0(g_output_dir,"/medicines_counts/RAM_Objective_4")

# Load data 
### OBJECTIVE 1: INCIDENCE ###

incidence_all<-load_if_exists(paste0(obj1_dir,"/PC_RAM_incidence_counts.rds"))

incidence_age_group_2099<-load_if_exists(paste0(obj1_dir,"/Stratified/PC_RAM_incidence_counts_12-20.99_age_group.rds"))
incidence_age_group_3099<-load_if_exists(paste0(obj1_dir,"/Stratified/PC_RAM_incidence_counts_21-30.99_age_group.rds"))
incidence_age_group_4099<-load_if_exists(paste0(obj1_dir,"/Stratified/PC_RAM_incidence_counts_31-40.99_age_group.rds"))
incidence_age_group_5599<-load_if_exists(paste0(obj1_dir,"/Stratified/PC_RAM_incidence_counts_41-55.99_age_group.rds"))

incidence_indication_acne<-load_if_exists(paste0(obj1_dir,"/Stratified/PC_RAM_incidence_counts_acne_indication_group.rds"))
incidence_indication_derm<-load_if_exists(paste0(obj1_dir,"/Stratified/PC_RAM_incidence_counts_dermatitis_indication_group.rds"))
incidence_indication_psor<-load_if_exists(paste0(obj1_dir,"/Stratified/PC_RAM_incidence_counts_psoriasis_indication_group.rds"))

### OBJECTIVE 1: PREVALENCE ###

prevalence_all<-load_if_exists(paste0(obj1_dir,"/PC_RAM_prevalence_counts.rds"))

prevalence_age_group_2099<-load_if_exists(paste0(obj1_dir,"/Stratified/PC_RAM_prevalence_counts_12-20.99_age_group.rds"))
prevalence_age_group_3099<-load_if_exists(paste0(obj1_dir,"/Stratified/PC_RAM_prevalence_counts_21-30.99_age_group.rds"))
prevalence_age_group_4099<-load_if_exists(paste0(obj1_dir,"/Stratified/PC_RAM_prevalence_counts_31-40.99_age_group.rds"))
prevalence_age_group_5599<-load_if_exists(paste0(obj1_dir,"/Stratified/PC_RAM_prevalence_counts_41-55.99_age_group.rds"))

prevalence_indication_acne<-load_if_exists(paste0(obj1_dir,"/Stratified/PC_RAM_prevalence_counts_acne_indication_group.rds"))
prevalence_indication_derm<-load_if_exists(paste0(obj1_dir,"/Stratified/PC_RAM_prevalence_counts_dermatitis_indication_group.rds"))
prevalence_indication_psor<-load_if_exists(paste0(obj1_dir,"/Stratified/PC_RAM_prevalence_counts_psoriasis_indication_group.rds"))

### OBJECTIVE 2: DISCONTINUED ###

discontinued_all<-load_if_exists(paste0(obj2_dir,"/PC_RAM_discontinued_counts.rds"))

discontinued_age_group_2099<-load_if_exists(paste0(obj2_dir,"/Stratified/PC_RAM_discontinued_counts_12-20.99_age_group.rds"))
discontinued_age_group_3099<-load_if_exists(paste0(obj2_dir,"/Stratified/PC_RAM_discontinued_counts_21-30.99_age_group.rds"))
discontinued_age_group_4099<-load_if_exists(paste0(obj2_dir,"/Stratified/PC_RAM_discontinued_counts_31-40.99_age_group.rds"))
discontinued_age_group_5599<-load_if_exists(paste0(obj2_dir,"/Stratified/PC_RAM_discontinued_counts_41-55.99_age_group.rds"))

discontinued_indication_acne<-load_if_exists(paste0(obj2_dir,"/Stratified/PC_RAM_discontinued_counts_acne_indication_group.rds"))
discontinued_indication_derm<-load_if_exists(paste0(obj2_dir,"/Stratified/PC_RAM_discontinued_counts_dermatitis_indication_group.rds"))
discontinued_indication_psor<-load_if_exists(paste0(obj2_dir,"/Stratified/PC_RAM_discontinued_counts_psoriasis_indication_group.rds"))

### OBJECTIVE 2: SWITCHERS ###

switcher_1<-load_if_exists(paste0(obj2_dir,"/PC_RAM_switcher_1_counts.rds"))
switcher_2<-load_if_exists(paste0(obj2_dir,"/PC_RAM_switcher_2_counts.rds"))

switcher_age_group_2099<-load_if_exists(paste0(obj2_dir,"/Stratified/PC_RAM_switcher_counts_12-20.99_age_group.rds"))
switcher_age_group_3099<-load_if_exists(paste0(obj2_dir,"/Stratified/PC_RAM_switcher_counts_21-30.99_age_group.rds"))
switcher_age_group_4099<-load_if_exists(paste0(obj2_dir,"/Stratified/PC_RAM_switcher_counts_31-40.99_age_group.rds"))
switcher_age_group_5599<-load_if_exists(paste0(obj2_dir,"/Stratified/PC_RAM_switcher_counts_41-55.99_age_group.rds"))

switcher_indication_acne<-load_if_exists(paste0(obj2_dir,"/Stratified/PC_RAM_switcher_counts_acne_indication_group.rds"))
switcher_indication_derm<-load_if_exists(paste0(obj2_dir,"/Stratified/PC_RAM_switcher_counts_dermatitis_indication_group.rds"))
switcher_indication_psor<-load_if_exists(paste0(obj2_dir,"/Stratified/PC_RAM_switcher_counts_psoriasis_indication_group.rds"))

### OBJECTIVE 3: GENERAL CONCOMITANCE ###
genconcomit_all_records<-load_if_exists(paste0(obj3_dir,"/PC_RAM_generalconcomit_RECORDS_counts.rds"))
genconcomit_all_users<-load_if_exists(paste0(obj3_dir,"/PC_RAM_general_concomit_USERS_counts.rds"))

genconcomit_age_group_2099<-load_if_exists(paste0(obj3_dir,"/Stratified/PC_RAM_general_concomit_USERS_counts_12-20.99_age_group.rds"))
genconcomit_age_group_3099<-load_if_exists(paste0(obj3_dir,"/Stratified/PC_RAM_general_concomit_USERS_counts_21-30.99_age_group.rds"))
genconcomit_age_group_4099<-load_if_exists(paste0(obj3_dir,"/Stratified/PC_RAM_general_concomit_USERS_counts_31-40.99_age_group.rds"))
genconcomit_age_group_5599<-load_if_exists(paste0(obj3_dir,"/Stratified/PC_RAM_general_concomit_USERS_counts_41-55.99_age_group.rds"))

genconcomit_indication_acne<-load_if_exists(paste0(obj3_dir,"/Stratified/PC_RAM_general_concomit_USERS_counts_acne_indication_group.rds"))
genconcomit_indication_derm<-load_if_exists(paste0(obj3_dir,"/Stratified/PC_RAM_general_concomit_USERS_counts_dermatitis_indication_group.rds"))
genconcomit_indication_psor<-load_if_exists(paste0(obj3_dir,"/Stratified/PC_RAM_general_concomit_USERS_counts_psoriasis_indication_group.rds"))

### OBJECTIVE 3: CONTRAINDICATED ###
contra_all_records<-load_if_exists(paste0(obj3_dir,"/PC_RAM_contra_RECORDS_counts.rds"))
contra_all_users<-load_if_exists(paste0(obj3_dir,"/PC_RAM_contra_USERS_counts.rds"))

contra_indication_acne<-load_if_exists(paste0(obj3_dir,"/Stratified/PC_RAM_contra_USERS_counts_acne_indication_group.rds"))
contra_indication_derm<-load_if_exists(paste0(obj3_dir,"/Stratified/PC_RAM_contra_USERS_counts_dermatitis_indication_group.rds"))
contra_indication_psor<-load_if_exists(paste0(obj3_dir,"/Stratified/PC_RAM_contra_USERS_counts_psoriasis_indication_group.rds"))

### OBJECTIVE 4: TERATOGENIC ###

teratogenic_all_records<-load_if_exists(paste0(obj4_dir,"/PC_RAM_teratogenic_RECORDS_counts.rds"))
teratogenic_all_users<-load_if_exists(paste0(obj4_dir,"/PC_RAM_teratogenic_USERS_counts.rds"))

teratogenic_indication_acne<-load_if_exists(paste0(obj4_dir,"/Stratified/PC_RAM_teratogenic_USERS_counts_acne_indication_group.rds"))
teratogenic_indication_derm<-load_if_exists(paste0(obj4_dir,"/Stratified/PC_RAM_teratogenic_USERS_counts_dermatitis_indication_group.rds"))
teratogenic_indication_psor<-load_if_exists(paste0(obj4_dir,"/Stratified/PC_RAM_teratogenic_USERS_counts_psoriasis_indication_group.rds"))

### RAM COUNTS ###

RAM_records_all<-load_if_exists(paste0(g_output_dir,"/medicines_counts/PC_RAM_record_counts_overall.rds"))

RAM_records_acne<-load_if_exists(paste0(g_output_dir,"/medicines_counts/PC_RAM_record_counts_acne.rds"))
RAM_records_derm<-load_if_exists(paste0(g_output_dir,"/medicines_counts/PC_RAM_record_counts_dermatitis.rds"))
RAM_records_psor<-load_if_exists(paste0(g_output_dir,"/medicines_counts/PC_RAM_record_counts_psoriasis.rds"))

RAM_users_all<-load_if_exists(paste0(g_output_dir,"/medicines_counts/PC_RAM_user_counts_overall.rds"))

RAM_users_acne<-load_if_exists(paste0(g_output_dir,"/medicines_counts/PC_RAM_user_counts_acne.rds"))
RAM_users_derm<-load_if_exists(paste0(g_output_dir,"/medicines_counts/PC_RAM_user_counts_dermatitis.rds"))
RAM_users_psor<-load_if_exists(paste0(g_output_dir,"/medicines_counts/PC_RAM_user_counts_psoriasis.rds"))

### RETINOID COUNTS ###
retinoid_incidence<-load_if_exists(paste0(g_output_dir,"/medicines_counts/PC_Retinoid_incidence_counts.rds"))
retinoid_prevalence<-load_if_exists(paste0(g_output_dir,"/medicines_counts/PC_Retinoid_prevalence_counts.rds"))
retinoid_discontinued<-load_if_exists(paste0(g_output_dir,"/medicines_counts/PC_Retinoid_discontinued_counts.rds"))

### POPULATION COUNTS ###
population_counts_flowchart<-load_if_exists(paste0(g_output_dir,"/baseline_tables/PC_flowchart_baseline_tables.rds"))

###############################################################################
################################# OBJECTIVE 1 #################################
###############################################################################

################################## INCIDENCE ##################################
# Numerator   = number of new RAM users (no use the year prior) per calendar month 
# Denominator = subset of WOCBP with at least one retinoid prescription within study period 
# Stratified by age group and indication 

################################## PREVALENCE #################################
# Numerator   = number of RAM users per calendar month with at least one day of exposure that month 
# Denominator = subset of WOCBP with at least one retinoid prescription within study period 
# Stratified by age group and indication 

### TESTS ###
# 1. Incidence Numerator <= Prevalence Denominator 
# 2. Incidence Denominator == Prevalence Denominator 
# 3. Sum of all incidence age group numerators == Incidence age group denominator == RAM Incidence Numerator 
# 4. Sum of all incidence indication numerators == Incidence indication denominator == RAM Incidence Numerator 
# 5. Sum of all prevalence age group numerators == Prevalence age group denominator == RAM Prevalence Numerator 
# 6. Sum of all prevalence indication numerators == Prevalence indication denominator == RAM Prevalence Numerator 

###############################################################################
###############################################################################
###############################################################################

# FOR PC/PC_HOSP
# 1. Incidence Numerator <= Prevalence Denominator 
incidence_prevalence_merged<-merge(incidence_all[,.(YM,N,Freq)],prevalence_all[,.(YM,N,Freq)],by="YM",suffixes=c("_incidence","_prevalence"))
#check
incidence.num_morethan_prevalence.num<-nrow(incidence_prevalence_merged[N_incidence>N_prevalence,])

# 2. Incidence Denominator == Prevalence Denominator 
#check
incidence.denom_notequal_prevalence.denom<-nrow(incidence_prevalence_merged[Freq_incidence!=Freq_prevalence,])

# 3. Sum of all incidence age group numerators == Incidence age group denominator == RAM Incidence Numerator 
# combine age group data into a single table 

# Create a list of the datasets
datasets <- list(
  if (exists("incidence_age_group_2099") && is.data.table(incidence_age_group_2099)) incidence_age_group_2099[,.(YM,N,Freq)],
  if (exists("incidence_age_group_3099") && is.data.table(incidence_age_group_3099)) incidence_age_group_3099[,.(YM,N,Freq)],
  if (exists("incidence_age_group_4099") && is.data.table(incidence_age_group_4099)) incidence_age_group_4099[,.(YM,N,Freq)],
  if (exists("incidence_age_group_5599") && is.data.table(incidence_age_group_5599)) incidence_age_group_5599[,.(YM,N,Freq)]
)

# Filter out NULL entries (non-existent datasets)
datasets <- Filter(Negate(is.null), datasets)

# Combine the datasets using rbindlist
if (length(datasets) > 0) {
  incidence_age_groups_combined <- rbindlist(datasets)
  
  # sum 'N' by 'YM' across age groups
  incidence_age_group_sums<-incidence_age_groups_combined[,.(age_group_N_sum=sum(N,na.rm=TRUE),Freq=unique(Freq)), by = YM]
  # merge the summed age group data with incidence_all by 'YM'
  incidence_age_group_merged<-merge(incidence_all[,.(YM,all_N=N)],incidence_age_group_sums,by="YM",all.x=TRUE)
  #check
  incidence.agegroup.num.total_vs_incidence.num<-nrow(incidence_age_group_merged[age_group_N_sum!=all_N,])
  incidence.agegroup.num.total_vs_incidence.agegroup.denom<-nrow(incidence_age_group_merged[age_group_N_sum!=Freq,])
  incidence.num_vs_incidence.agegroup.denom<-nrow(incidence_age_group_merged[all_N!=Freq,])
}
# 4. Sum of all incidence indication numerators == Incidence indication denominator == RAM Incidence Numerator 
# combine indication data into a single table
datasets <- list(
  if (exists("incidence_indication_acne") && is.data.table(incidence_indication_acne)) incidence_indication_acne[,.(YM,N,Freq)],
  if (exists("incidence_indication_derm") && is.data.table(incidence_indication_derm)) incidence_indication_derm[,.(YM,N,Freq)],
  if (exists("incidence_indication_psor") && is.data.table(incidence_indication_psor)) incidence_indication_psor[,.(YM,N,Freq)]
)

# Filter out NULL entries (non-existent datasets)
datasets <- Filter(Negate(is.null), datasets)

# Combine the datasets using rbindlist
if (length(datasets) > 0) {
  incidence_indication_combined <- rbindlist(datasets)
  
  # sum 'N' by 'YM' across indications
  incidence_indication_sums<-incidence_indication_combined[,.(indication_N_sum=sum(N,na.rm=TRUE),Freq=unique(Freq)),by=YM]
  # merge the summed indication data with incidence_all by 'YM'
  incidence_indication_merged<-merge(incidence_all[,.(YM,all_N=N)],incidence_indication_sums,by="YM",all.x=TRUE)
  #check
  incidence.indication.num.total_vs_incidence.num<-nrow(incidence_indication_merged[indication_N_sum!=all_N,])
  incidence.indication.num.total_vs_incidence.indication.denom<-nrow(incidence_indication_merged[indication_N_sum!=Freq,])
  incidence.num_vs_incidence.indication.denom<-nrow(incidence_indication_merged[all_N!=Freq,])
}

# 5. Sum of all prevalence age group numerators == Prevalence age group denominator == RAM Prevalence Numerator 
# Combine age groups data into a single table 
datasets <- list(
  if (exists("prevalence_age_group_2099") && is.data.table(prevalence_age_group_2099)) prevalence_age_group_2099[,.(YM,N,Freq)],
  if (exists("prevalence_age_group_3099") && is.data.table(prevalence_age_group_3099)) prevalence_age_group_3099[,.(YM,N,Freq)],
  if (exists("prevalence_age_group_4099") && is.data.table(prevalence_age_group_4099)) prevalence_age_group_4099[,.(YM,N,Freq)],
  if (exists("prevalence_age_group_5599") && is.data.table(prevalence_age_group_5599)) prevalence_age_group_5599[,.(YM,N,Freq)]
)

# Filter out NULL entries (non-existent datasets)
datasets <- Filter(Negate(is.null), datasets)

# Combine the datasets using rbindlist
if (length(datasets) > 0) {
  prevalence_age_groups_combined <- rbindlist(datasets)
  
  
  # sum 'N' by 'YM' across age groups
  prevalence_age_group_sums<-prevalence_age_groups_combined[,.(age_group_N_sum=sum(N,na.rm=TRUE),Freq=unique(Freq)), by = YM]
  # merge the summed age group data with prevalence_all by 'YM'
  prevalence_age_group_merged<-merge(prevalence_all[,.(YM,all_N=N)],prevalence_age_group_sums,by="YM",all.x=TRUE)
  #check
  prevalence.agegroup.num.total_notequal_prevalence.num<-nrow(prevalence_age_group_merged[age_group_N_sum!=all_N,])
  prevalence.agegroup.num.total_notequal_prevalence.agegroup.denom<-nrow(prevalence_age_group_merged[age_group_N_sum!=Freq,])
  prevalence.num_notequal_prevalence.agegroup.denom<-nrow(prevalence_age_group_merged[all_N!=Freq,])
}

# 6. Sum of all prevalence indication numerators == Prevalence indication denominator == RAM Prevalence Numerator 
# combine indication  data into a single table 
datasets <- list(
  if (exists("prevalence_indication_acne") && is.data.table(prevalence_indication_acne)) prevalence_indication_acne[,.(YM,N,Freq)],
  if (exists("prevalence_indication_derm") && is.data.table(prevalence_indication_derm)) prevalence_indication_derm[,.(YM,N,Freq)],
  if (exists("prevalence_indication_psor") && is.data.table(prevalence_indication_psor)) prevalence_indication_psor[,.(YM,N,Freq)]
)

# Filter out NULL entries (non-existent datasets)
datasets <- Filter(Negate(is.null), datasets)

# Combine the datasets using rbindlist
if (length(datasets) > 0) {
  prevalence_indication_combined <- rbindlist(datasets)
  
  # sum 'N' by 'YM' across indications
  prevalence_indication_sums<-prevalence_indication_combined[,.(indication_N_sum=sum(N,na.rm=TRUE),Freq=unique(Freq)),by=YM]
  # merge the summed indication data with prevalence_all by 'YM'
  prevalence_indication_merged<-merge(prevalence_all[,.(YM,all_N=N)],prevalence_indication_sums,by="YM",all.x=TRUE)
  #check
  prevalence.indication.num.total_notequal_prevalence.num<-nrow(prevalence_indication_merged[indication_N_sum!=all_N,])
  prevalence.indication.num.total_notequal_prevalence.indication.denom<-nrow(prevalence_indication_merged[indication_N_sum!=Freq,])
  prevalence.num_notequal_prevalence.indication.denom<-nrow(prevalence_indication_merged[all_N!=Freq,])
}
### FLOWCHART ###
# Define the names vector
names <- c(
  # 1. Incidence Numerator <= Prevalence Denominator 
  "incidence.num_morethan_prevalence.num",
  # 2. Incidence Denominator == Prevalence Denominator 
  "incidence.denom_notequal_prevalence.denom",
  # 3. Sum of all incidence age group numerators == Incidence age group denominator == RAM Incidence Numerator 
  "incidence.agegroup.num.total_vs_incidence.num",
  "incidence.agegroup.num.total_vs_incidence.agegroup.denom",
  "incidence.num_vs_incidence.agegroup.denom",
  # 4. Sum of all incidence indication numerators == Incidence indication denominator == RAM Incidence Numerator 
  "incidence.indication.num.total_vs_incidence.num",
  "incidence.indication.num.total_vs_incidence.indication.denom",
  "incidence.num_vs_incidence.indication.denom",
  # 5. Sum of all prevalence age group numerators == Prevalence age group denominator == RAM Prevalence Numerator 
  "prevalence.agegroup.num.total_notequal_prevalence.num",
  "prevalence.agegroup.num.total_notequal_prevalence.agegroup.denom",
  "prevalence.num_notequal_prevalence.agegroup.denom",
  # 6. Sum of all prevalence indication numerators == Prevalence indication denominator == RAM Prevalence Numerator 
  "prevalence.indication.num.total_notequal_prevalence.num",
  "prevalence.indication.num.total_notequal_prevalence.indication.denom",
  "prevalence.num_notequal_prevalence.indication.denom"
)

# Define the corresponding variable names in the global environment
values <- c(
  # 1. Incidence Numerator <= Prevalence Denominator 
  "incidence.num_morethan_prevalence.num",
  # 2. Incidence Denominator == Prevalence Denominator 
  "incidence.denom_notequal_prevalence.denom",
  # 3. Sum of all incidence age group numerators == Incidence age group denominator == RAM Incidence Numerator 
  "incidence.agegroup.num.total_vs_incidence.num",
  "incidence.agegroup.num.total_vs_incidence.agegroup.denom",
  "incidence.num_vs_incidence.agegroup.denom",
  # 4. Sum of all incidence indication numerators == Incidence indication denominator == RAM Incidence Numerator 
  "incidence.indication.num.total_vs_incidence.num",
  "incidence.indication.num.total_vs_incidence.indication.denom",
  "incidence.num_vs_incidence.indication.denom",
  # 5. Sum of all prevalence age group numerators == Prevalence age group denominator == RAM Prevalence Numerator 
  "prevalence.agegroup.num.total_notequal_prevalence.num",
  "prevalence.agegroup.num.total_notequal_prevalence.agegroup.denom",
  "prevalence.num_notequal_prevalence.agegroup.denom",
  # 6. Sum of all prevalence indication numerators == Prevalence indication denominator == RAM Prevalence Numerator 
  "prevalence.indication.num.total_notequal_prevalence.num",
  "prevalence.indication.num.total_notequal_prevalence.indication.denom",
  "prevalence.num_notequal_prevalence.indication.denom"
)

# Initialize empty vectors to store valid names, values, and skipped variables
valid_names <- c()
valid_values <- c()
skipped_vars <- c()

# Loop through each value and its corresponding name
for (i in seq_along(values)) {
  var_value <- values[i]  # The variable name as it should exist in the environment
  var_name <- names[i]    # The human-readable name for the flowchart
  
  # Check if the variable exists in the global environment
  if (exists(var_value, envir = .GlobalEnv)) {
    # Get the value of the variable from the environment
    value <- get(var_value, envir = .GlobalEnv)
    
    # Store valid names and values
    valid_names <- c(valid_names, var_name)
    valid_values <- c(valid_values, value)
  } else {
    # If variable is missing, store the skipped variable's name
    skipped_vars <- c(skipped_vars, var_name)
  }
}

# Create a data.table with valid names and values
flowchart_objective1 <- data.table(names = valid_names, values = valid_values)

# Print the list of skipped variables
if (length(skipped_vars) > 0) {
  message("Skipped variables:")
  print(skipped_vars)
} else {
  message("No variables were skipped.")
}

View(flowchart_objective1)
# saveRDS(flowchart_objective1, paste0(g_output_dir,"/medicines_counts/flowchart_objective1.rds")) 


###############################################################################
################################# OBJECTIVE 2 #################################
###############################################################################

################################## DISCONTINUED ##################################
# Numerator   = number of subjects discontinuing RAM per calendar month
# Denominator = number of all RAM users during the same month - RAM prevalence 
# Stratified by age group and indication 

################################## SWITCHED  #################################
# Numerator = number of RAM incident users who switched from an oral retinoid 
# Denominator1 = number of all RAM users during the same month - retinoid prevalence 
# Denominator2 = number of WOCBP who ever discontinued a retinoid the actual month - retinoid discontinued
# Stratified by age group and indication 

### TESTS ###
# 1. Discontinued denominator == RAM prevalence numerator
# 2. Sum of all discontinued age group numerators == Discontinued age group denominator == RAM discontinued Numerator 
# 3. Sum of all discontinued indication numerators == Discontinued indication denominator == RAM discontinued Numerator 
# 4. Switcher 1 numerator == Switcher 2 numerator 
# 5. Switcher 1 denominator == retinoid prevalence numerator
# 6. Switcher 2 denominator == retinoid discontinued numerator
# 7. Sum of all switcher age group numerators == Switcher age group denominator == RAM switcher Numerator 
# 8. Sum of all switcher indication numerators == Switcher indication denominator == RAM switchter Numerator 

###############################################################################
###############################################################################
###############################################################################
# 1. Discontinued denominator == RAM prevalence numerator
discontinued_prevalence_merged<-merge(discontinued_all[,.(YM,N,Freq)],prevalence_all[,.(YM,N,Freq)],by="YM",suffixes=c("_discontinued","_prevalence"))
#check
discontinued.denom_notequal_prevalence.num<-nrow(discontinued_prevalence_merged[Freq_discontinued!=N_prevalence,])

# 2. Sum of all discontinued age group numerators == Discontinued age group denominator == RAM discontinued Numerator 
# combine age groups files into a single table 
datasets <- list(
  if (exists("discontinued_age_group_2099") && is.data.table(discontinued_age_group_2099)) discontinued_age_group_2099[,.(YM,N,Freq)],
  if (exists("discontinued_age_group_3099") && is.data.table(discontinued_age_group_3099)) discontinued_age_group_3099[,.(YM,N,Freq)],
  if (exists("discontinued_age_group_4099") && is.data.table(discontinued_age_group_4099)) discontinued_age_group_4099[,.(YM,N,Freq)],
  if (exists("discontinued_age_group_5599") && is.data.table(discontinued_age_group_5599)) discontinued_age_group_5599[,.(YM,N,Freq)]
)

# Filter out NULL entries (non-existent datasets)
datasets <- Filter(Negate(is.null), datasets)

# Combine the datasets using rbindlist
if (length(datasets) > 0) {
  discontinued_age_groups_combined <- rbindlist(datasets)
  
  # sum 'N' by 'YM' across age groups
  discontinued_age_group_sums<-discontinued_age_groups_combined[,.(age_group_N_sum=sum(N,na.rm=TRUE),Freq=unique(Freq)), by = YM]
  # merge the summed age group data with discontinued_all by 'YM'
  discontinued_age_group_merged<-merge(discontinued_all[,.(YM,all_N=N)],discontinued_age_group_sums,by="YM",all.x=TRUE)
  #check
  discontinued.agegroup.num.total_vs_discontinued.num<-nrow(discontinued_age_group_merged[age_group_N_sum!=all_N,])
  discontinued.agegroup.num.total_vs_discontinued.agegroup.denom<-nrow(discontinued_age_group_merged[age_group_N_sum!=Freq,])
  discontinued.num_vs_discontinued.agegroup.denom<-nrow(discontinued_age_group_merged[all_N!=Freq,])
}

# 3. Sum of all discontinued indication numerators == Discontinued indication denominator == RAM discontinued Numerator 
# combine indication files into a single table 
datasets <- list(
  if (exists("discontinued_indication_acne") && is.data.table(discontinued_indication_acne)) discontinued_indication_acne[,.(YM,N,Freq)],
  if (exists("discontinued_indication_derm") && is.data.table(discontinued_indication_derm)) discontinued_indication_derm[,.(YM,N,Freq)],
  if (exists("discontinued_indication_psor") && is.data.table(discontinued_indication_psor)) discontinued_indication_psor[,.(YM,N,Freq)]
)

# Filter out NULL entries (non-existent datasets)
datasets <- Filter(Negate(is.null), datasets)

# Combine the datasets using rbindlist
if (length(datasets) > 0) {
  discontinued_indication_combined <- rbindlist(datasets)
  
  # sum 'N' by 'YM' across indications
  discontinued_indication_sums<-discontinued_indication_combined[,.(indication_N_sum=sum(N,na.rm=TRUE),Freq=unique(Freq)),by=YM]
  # merge the summed indication data with discontinued_all by 'YM'
  discontinued_indication_merged<-merge(discontinued_all[,.(YM,all_N=N)],discontinued_indication_sums,by="YM",all.x=TRUE)
  #check
  discontinued.indication.num.total_vs_discontinued.num<-nrow(discontinued_indication_merged[indication_N_sum!=all_N,])
  discontinued.indication.num.total_vs_discontinued.indication.denom<-nrow(discontinued_indication_merged[indication_N_sum!=Freq,])
  discontinued.num_vs_discontinued.indication.denom<-nrow(discontinued_indication_merged[all_N!=Freq,])
}

# 4. Switcher 1 numerator == Switcher 2 numerator 
switcher1_switcher2_merged<-merge(switcher_1[,.(YM,N,Freq)],switcher_2[,.(YM,N,Freq)],by="YM",suffixes=c("_switcher1","_switcher2"))
#check
switcher1.num_notequal_switcher2.num<-nrow(switcher1_switcher2_merged[N_switcher1!=N_switcher2,])

# 5. Switcher 1 denominator == retinoid prevalence numerator
switcher1_retinoid_prevalent_merged<-merge(switcher_1[,.(YM,Freq)],retinoid_prevalence[,.(YM,N)],by="YM")
#check
switcher1.denom_notequal_retinoid.prevalence.num<-nrow(switcher1_retinoid_prevalent_merged[Freq!=N,])

# 6. Switcher 2 denominator == retinoid discontinued numerator
switcher2_retinoid_discontined_merged<-merge(switcher_2[,.(YM,Freq)],retinoid_discontinued[,.(YM,N)],by="YM")
#check
switcher2.denom_notequal_retinoid.discontinued.num<-nrow(switcher1_retinoid_prevalent_merged[Freq!=N,])

# 7. Sum of all switcher age group numerators == Switcher age group denominator == RAM switcher Numerator 
# combine age groups files into a single table 
datasets <- list(
  if (exists("switcher_age_group_2099") && is.data.table(switcher_age_group_2099)) switcher_age_group_2099[,.(YM,N,Freq)],
  if (exists("switcher_age_group_3099") && is.data.table(switcher_age_group_3099)) switcher_age_group_3099[,.(YM,N,Freq)],
  if (exists("switcher_age_group_4099") && is.data.table(switcher_age_group_4099)) switcher_age_group_4099[,.(YM,N,Freq)],
  if (exists("switcher_age_group_5599") && is.data.table(switcher_age_group_5599)) switcher_age_group_5599[,.(YM,N,Freq)]
)

# Filter out NULL entries (non-existent datasets)
datasets <- Filter(Negate(is.null), datasets)

# Combine the datasets using rbindlist
if (length(datasets) > 0) {
  switcher_age_groups_combined <- rbindlist(datasets)
  
  # sum 'N' by 'YM' across age groups
  switcher_age_group_sums<-switcher_age_groups_combined[,.(age_group_N_sum=sum(N,na.rm=TRUE),Freq=unique(Freq)), by = YM]
  # merge the summed age group data with switcher_all by 'YM'
  switcher1_age_group_merged<-merge(switcher_1[,.(YM,all_N=N)],switcher_age_group_sums,by="YM",all.x=TRUE)
  switcher2_age_group_merged<-merge(switcher_2[,.(YM,all_N=N)],switcher_age_group_sums,by="YM",all.x=TRUE)
  #check
  switcher1.agegroup.num.total_vs_switcher1.num<-nrow(switcher1_age_group_merged[age_group_N_sum!=all_N,])
  switcher1.agegroup.num.total_vs_switcher1.agegroup.denom<-nrow(switcher1_age_group_merged[age_group_N_sum!=Freq,])
  switcher1.num_vs_switcher1.agegroup.denom<-nrow(switcher1_age_group_merged[all_N!=Freq,])
  switcher2.agegroup.num.total_vs_switcher2.num<-nrow(switcher2_age_group_merged[age_group_N_sum!=all_N,])
  switcher2.agegroup.num.total_vs_switcher2.agegroup.denom<-nrow(switcher2_age_group_merged[age_group_N_sum!=Freq,])
  switcher2.num_vs_switcher2.agegroup.denom<-nrow(switcher2_age_group_merged[all_N!=Freq,])
}

# 8. Sum of all switcher indication numerators == Switcher indication denominator == RAM switchter Numerator 
# combine indication files into a single table and sum 'N' by 'YM'
datasets <- list(
  if (exists("switcher_indication_acne") && is.data.table(switcher_indication_acne)) switcher_indication_acne[,.(YM,N,Freq)],
  if (exists("switcher_indication_derm") && is.data.table(switcher_indication_derm)) switcher_indication_derm[,.(YM,N,Freq)],
  if (exists("switcher_indication_psor") && is.data.table(switcher_indication_psor)) switcher_indication_psor[,.(YM,N,Freq)]
)

# Filter out NULL entries (non-existent datasets)
datasets <- Filter(Negate(is.null), datasets)

# Combine the datasets using rbindlist
if (length(datasets) > 0) {
  switcher_indication_combined <- rbindlist(datasets)
  
  # sum 'N' by 'YM' across age groups
  switcher_indication_sums<-switcher_indication_combined[,.(indication_N_sum=sum(N,na.rm=TRUE),Freq=unique(Freq)),by=YM]
  # merge the summed age group data with switcher_all by 'YM'
  switcher1_indication_merged<-merge(switcher_1[,.(YM,all_N=N)],switcher_indication_sums,by="YM",all.x=TRUE)
  switcher2_indication_merged<-merge(switcher_2[,.(YM,all_N=N)],switcher_indication_sums,by="YM",all.x=TRUE)
  #check
  switcher1.indication.num.total_vs_switcher1.num<-nrow(switcher1_indication_merged[indication_N_sum!=all_N,])
  switcher1.indication.num.total_vs_switcher1.indication.denom<-nrow(switcher1_indication_merged[indication_N_sum!=Freq,])
  switcher1.num_vs_switcher1.indication.denom<-nrow(switcher1_indication_merged[all_N!=Freq,])
  switcher2.indication.num.total_vs_switcher2.num<-nrow(switcher2_indication_merged[indication_N_sum!=all_N,])
  switcher2.indication.num.total_vs_switcher2.indication.denom<-nrow(switcher2_indication_merged[indication_N_sum!=Freq,])
  switcher2.num_vs_switcher2.indication.denom<-nrow(switcher2_indication_merged[all_N!=Freq,])
}
### FLOWCHART ###
# Define the names vector
names <- c(
  # 1. Discontinued denominator == RAM prevalence numerator
  "discontinued.denom_notequal_prevalence.num",
  # 2. Sum of all discontinued age group numerators == Discontinued age group denominator == RAM discontinued Numerator 
  "discontinued.agegroup.num.total_vs_discontinued.num",
  "discontinued.agegroup.num.total_vs_discontinued.agegroup.denom",
  "discontinued.num_vs_discontinued.agegroup.denom",
  # 3. Sum of all discontinued indication numerators == Discontinued indication denominator == RAM discontinued Numerator 
  "discontinued.indication.num.total_vs_discontinued.num",
  "discontinued.indication.num.total_vs_discontinued.indication.denom",
  "discontinued.num_vs_discontinued.indication.denom",
  # 4. Switcher 1 numerator == Switcher 2 numerator 
  "switcher1.num_notequal_switcher2.num",
  # 5. Switcher 1 denominator == retinoid prevalence numerator
  "switcher1.denom_notequal_retinoid.prevalence.num",
  # 6. Switcher 2 denominator == retinoid discontinued numerator
  "switcher2.denom_notequal_retinoid.discontinued.num",
  # 7. Sum of all switcher age group numerators == Switcher age group denominator == RAM switcher Numerator 
  "switcher1.agegroup.num.total_vs_switcher1.num",
  "switcher1.agegroup.num.total_vs_switcher1.agegroup.denom",
  "switcher1.num_vs_switcher1.agegroup.denom",
  "switcher2.agegroup.num.total_vs_switcher2.num",
  "switcher2.agegroup.num.total_vs_switcher2.agegroup.denom",
  "switcher2.num_vs_switcher2.agegroup.denom",
  # 8. Sum of all switcher indication numerators == Switcher indication denominator == RAM switcher Numerator 
  "switcher1.indication.num.total_vs_switcher1.num",
  "switcher1.indication.num.total_vs_switcher1.indication.denom",
  "switcher1.num_vs_switcher1.indication.denom",
  "switcher2.indication.num.total_vs_switcher2.num",
  "switcher2.indication.num.total_vs_switcher2.indication.denom",
  "switcher2.num_vs_switcher2.indication.denom"
)

# Define the corresponding variable names in the global environment
values <- c(
  # 1. Discontinued denominator == RAM prevalence numerator
  "discontinued.denom_notequal_prevalence.num",
  # 2. Sum of all discontinued age group numerators == Discontinued age group denominator == RAM discontinued Numerator 
  "discontinued.agegroup.num.total_vs_discontinued.num",
  "discontinued.agegroup.num.total_vs_discontinued.agegroup.denom",
  "discontinued.num_vs_discontinued.agegroup.denom",
  # 3. Sum of all discontinued indication numerators == Discontinued indication denominator == RAM discontinued Numerator 
  "discontinued.indication.num.total_vs_discontinued.num",
  "discontinued.indication.num.total_vs_discontinued.indication.denom",
  "discontinued.num_vs_discontinued.indication.denom",
  # 4. Switcher 1 numerator == Switcher 2 numerator 
  "switcher1.num_notequal_switcher2.num",
  # 5. Switcher 1 denominator == retinoid prevalence numerator
  "switcher1.denom_notequal_retinoid.prevalence.num",
  # 6. Switcher 2 denominator == retinoid discontinued numerator
  "switcher2.denom_notequal_retinoid.discontinued.num",
  # 7. Sum of all switcher age group numerators == Switcher age group denominator == RAM switcher Numerator 
  "switcher1.agegroup.num.total_vs_switcher1.num",
  "switcher1.agegroup.num.total_vs_switcher1.agegroup.denom",
  "switcher1.num_vs_switcher1.agegroup.denom",
  "switcher2.agegroup.num.total_vs_switcher2.num",
  "switcher2.agegroup.num.total_vs_switcher2.agegroup.denom",
  "switcher2.num_vs_switcher2.agegroup.denom",
  # 8. Sum of all switcher indication numerators == Switcher indication denominator == RAM switcher Numerator 
  "switcher1.indication.num.total_vs_switcher1.num",
  "switcher1.indication.num.total_vs_switcher1.indication.denom",
  "switcher1.num_vs_switcher1.indication.denom",
  "switcher2.indication.num.total_vs_switcher2.num",
  "switcher2.indication.num.total_vs_switcher2.indication.denom",
  "switcher2.num_vs_switcher2.indication.denom"
)

# Initialize empty vectors to store valid names, values, and skipped variables
valid_names <- c()
valid_values <- c()
skipped_vars <- c()

# Loop through each value and its corresponding name
for (i in seq_along(values)) {
  var_value <- values[i]  # The variable name as it should exist in the environment
  var_name <- names[i]    # The human-readable name for the flowchart
  
  # Check if the variable exists in the global environment
  if (exists(var_value, envir = .GlobalEnv)) {
    # Get the value of the variable from the environment
    value <- get(var_value, envir = .GlobalEnv)
    
    # Store valid names and values
    valid_names <- c(valid_names, var_name)
    valid_values <- c(valid_values, value)
  } else {
    # If variable is missing, store the skipped variable's name
    skipped_vars <- c(skipped_vars, var_name)
  }
}

# Create a data.table with valid names and values
flowchart_objective2 <- data.table(names = valid_names, values = valid_values)

# Print the list of skipped variables
if (length(skipped_vars) > 0) {
  message("Skipped variables:")
  print(skipped_vars)
} else {
  message("No variables were skipped.")
}

View(flowchart_objective2)
# saveRDS(flowchart_objective2, paste0(g_output_dir,"/medicines_counts/flowchart_objective2.rds")) 


###############################################################################
################################# OBJECTIVE 3 #################################
###############################################################################

################################## GENERAL CONCOMITANCE ############################
# Numerator = number of users of RAM + Retinoid per calendar month 
# Denominator = number of retinoid users the same month - retinoid prevalence 
# Stratified by age group and indication 

################################## CONTRAINDICATED #################################
# Numerator = contraindicated RAM codes within general concomitance 
# Denominator = general concomitance 
# Stratified by indication 
# Records vs Users

### TESTS ###
# 1. General concomitance numerator <= retinoid prevalence numerator
# 2. General concomitance numerator <= RAM prevalence numerator
# 3. General concomitance denominator == retinoid prevalence numerator 
# 4. General concomitance numerator users <= General concomitance numerator records 
# 5. Sum of all general concomitance age group numerators == General concomitance age group denominator == RAM General concomitance Numerator 
# 6. Sum of all general concomitance indication numerators == General concomitance indication denominator == RAM General concomitance Numerator 
# 7. Contraindicated user counts < contraindicated record counts
# 8. Contraindicated numerator <= general concomitance numerator (USERS)
# 9. Contraindicated denominator == general concomitance numerator (USERS)
# 10. Contraindicated numerator <= general concomitance numerator (RECORDS)
# 11. Contraindicated denominator == general concomitance numerator (RECORDS)
# 12. Sum of all Contraindicated indication numerators == Contraindicated indication denominator == Contraindicated counts Numerator 

###############################################################################
###############################################################################
###############################################################################
if (exists("genconcomit_all_users") && is.data.table(genconcomit_all_users)){
  # 1. General concomitance numerator <= retinoid prevalence numerator
  genconcomit_retinoid_prevalence_merged<-merge(genconcomit_all_users[,.(YM,N,Freq)],retinoid_prevalence[,.(YM,N)],by="YM",suffixes=c("_genconcomit","_retinoidprev"))
  #check
  genconcomit.num_morethan_retinoid.prevalence.num<-nrow(genconcomit_retinoid_prevalence_merged[N_genconcomit>N_retinoidprev,])
  
  # 2. General concomitance numerator <= RAM prevalence numerator
  genconcomit_RAM_prevalence_merged<-merge(genconcomit_all_users[,.(YM,N,Freq)],prevalence_all[,.(YM,N)],by="YM",suffixes=c("_genconcomit","_RAMprevalence"))
  #check
  genconcomit.num_morethan_RAM.prevalence.num<-nrow(genconcomit_RAM_prevalence_merged[N_genconcomit>N_RAMprevalence,])
  
  # 3. General concomitance denominator == retinoid prevalence numerator 
  #check
  genconcomit.denom_notequal_retinoid.prevalence.num<-nrow(genconcomit_retinoid_prevalence_merged[Freq!=N_retinoidprev,])
  
  # 4. General concomitance numerator users <= General concomitance numerator records 
  genconcomit.users_vs_genconcomit.records<-merge(genconcomit_all_users[,.(YM,N)],genconcomit_all_records[,.(YM,N)],by="YM",suffixes=c("_users","_records"))
  #check
  genconcomit.users_morethan_genconcomit.records<-nrow(genconcomit.users_vs_genconcomit.records[N_users>N_records,])
  
  # 5. Sum of all general concomitance age group numerators == General concomitance age group denominator == RAM General concomitance Numerator 
  # combine age groups files into a single table 
  datasets <- list(
    if (exists("genconcomit_age_group_2099") && is.data.table(genconcomit_age_group_2099)) genconcomit_age_group_2099[,.(YM,N,Freq)],
    if (exists("genconcomit_age_group_3099") && is.data.table(genconcomit_age_group_3099)) genconcomit_age_group_3099[,.(YM,N,Freq)],
    if (exists("genconcomit_age_group_4099") && is.data.table(genconcomit_age_group_4099)) genconcomit_age_group_4099[,.(YM,N,Freq)],
    if (exists("genconcomit_age_group_5599") && is.data.table(genconcomit_age_group_5599)) genconcomit_age_group_5599[,.(YM,N,Freq)]
  )
  
  # Filter out NULL entries (non-existent datasets)
  datasets <- Filter(Negate(is.null), datasets)
  
  # Combine the datasets using rbindlist
  if (length(datasets) > 0) {
    genconcomit_age_groups_combined <- rbindlist(datasets)
    
    # sum 'N' by 'YM' across age groups
    genconcomit_age_group_sums<-genconcomit_age_groups_combined[,.(age_group_N_sum=sum(N,na.rm=TRUE),Freq=max(Freq)), by = YM]
    
    # merge the summed age group data with genconcomit_all by 'YM'
    genconcomit_age_group_merged<-merge(genconcomit_all_users[,.(YM,all_N=N)],genconcomit_age_group_sums,by="YM",all.x=TRUE)
    #check
    genconcomit.agegroup.num.total_vs_genconcomit.num<-nrow(genconcomit_age_group_merged[age_group_N_sum!=all_N,])
    genconcomit.agegroup.num.total_vs_genconcomit.agegroup.denom<-nrow(genconcomit_age_group_merged[age_group_N_sum!=Freq,])
    genconcomit.num_vs_genconcomit.agegroup.denom<-nrow(genconcomit_age_group_merged[all_N!=Freq,])
  }
  
  # 6. Sum of all general concomitance indication numerators == General concomitance indication denominator == RAM General concomitance Numerator 
  # combine indication files into a single table 
  datasets <- list(
    if (exists("genconcomit_indication_acne") && is.data.table(genconcomit_indication_acne)) genconcomit_indication_acne[,.(YM,N,Freq)],
    if (exists("genconcomit_indication_derm") && is.data.table(genconcomit_indication_derm)) genconcomit_indication_derm[,.(YM,N,Freq)],
    if (exists("genconcomit_indication_psor") && is.data.table(genconcomit_indication_psor)) genconcomit_indication_psor[,.(YM,N,Freq)]
  )
  
  # Filter out NULL entries (non-existent datasets)
  datasets <- Filter(Negate(is.null), datasets)
  
  # Combine the datasets using rbindlist
  if (length(datasets) > 0) {
    genconcomit_indication_combined <- rbindlist(datasets)
    
    # sum 'N' by 'YM' across age groups
    genconcomit_indication_sums<-genconcomit_indication_combined[,.(indication_N_sum=sum(N,na.rm=TRUE),Freq=unique(Freq)),by=YM]
    # merge the summed age group data with genconcomit_all by 'YM'
    genconcomit_indication_merged<-merge(genconcomit_all_users[,.(YM,all_N=N)],genconcomit_indication_sums,by="YM",all.x=TRUE)
    #check
    genconcomit.indication.num.total_vs_genconcomit.num<-nrow(genconcomit_indication_merged[indication_N_sum!=all_N,])
    genconcomit.indication.num.total_vs_genconcomit.indication.denom<-nrow(genconcomit_indication_merged[indication_N_sum!=Freq,])
    genconcomit.num_vs_genconcomit.indication.denom<-nrow(genconcomit_indication_merged[all_N!=Freq,])
  }
  
  # 7. contraindicated user counts <= contraindicated record counts
  contra_users_records_merged<-merge(contra_all_records[,.(YM,N,Freq)],contra_all_users[,.(YM,N,Freq)],by="YM",suffixes=c("_records","_users"))
  #check
  contra.users.num_lessthan_contra.records.num<-nrow(contra_users_records_merged[N_records<N_users,])
  contra.users.denom_lessthan_contra.records.denom<-nrow(contra_users_records_merged[Freq_records<Freq_users,])#general concomitance
  
  # 8. Contraindicated numerator <= general concomitance numerator (USERS)
  contra_users_genconcomit_users_merged<-merge(contra_all_users[,.(YM,N,Freq)],genconcomit_all_users[,.(YM,N,Freq)],by="YM",suffixes=c("_contra","_genconcomit"))
  #check
  contra.users.num_morethan_genconcomitusers.num<-nrow(contra_users_genconcomit_users_merged[N_contra>N_genconcomit,])
  
  # 9. Contraindicated denominator == general concomitance numerator (USERS)
  #check
  contra.users.denom_notequal_genconcomit.users.num<-nrow(contra_users_genconcomit_users_merged[Freq_contra!=N_genconcomit,])
  
  # 10. Contraindicated numerator <= general concomitance numerator (RECORDS)
  contra_records_genconcomit_records_merged<-merge(contra_all_records[,.(YM,N,Freq)],genconcomit_all_records[,.(YM,N)],by="YM",suffixes=c("_contra","_genconcomit"))
  #check
  contra.records.num_morethan_genconcomitrecords.num<-nrow(contra_records_genconcomit_records_merged[N_contra>N_genconcomit,])
  
  # 11. Contraindicated denominator == general concomitance numerator (RECORDS)
  #check
  contra.records.denom_notequal_genconcomit.records.num<-nrow(contra_records_genconcomit_records_merged[Freq!=N_genconcomit,])
  
  # 12. Sum of all Contraindicated indication numerators == Contraindicated indication denominator == Contraindicated counts Numerator 
  # combine indication files into a single table 
  datasets <- list(
    if (exists("contra_indication_acne") && is.data.table(contra_indication_acne)) contra_indication_acne[,.(YM,N,Freq)],
    if (exists("contra_indication_derm") && is.data.table(contra_indication_derm)) contra_indication_derm[,.(YM,N,Freq)],
    if (exists("contra_indication_psor") && is.data.table(contra_indication_psor)) contra_indication_psor[,.(YM,N,Freq)]
  )
  
  # Filter out NULL entries (non-existent datasets)
  datasets <- Filter(Negate(is.null), datasets)
  
  # Combine the datasets using rbindlist
  if (length(datasets) > 0) {
    
    contra_indication_combined <- rbindlist(datasets)
    # sum 'N' by 'YM' across indications
    contra_indication_sums<-contra_indication_combined[,.(indication_N_sum=sum(N,na.rm=TRUE),Freq=unique(Freq)),by=YM]
    # merge the summed indications data with contra_all by 'YM'
    contra_indication_merged<-merge(contra_all_users[,.(YM,all_N=N)],contra_indication_sums,by="YM",all.x=TRUE)
    #check
    contra.indication.num.total_vs_contra.num<-nrow(contra_indication_merged[indication_N_sum!=all_N,])
    contra.indication.num.total_vs_contra.indication.denom<-nrow(contra_indication_merged[indication_N_sum!=Freq,])
    contra.num_vs_contra.indication.denom<-nrow(contra_indication_merged[all_N!=Freq,])
  }
  
  ### FLOWCHART ###
  names<-c(
    # 1. General concomitance numerator <= retinoid prevalence numerator
    "genconcomit.num_morethan_retinoid.prevalence.num",
    # 2. General concomitance numerator <= RAM prevalence numerator
    "genconcomit.num_morethan_RAM.prevalence.num",
    # 3. General concomitance denominator == retinoid prevalence numerator
    "genconcomit.denom_notequal_retinoid.prevalence.num",
    # 4. General concomitance numerator users <= General concomitance numerator records
    "genconcomit.users_morethan_genconcomit.records",
    # 5. Sum of all general concomitance age group numerators == General concomitance age group denominator == RAM General concomitance Numerator
    "genconcomit.agegroup.num.total_vs_genconcomit.num",
    "genconcomit.agegroup.num.total_vs_genconcomit.agegroup.denom",
    "genconcomit.num_vs_genconcomit.agegroup.denom",
    # 6. Sum of all general concomitance indication numerators == General concomitance indication denominator == RAM General concomitance Numerator
    "genconcomit.indication.num.total_vs_genconcomit.num",
    "genconcomit.indication.num.total_vs_genconcomit.indication.denom",
    "genconcomit.num_vs_genconcomit.indication.denom",
    # 7. Contraindicated user counts < contraindicated record counts
    "contra.users.num_lessthan_contra.records.num",
    "contra.users.denom_lessthan_contra.records.denom",
    # 8. Contraindicated numerator <= general concomitance numerator (USERS)
    "contra.users.num_morethan_genconcomitusers.num",
    # 9. Contraindicated denominator == general concomitance numerator (USERS)
    "contra.users.denom_notequal_genconcomit.users.num",
    # 10. Contraindicated numerator <= general concomitance numerator (RECORDS)
    "contra_records_genconcomit_records_merged",
    # 11. Contraindicated denominator == general concomitance numerator (RECORDS)
    "contra.records.denom_notequal_genconcomit.records.num",
    # 12. Sum of all Contraindicated indication numerators == Contraindicated indication denominator == Contraindicated counts Numerator
    "contra.indication.num.total_vs_contra.num",
    "contra.indication.num.total_vs_contra.indication.denom",
    "contra.num_vs_contra.indication.denom"
  )
  
  # The actual variable names that should exist in the environment
  values <- c(
    # 1. General concomitance numerator <= retinoid prevalence numerator
    "genconcomit.num_morethan_retinoid.prevalence.num",
    # 2. General concomitance numerator <= RAM prevalence numerator
    "genconcomit.num_morethan_RAM.prevalence.num",
    # 3. General concomitance denominator == retinoid prevalence numerator
    "genconcomit.denom_notequal_retinoid.prevalence.num",
    # 4. General concomitance numerator users <= General concomitance numerator records
    "genconcomit.users_morethan_genconcomit.records",
    # 5. Sum of all general concomitance age group numerators == General concomitance age group denominator == RAM General concomitance Numerator
    "genconcomit.agegroup.num.total_vs_genconcomit.num",
    "genconcomit.agegroup.num.total_vs_genconcomit.agegroup.denom",
    "genconcomit.num_vs_genconcomit.agegroup.denom",
    # 6. Sum of all general concomitance indication numerators == General concomitance indication denominator == RAM General concomitance Numerator
    "genconcomit.indication.num.total_vs_genconcomit.num",
    "genconcomit.indication.num.total_vs_genconcomit.indication.denom",
    "genconcomit.num_vs_genconcomit.indication.denom",
    # 7. Contraindicated user counts < contraindicated record counts
    "contra.users.num_lessthan_contra.records.num",
    "contra.users.denom_lessthan_contra.records.denom",
    # 8. Contraindicated numerator <= general concomitance numerator (USERS)
    "contra.users.num_morethan_genconcomitusers.num",
    # 9. Contraindicated denominator == general concomitance numerator (USERS)
    "contra.users.denom_notequal_genconcomit.users.num",
    # 10. Contraindicated numerator <= general concomitance numerator (RECORDS)
    "contra.records.num_morethan_genconcomitrecords.num",    
    # 11. Contraindicated denominator == general concomitance numerator (RECORDS)
    "contra.records.denom_notequal_genconcomit.records.num",
    # 12. Sum of all Contraindicated indication numerators == Contraindicated indication denominator == Contraindicated counts Numerator
    "contra.indication.num.total_vs_contra.num",
    "contra.indication.num.total_vs_contra.indication.denom",
    "contra.num_vs_contra.indication.denom"
  )
  
  # Create empty vectors to store valid names, valid values, and skipped variables
  valid_names <- c()
  valid_values <- c()
  skipped_vars <- c()
  
  # Loop through each value and its corresponding name
  for (i in seq_along(values)) {
    var_value <- values[i] # The actual variable name in the environment
    var_name <- names[i]   # The descriptive name you want to use
    
    # Check if the variable exists in the global environment
    if (exists(var_value, envir = .GlobalEnv)) {
      # Retrieve the value of the variable from the environment
      value <- get(var_value, envir = .GlobalEnv)
      
      # Add the valid name and value to the lists
      valid_names <- c(valid_names, var_name)
      valid_values <- c(valid_values, value)
    } else {
      # If the variable is missing, add its name to the skipped_vars list
      skipped_vars <- c(skipped_vars, var_name)
    }
  }
  
  # Create the data.table with valid names and values
  flowchart_objective3 <- data.table(names = valid_names, values = valid_values)
  
  # Print the resulting data.table
  View(flowchart_objective3)
  # saveRDS(flowchart_objective3, paste0(g_output_dir,"/medicines_counts/flowchart_objective3.rds")) 
  
  # Print the list of skipped variables
  if (length(skipped_vars) > 0) {
    message("Skipped variables:")
    print(skipped_vars)
  } else {
    message("No variables were skipped.")
  }
} else {
  print("There is no General Concomitance Data")
}


###############################################################################
################################# OBJECTIVE 4 #################################
###############################################################################

################################## TERATOGENIC USERS ##################################
# Numerator   = Number of teratogenic ATC users per month-year 
# Denominator = RAM prevalence 
# Stratified by indication 

################################## TERATOGENIC RECORDS  #################################
# Numerator   = Number of teratogenic ATC records per month-year 
# Denominator = All RAM prescriptions in the retinoid subpopulation

### TESTS ###
# 1. Teratogenic user numerator < Teratogenic user denominator
# 2. Teratogenic user denominator == RAM prevalence numerator 
# 3. Sum of all teratogenic indication numerators == Teratogenic indication denominator == RAM Teratogenic Numerator 


###############################################################################
###############################################################################
###############################################################################
if (exists("teratogenic_all_users") && is.data.table(teratogenic_all_users)){
  #  1. Teratogenic user numerator < Teratogenic user denominator
  teratogenic.num_morethan_teratogenic.denom<-nrow(teratogenic_all_users[N>Freq,])
  
  # 2. Teratogenic user denominator == RAM prevalence numerator 
  teratogenic_RAM_prevalence_merged<-merge(teratogenic_all_users[,.(YM,Freq)],prevalence_all[,.(YM,N)])
  #check
  teratogenic.user.denom_notequal_RAM_prevalence.num<-nrow(teratogenic_RAM_prevalence_merged[N!=Freq,])
  
  # 3. Sum of all teratogenic indication numerators == Teratogenic indication denominator == RAM Teratogenic Numerator 
  # combine indication files into a single table 
  datasets <- list(
    if (exists("teratogenic_indication_acne") && is.data.table(teratogenic_indication_acne)) teratogenic_indication_acne[,.(YM,N,Freq)],
    if (exists("teratogenic_indication_derm") && is.data.table(teratogenic_indication_derm)) teratogenic_indication_derm[,.(YM,N,Freq)],
    if (exists("teratogenic_indication_psor") && is.data.table(teratogenic_indication_psor)) teratogenic_indication_psor[,.(YM,N,Freq)]
  )
  
  # Filter out NULL entries (non-existent datasets)
  datasets <- Filter(Negate(is.null), datasets)
  
  # Combine the datasets using rbindlist
  if (length(datasets) > 0) {
    teratogenic_indication_combined <- rbindlist(datasets)
    
    # sum 'N' by 'YM' across indications
    teratogenic_indication_sums<-teratogenic_indication_combined[,.(indication_N_sum=sum(N,na.rm=TRUE),Freq=unique(Freq)),by=YM]
    # merge the summed age group data with teratogenic_all by 'YM'
    teratogenic_indication_merged<-merge(teratogenic_all_users[,.(YM,all_N=N)],teratogenic_indication_sums,by="YM",all.x=TRUE)
    #check
    teratogenic.indication.num.total_vs_teratogenic.num<-nrow(teratogenic_indication_merged[indication_N_sum!=all_N,])
    teratogenic.indication.num.total_vs_teratogenic.indication.denom<-nrow(teratogenic_indication_merged[indication_N_sum!=Freq,])
    teratogenic.num_vs_teratogenic.indication.denom<-nrow(teratogenic_indication_merged[all_N!=Freq,])
  }
  
  ### FLOWCHART ###
  # Define the names vector
  names <- c(
    #  1. Teratogenic user numerator < Teratogenic user denominator
    "teratogenic.num_morethan_teratogenic.denom",
    # 2. Teratogenic user denominator == RAM prevalence numerator
    "teratogenic.user.denom_notequal_RAM_prevalence.num",
    # 3. Sum of all teratogenic indication numerators == Teratogenic indication denominator == RAM Teratogenic Numerator
    "teratogenic.indication.num.total_vs_teratogenic.num",
    "teratogenic.indication.num.total_vs_teratogenic.indication.denom",
    "teratogenic.num_vs_teratogenic.indication.denom"
  )
  
  # Define the corresponding variable names in the global environment
  values <- c(
    #  1. Teratogenic user numerator < Teratogenic user denominator
    "teratogenic.num_morethan_teratogenic.denom",
    # 2. Teratogenic user denominator == RAM prevalence numerator
    "teratogenic.user.denom_notequal_RAM_prevalence.num",
    # 3. Sum of all teratogenic indication numerators == Teratogenic indication denominator == RAM Teratogenic Numerator
    "teratogenic.indication.num.total_vs_teratogenic.num",
    "teratogenic.indication.num.total_vs_teratogenic.indication.denom",
    "teratogenic.num_vs_teratogenic.indication.denom"
  )
  
  # Initialize empty vectors to store valid names, values, and skipped variables
  valid_names <- c()
  valid_values <- c()
  skipped_vars <- c()
  
  # Loop through each value and its corresponding name
  for (i in seq_along(values)) {
    var_value <- values[i]  # The variable name as it should exist in the environment
    var_name <- names[i]    # The human-readable name for the flowchart
    
    # Check if the variable exists in the global environment
    if (exists(var_value, envir = .GlobalEnv)) {
      # Get the value of the variable from the environment
      value <- get(var_value, envir = .GlobalEnv)
      
      # Store valid names and values
      valid_names <- c(valid_names, var_name)
      valid_values <- c(valid_values, value)
    } else {
      # If variable is missing, store the skipped variable's name
      skipped_vars <- c(skipped_vars, var_name)
    }
  }
  
  # Create a data.table with valid names and values
  flowchart_objective4 <- data.table(names = valid_names, values = valid_values)
  
  # Print the list of skipped variables
  if (length(skipped_vars) > 0) {
    message("Skipped variables:")
    print(skipped_vars)
  } else {
    message("No variables were skipped.")
  }
  # saveRDS(flowchart_objective4, paste0(g_output_dir,"/medicines_counts/flowchart_objective4.rds")) 
} else {
  print("There is no teratogenic data")
}

# Print the resulting data.table
View(flowchart_objective4)

###############################################################################
################################ RAW RAM COUNTS ###############################
###############################################################################
### TESTS ###
# 1. Record counts > User Counts for each ATC (PRE & POST)
# 2. Number of unique ATC codes in records counts == Number of unique ATC codes in user counts (OVERALL,for each indication)
# 3. Acne + Dermatitis + Psoriasis record counts == Overall record counts (PRE & POST)
# 4. Acne + Dermatitis + Psoriasis user counts (per ATC) == Overall user counts (PRE & POST)

###############################################################################
###############################################################################
###############################################################################
# 1. Record counts >= User Counts for each ATC (PRE & POST)
RAM_records_users_merged<-merge(RAM_records_all[,.(ATC.RAM,pre,post)],RAM_users_all[,.(ATC.RAM,pre,post)],by="ATC.RAM",suffixes=c("_records","_users"))
#check
RAM.records.pre_lessthan_RAMusers.pre<-nrow(RAM_records_users_merged[pre_records<pre_users,])
RAM.records.post_lessthan_RAMusers.post<-nrow(RAM_records_users_merged[post_records<post_users,])

# 2. Number of unique ATC codes in records counts == Number of unique ATC codes in user counts (OVERALL,for each indication)
#check
nr.unique.ATC.records_notequal_nr.unique.ATC.users<-length(unique(RAM_records_all$ATC.RAM))-length(unique(RAM_users_all$ATC.RAM))
nr.unique.ATC.records_notequal_nr.unique.ATC.users.ACNE<-length(unique(RAM_records_acne$ATC.RAM))-length(unique(RAM_users_acne$ATC.RAM))
nr.unique.ATC.records_notequal_nr.unique.ATC.users.DERMATITIS<-length(unique(RAM_records_derm$ATC.RAM))-length(unique(RAM_users_derm$ATC.RAM))
nr.unique.ATC.records_notequal_nr.unique.ATC.users.PSORIASIS<-length(unique(RAM_records_psor$ATC.RAM))-length(unique(RAM_users_psor$ATC.RAM))

# 3. Acne + Dermatitis + Psoriasis record counts == Overall record counts (PRE & POST)
# combine record files
RAM_records_combined<-rbindlist(list(
  RAM_records_acne[,.(ATC.RAM,pre,post)],
  RAM_records_derm[,.(ATC.RAM,pre,post)],
  RAM_records_psor[,.(ATC.RAM,pre,post)]
))
# sum 'N' by 'YM' across age groups
RAM_records_sums<-RAM_records_combined[,.(pre_sum_ind=sum(pre,na.rm=TRUE),post_sum_ind=sum(post,na.rm=TRUE)), by = ATC.RAM]
# merge the summed age group data with incidence_all by 'YM'
RAM_records_merged<-merge(RAM_records_all[,.(ATC.RAM,pre_from_all=pre,post_from_all=post)],RAM_records_sums,by="ATC.RAM",all.x=TRUE)
#check
RAM.records.pre.from.all_notequal_RAM.records.pre.from.indication.sum<-nrow(RAM_records_merged[pre_from_all!=pre_sum_ind,])
RAM.records.post.from.all_notequal_RAM.records.post.from.indication.sum<-nrow(RAM_records_merged[post_from_all!=post_sum_ind,])

# 4. Acne + Dermatitis + Psoriasis user counts (per ATC) == Overall user counts (PRE & POST)
# combine user files
RAM_users_combined<-rbindlist(list(
  RAM_users_acne[,.(ATC.RAM,pre,post)],
  RAM_users_derm[,.(ATC.RAM,pre,post)],
  RAM_users_psor[,.(ATC.RAM,pre,post)]
))
# sum 'N' by 'YM' across age groups
RAM_users_sums<-RAM_users_combined[,.(pre_sum_ind=sum(pre,na.rm=TRUE),post_sum_ind=sum(post,na.rm=TRUE)), by = ATC.RAM]
# merge the summed age group data with incidence_all by 'YM'
RAM_users_merged<-merge(RAM_users_all[,.(ATC.RAM,pre_from_all=pre,post_from_all=post)],RAM_users_sums,by="ATC.RAM",all.x=TRUE)
#check
RAM.users.pre.from.all_notequal_RAM.users.pre.from.indication.sum<-nrow(RAM_users_merged[pre_from_all!=pre_sum_ind,])
RAM.users.post.from.all_notequal_RAM.users.post.from.indication.sum<-nrow(RAM_users_merged[post_from_all!=post_sum_ind,])

### FLOWCHART ###
# Define the names vector
names <- c(
  # 1. Record counts > User Counts for each ATC (PRE & POST)
  "RAM.records.pre_lessthan_RAMusers.pre",
  "RAM.records.post_lessthan_RAMusers.post",
  # 2. Number of unique ATC codes in records counts == Number of unique ATC codes in user counts (OVERALL,for each indication)
  "nr.unique.ATC.records_notequal_nr.unique.ATC.users",
  "nr.unique.ATC.records_notequal_nr.unique.ATC.users.ACNE",
  "nr.unique.ATC.records_notequal_nr.unique.ATC.users.DERMATITIS",
  "nr.unique.ATC.records_notequal_nr.unique.ATC.users.PSORIASIS",
  # 3. Acne + Dermatitis + Psoriasis record counts == Overall record counts (PRE & POST)
  "RAM.records.pre.from.all_notequal_RAM.records.pre.from.indication.sum",
  "RAM.records.post.from.all_notequal_RAM.records.post.from.indication.sum",
  # 4. Acne + Dermatitis + Psoriasis user counts (per ATC) == Overall user counts (PRE & POST)
  "RAM.users.pre.from.all_notequal_RAM.users.pre.from.indication.sum",
  "RAM.users.post.from.all_notequal_RAM.users.post.from.indication.sum"
)

# Define the corresponding variable names in the global environment
values <- c(
  # 1. Record counts > User Counts for each ATC (PRE & POST)
  "RAM.records.pre_lessthan_RAMusers.pre",
  "RAM.records.post_lessthan_RAMusers.post",
  # 2. Number of unique ATC codes in records counts == Number of unique ATC codes in user counts (OVERALL,for each indication)
  "nr.unique.ATC.records_notequal_nr.unique.ATC.users",
  "nr.unique.ATC.records_notequal_nr.unique.ATC.users.ACNE",
  "nr.unique.ATC.records_notequal_nr.unique.ATC.users.DERMATITIS",
  "nr.unique.ATC.records_notequal_nr.unique.ATC.users.PSORIASIS",
  # 3. Acne + Dermatitis + Psoriasis record counts == Overall record counts (PRE & POST)
  "RAM.records.pre.from.all_notequal_RAM.records.pre.from.indication.sum",
  "RAM.records.post.from.all_notequal_RAM.records.post.from.indication.sum",
  # 4. Acne + Dermatitis + Psoriasis user counts (per ATC) == Overall user counts (PRE & POST)
  "RAM.users.pre.from.all_notequal_RAM.users.pre.from.indication.sum",
  "RAM.users.post.from.all_notequal_RAM.users.post.from.indication.sum"
)

# Initialize empty vectors to store valid names, values, and skipped variables
valid_names <- c()
valid_values <- c()
skipped_vars <- c()

# Loop through each value and its corresponding name
for (i in seq_along(values)) {
  var_value <- values[i]  # The variable name as it should exist in the environment
  var_name <- names[i]    # The human-readable name for the flowchart
  
  # Check if the variable exists in the global environment
  if (exists(var_value, envir = .GlobalEnv)) {
    # Get the value of the variable from the environment
    value <- get(var_value, envir = .GlobalEnv)
    
    # Store valid names and values
    valid_names <- c(valid_names, var_name)
    valid_values <- c(valid_values, value)
  } else {
    # If variable is missing, store the skipped variable's name
    skipped_vars <- c(skipped_vars, var_name)
  }
}

# Create a data.table with valid names and values
flowchart_rawRAMcounts <- data.table(names = valid_names, values = valid_values)

# Print the list of skipped variables
if (length(skipped_vars) > 0) {
  message("Skipped variables:")
  print(skipped_vars)
} else {
  message("No variables were skipped.")
}

View(flowchart_rawRAMcounts)
# saveRDS(flowchart_rawRAMcounts, paste0(g_output_dir,"/medicines_counts/flowchart_rawRAMcounts.rds")) 



#################################################################################################
################################ WOCBP vs RETINOID vs RAM counts ################################
#################################################################################################
# Tests
# 1. WOCBP counts >= Retinoid user counts
# 2. Retinoid users counts >= RAM users counts

WOCBP.total<-population_counts_flowchart[names_for_flowchart=="population_WOCBP", values_for_flowchart]
retinoid.users.in.WOCBP<-population_counts_flowchart[names_for_flowchart=="population_retinoid.users.in.WOCBP", values_for_flowchart]
RAM.users.in.retinoid.pop<-population_counts_flowchart[names_for_flowchart=="population_RAM.users.within.retinoid.users", values_for_flowchart]
RAM.users.in.retinoid.pop.acne<-population_counts_flowchart[names_for_flowchart=="population_RAM.users.within.retinoid.users_acne", values_for_flowchart]
RAM.users.in.retinoid.pop.derm<-population_counts_flowchart[names_for_flowchart=="population_RAM.users.within.retinoid.users_derm", values_for_flowchart]
RAM.users.in.retinoid.pop.psor<-population_counts_flowchart[names_for_flowchart=="population_RAM.users.within.retinoid.users_psor", values_for_flowchart]

WOCBP_morethan_retinoid.users<-WOCBP.total-retinoid.users.in.WOCBP>=0
retinoid.users_morethan_RAM.pop.in.retinoid.users<-retinoid.users.in.WOCBP-RAM.users.in.retinoid.pop>=0
RAM_indication_counts_equal_RAM.pop.in.retinoid.users<-RAM.users.in.retinoid.pop.acne+RAM.users.in.retinoid.pop.derm+RAM.users.in.retinoid.pop.psor == RAM.users.in.retinoid.pop

# flowchart 
library(data.table)

# Define the names vector
names <- c(
  "WOCBP_morethan_retinoid.users",
  "retinoid.users_morethan_RAM.pop.in.retinoid.users",
  "RAM_indication_counts_equal_RAM.pop.in.retinoid.users"
)

# Define the corresponding variable names in the global environment
values <- c(
  "WOCBP_morethan_retinoid.users",
  "retinoid.users_morethan_RAM.pop.in.retinoid.users",
  "RAM_indication_counts_equal_RAM.pop.in.retinoid.users"
)

# Initialize empty vectors to store valid names, values, and skipped variables
valid_names <- c()
valid_values <- c()
skipped_vars <- c()

# Loop through each value and its corresponding name
for (i in seq_along(values)) {
  var_value <- values[i]  # The variable name as it should exist in the environment
  var_name <- names[i]    # The human-readable name for the flowchart
  
  # Check if the variable exists in the global environment
  if (exists(var_value, envir = .GlobalEnv)) {
    # Get the value of the variable from the environment
    value <- get(var_value, envir = .GlobalEnv)
    
    # Store valid names and values
    valid_names <- c(valid_names, var_name)
    valid_values <- c(valid_values, value)
  } else {
    # If variable is missing, store the skipped variable's name
    skipped_vars <- c(skipped_vars, var_name)
  }
}

# Create a data.table with valid names and values
flowchart_population_comparisons <- data.table(names = valid_names, values = valid_values)

# Print the list of skipped variables
if (length(skipped_vars) > 0) {
  message("Skipped variables:")
  print(skipped_vars)
} else {
  message("No variables were skipped.")
}

View(flowchart_population_comparisons)
# saveRDS(flowchart_population_comparisons, paste0(g_output_dir,"/medicines_counts/flowchart_population_comparisons.rds")) 