# Function to check if file exists
load_if_exists<-function(file_path){if(file.exists(file_path)){return(as.data.table(readRDS(file_path)))}else{print(paste("File does not exist:", file_path))}}

# Set paths
# g_output_dir<-"C:/Users/mgamb/Desktop/Pharmo_output/g_output"
g_output_dir<-"C:/Users/mgamb/Desktop/BIFAP_results/Pooled"
# Load data 
### OBJECTIVE 1: INCIDENCE ###

incidence_all<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_incidence_counts_pooled.rds"))

incidence_age_group_2099<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_incidence_counts_12-20.99_age_group_pooled.rds"))
incidence_age_group_3099<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_incidence_counts_21-30.99_age_group_pooled.rds"))
incidence_age_group_4099<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_incidence_counts_31-40.99_age_group_pooled.rds"))
incidence_age_group_5599<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_incidence_counts_41-55.99_age_group_pooled.rds"))

incidence_indication_acne<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_incidence_counts_acne_indication_group_pooled.rds"))
incidence_indication_derm<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_incidence_counts_dermatitis_indication_group_pooled.rds"))
incidence_indication_psor<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_incidence_counts_psoriasis_indication_group_pooled.rds"))

### OBJECTIVE 1: PREVALENCE ###

prevalence_all<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_prevalence_counts_pooled.rds"))

prevalence_age_group_2099<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_prevalence_counts_12-20.99_age_group_pooled.rds"))
prevalence_age_group_3099<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_prevalence_counts_21-30.99_age_group_pooled.rds"))
prevalence_age_group_4099<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_prevalence_counts_31-40.99_age_group_pooled.rds"))
prevalence_age_group_5599<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_prevalence_counts_41-55.99_age_group_pooled.rds"))

prevalence_indication_acne<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_prevalence_counts_acne_indication_group_pooled.rds"))
prevalence_indication_derm<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_prevalence_counts_dermatitis_indication_group_pooled.rds"))
prevalence_indication_psor<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_prevalence_counts_psoriasis_indication_group_pooled.rds"))

### OBJECTIVE 2: DISCONTINUED ###

discontinued_all<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_discontinued_counts_pooled.rds"))

discontinued_age_group_2099<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_discontinued_counts_12-20.99_age_group_pooled.rds"))
discontinued_age_group_3099<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_discontinued_counts_21-30.99_age_group_pooled.rds"))
discontinued_age_group_4099<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_discontinued_counts_31-40.99_age_group_pooled.rds"))
discontinued_age_group_5599<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_discontinued_counts_41-55.99_age_group_pooled.rds"))

discontinued_indication_acne<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_discontinued_counts_acne_indication_group_pooled.rds"))
discontinued_indication_derm<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_discontinued_counts_dermatitis_indication_group_pooled.rds"))
discontinued_indication_psor<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_discontinued_counts_psoriasis_indication_group_pooled.rds"))

### OBJECTIVE 2: SWITCHERS ###

switcher_1<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_switcher_1_counts_pooled.rds"))
switcher_2<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_switcher_2_counts_pooled.rds"))

switcher_age_group_2099<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_switcher_counts_12-20.99_age_group_pooled.rds"))
switcher_age_group_3099<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_switcher_counts_21-30.99_age_group_pooled.rds"))
switcher_age_group_4099<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_switcher_counts_31-40.99_age_group_pooled.rds"))
switcher_age_group_5599<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_switcher_counts_41-55.99_age_group_pooled.rds"))

switcher_indication_acne<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_switcher_counts_acne_indication_group_pooled.rds"))
switcher_indication_derm<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_switcher_counts_dermatitis_indication_group_pooled.rds"))
switcher_indication_psor<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_switcher_counts_psoriasis_indication_group_pooled.rds"))

### OBJECTIVE 3: GENERAL CONCOMITANCE ###
genconcomit_all_records<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_generalconcomit_RECORDS_counts_pooled.rds"))
genconcomit_all_users<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_general_concomit_USERS_counts_pooled.rds"))

genconcomit_age_group_2099<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_general_concomit_USERS_counts_12-20.99_age_group_pooled.rds"))
genconcomit_age_group_3099<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_general_concomit_USERS_counts_21-30.99_age_group_pooled.rds"))
genconcomit_age_group_4099<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_general_concomit_USERS_counts_31-40.99_age_group_pooled.rds"))
genconcomit_age_group_5599<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_general_concomit_USERS_counts_41-55.99_age_group_pooled.rds"))

genconcomit_indication_acne<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_general_concomit_USERS_counts_acne_indication_group_pooled.rds"))
genconcomit_indication_derm<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_general_concomit_USERS_counts_dermatitis_indication_group_pooled.rds"))
genconcomit_indication_psor<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_general_concomit_USERS_counts_psoriasis_indication_group_pooled.rds"))

### OBJECTIVE 3: CONTRAINDICATED ###
contra_all_records<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_contra_RECORDS_counts_pooled.rds"))
contra_all_users<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_contra_USERS_counts_pooled.rds"))

contra_indication_acne<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_contra_USERS_counts_acne_indication_group_pooled.rds"))
contra_indication_derm<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_contra_USERS_counts_dermatitis_indication_group_pooled.rds"))
contra_indication_psor<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_contra_USERS_counts_psoriasis_indication_group_pooled.rds"))

### OBJECTIVE 4: TERATOGENIC ###

teratogenic_all_records<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_teratogenic_RECORDS_counts_pooled.rds"))
teratogenic_all_users<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_teratogenic_USERS_counts_pooled.rds"))

teratogenic_indication_acne<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_teratogenic_USERS_counts_acne_indication_group_pooled.rds"))
teratogenic_indication_derm<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_teratogenic_USERS_counts_dermatitis_indication_group_pooled.rds"))
teratogenic_indication_psor<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_teratogenic_USERS_counts_psoriasis_indication_group_pooled.rds"))

### RAM COUNTS ###

RAM_records_all<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_record_counts_overall_pooled.rds"))

RAM_records_acne<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_record_counts_acne_pooled.rds"))
RAM_records_derm<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_record_counts_dermatitis_pooled.rds"))
RAM_records_psor<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_record_counts_psoriasis_pooled.rds"))

RAM_users_all<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_user_counts_overall_pooled.rds"))

RAM_users_acne<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_user_counts_acne_pooled.rds"))
RAM_users_derm<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_user_counts_dermatitis_pooled.rds"))
RAM_users_psor<-load_if_exists(paste0(g_output_dir,"/ALL_RAM_user_counts_psoriasis_pooled.rds"))

### RETINOID COUNTS ###
retinoid_incidence<-load_if_exists(paste0(g_output_dir,"/ALL_Retinoid_incidence_counts_pooled.rds"))
retinoid_prevalence<-load_if_exists(paste0(g_output_dir,"/ALL_Retinoid_prevalence_counts_pooled.rds"))
retinoid_discontinued<-load_if_exists(paste0(g_output_dir,"/ALL_Retinoid_discontinued_counts_pooled.rds"))

