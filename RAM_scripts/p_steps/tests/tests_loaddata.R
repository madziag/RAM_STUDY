# Function to check if file exists
load_if_exists<-function(file_path){if(file.exists(file_path)){return(as.data.table(readRDS(file_path)))}else{print(paste("File does not exist:", file_path))}}

# Set paths
# g_output_dir<-"C:/Users/mgamb/Desktop/Pharmo_output/g_output"
g_output_dir<-"C:/Users/mgamb/Documents/GitHub/RAM_STUDY/RAM_scripts/g_output"
# Paths to folders 
obj1_dir<-paste0(g_output_dir,"/medicines_counts/RAM_Objective_1")
obj2_dir<-paste0(g_output_dir,"/medicines_counts/RAM_Objective_2")
obj3_dir<-paste0(g_output_dir,"/medicines_counts/RAM_Objective_3")
obj4_dir<-paste0(g_output_dir,"/medicines_counts/RAM_Objective_4")

# Load data 
### OBJECTIVE 1: INCIDENCE ###

incidence_all<-load_if_exists(paste0(obj1_dir,"/ALL_RAM_incidence_counts.rds"))

incidence_age_group_2099<-load_if_exists(paste0(obj1_dir,"/Stratified/ALL_RAM_incidence_counts_12-20.99_age_group.rds"))
incidence_age_group_3099<-load_if_exists(paste0(obj1_dir,"/Stratified/ALL_RAM_incidence_counts_21-30.99_age_group.rds"))
incidence_age_group_4099<-load_if_exists(paste0(obj1_dir,"/Stratified/ALL_RAM_incidence_counts_31-40.99_age_group.rds"))
incidence_age_group_5599<-load_if_exists(paste0(obj1_dir,"/Stratified/ALL_RAM_incidence_counts_41-55.99_age_group.rds"))

incidence_indication_acne<-load_if_exists(paste0(obj1_dir,"/Stratified/ALL_RAM_incidence_counts_acne_indication_group.rds"))
incidence_indication_derm<-load_if_exists(paste0(obj1_dir,"/Stratified/ALL_RAM_incidence_counts_dermatitis_indication_group.rds"))
incidence_indication_psor<-load_if_exists(paste0(obj1_dir,"/Stratified/ALL_RAM_incidence_counts_psoriasis_indication_group.rds"))

### OBJECTIVE 1: PREVALENCE ###

prevalence_all<-load_if_exists(paste0(obj1_dir,"/ALL_RAM_prevalence_counts.rds"))

prevalence_age_group_2099<-load_if_exists(paste0(obj1_dir,"/Stratified/ALL_RAM_prevalence_counts_12-20.99_age_group.rds"))
prevalence_age_group_3099<-load_if_exists(paste0(obj1_dir,"/Stratified/ALL_RAM_prevalence_counts_21-30.99_age_group.rds"))
prevalence_age_group_4099<-load_if_exists(paste0(obj1_dir,"/Stratified/ALL_RAM_prevalence_counts_31-40.99_age_group.rds"))
prevalence_age_group_5599<-load_if_exists(paste0(obj1_dir,"/Stratified/ALL_RAM_prevalence_counts_41-55.99_age_group.rds"))

prevalence_indication_acne<-load_if_exists(paste0(obj1_dir,"/Stratified/ALL_RAM_prevalence_counts_acne_indication_group.rds"))
prevalence_indication_derm<-load_if_exists(paste0(obj1_dir,"/Stratified/ALL_RAM_prevalence_counts_dermatitis_indication_group.rds"))
prevalence_indication_psor<-load_if_exists(paste0(obj1_dir,"/Stratified/ALL_RAM_prevalence_counts_psoriasis_indication_group.rds"))

### OBJECTIVE 2: DISCONTINUED ###

discontinued_all<-load_if_exists(paste0(obj2_dir,"/ALL_RAM_discontinued_counts.rds"))

discontinued_age_group_2099<-load_if_exists(paste0(obj2_dir,"/Stratified/ALL_RAM_discontinued_counts_12-20.99_age_group.rds"))
discontinued_age_group_3099<-load_if_exists(paste0(obj2_dir,"/Stratified/ALL_RAM_discontinued_counts_21-30.99_age_group.rds"))
discontinued_age_group_4099<-load_if_exists(paste0(obj2_dir,"/Stratified/ALL_RAM_discontinued_counts_31-40.99_age_group.rds"))
discontinued_age_group_5599<-load_if_exists(paste0(obj2_dir,"/Stratified/ALL_RAM_discontinued_counts_41-55.99_age_group.rds"))

discontinued_indication_acne<-load_if_exists(paste0(obj2_dir,"/Stratified/ALL_RAM_discontinued_counts_acne_indication_group.rds"))
discontinued_indication_derm<-load_if_exists(paste0(obj2_dir,"/Stratified/ALL_RAM_discontinued_counts_dermatitis_indication_group.rds"))
discontinued_indication_psor<-load_if_exists(paste0(obj2_dir,"/Stratified/ALL_RAM_discontinued_counts_psoriasis_indication_group.rds"))

### OBJECTIVE 2: SWITCHERS ###

switcher_1<-load_if_exists(paste0(obj2_dir,"/ALL_RAM_switcher_1_counts.rds"))
switcher_2<-load_if_exists(paste0(obj2_dir,"/ALL_RAM_switcher_2_counts.rds"))

switcher_age_group_2099<-load_if_exists(paste0(obj2_dir,"/Stratified/ALL_RAM_switcher_counts_12-20.99_age_group.rds"))
switcher_age_group_3099<-load_if_exists(paste0(obj2_dir,"/Stratified/ALL_RAM_switcher_counts_21-30.99_age_group.rds"))
switcher_age_group_4099<-load_if_exists(paste0(obj2_dir,"/Stratified/ALL_RAM_switcher_counts_31-40.99_age_group.rds"))
switcher_age_group_5599<-load_if_exists(paste0(obj2_dir,"/Stratified/ALL_RAM_switcher_counts_41-55.99_age_group.rds"))

switcher_indication_acne<-load_if_exists(paste0(obj2_dir,"/Stratified/ALL_RAM_switcher_counts_acne_indication_group.rds"))
switcher_indication_derm<-load_if_exists(paste0(obj2_dir,"/Stratified/ALL_RAM_switcher_counts_dermatitis_indication_group.rds"))
switcher_indication_psor<-load_if_exists(paste0(obj2_dir,"/Stratified/ALL_RAM_switcher_counts_psoriasis_indication_group.rds"))

### OBJECTIVE 3: GENERAL CONCOMITANCE ###
genconcomit_all_records<-load_if_exists(paste0(obj3_dir,"/ALL_RAM_generalconcomit_RECORDS_counts.rds"))
genconcomit_all_users<-load_if_exists(paste0(obj3_dir,"/ALL_RAM_general_concomit_USERS_counts.rds"))

genconcomit_age_group_2099<-load_if_exists(paste0(obj3_dir,"/Stratified/ALL_RAM_general_concomit_USERS_counts_12-20.99_age_group.rds"))
genconcomit_age_group_3099<-load_if_exists(paste0(obj3_dir,"/Stratified/ALL_RAM_general_concomit_USERS_counts_21-30.99_age_group.rds"))
genconcomit_age_group_4099<-load_if_exists(paste0(obj3_dir,"/Stratified/ALL_RAM_general_concomit_USERS_counts_31-40.99_age_group.rds"))
genconcomit_age_group_5599<-load_if_exists(paste0(obj3_dir,"/Stratified/ALL_RAM_general_concomit_USERS_counts_41-55.99_age_group.rds"))

genconcomit_indication_acne<-load_if_exists(paste0(obj3_dir,"/Stratified/ALL_RAM_general_concomit_USERS_counts_acne_indication_group.rds"))
genconcomit_indication_derm<-load_if_exists(paste0(obj3_dir,"/Stratified/ALL_RAM_general_concomit_USERS_counts_dermatitis_indication_group.rds"))
genconcomit_indication_psor<-load_if_exists(paste0(obj3_dir,"/Stratified/ALL_RAM_general_concomit_USERS_counts_psoriasis_indication_group.rds"))

### OBJECTIVE 3: CONTRAINDICATED ###
contra_all_records<-load_if_exists(paste0(obj3_dir,"/ALL_RAM_contra_RECORDS_counts.rds"))
contra_all_users<-load_if_exists(paste0(obj3_dir,"/ALL_RAM_contra_USERS_counts.rds"))

contra_indication_acne<-load_if_exists(paste0(obj3_dir,"/Stratified/ALL_RAM_contra_USERS_counts_acne_indication_group.rds"))
contra_indication_derm<-load_if_exists(paste0(obj3_dir,"/Stratified/ALL_RAM_contra_USERS_counts_dermatitis_indication_group.rds"))
contra_indication_psor<-load_if_exists(paste0(obj3_dir,"/Stratified/ALL_RAM_contra_USERS_counts_psoriasis_indication_group.rds"))

### OBJECTIVE 4: TERATOGENIC ###

teratogenic_all_records<-load_if_exists(paste0(obj4_dir,"/ALL_RAM_teratogenic_RECORDS_counts.rds"))
teratogenic_all_users<-load_if_exists(paste0(obj4_dir,"/ALL_RAM_teratogenic_USERS_counts.rds"))

teratogenic_indication_acne<-load_if_exists(paste0(obj4_dir,"/Stratified/ALL_RAM_teratogenic_USERS_counts_acne_indication_group.rds"))
teratogenic_indication_derm<-load_if_exists(paste0(obj4_dir,"/Stratified/ALL_RAM_teratogenic_USERS_counts_dermatitis_indication_group.rds"))
teratogenic_indication_psor<-load_if_exists(paste0(obj4_dir,"/Stratified/ALL_RAM_teratogenic_USERS_counts_psoriasis_indication_group.rds"))

### RAM COUNTS ###

RAM_records_all<-load_if_exists(paste0(g_output_dir,"/medicines_counts/ALL_RAM_record_counts_overall.rds"))

RAM_records_acne<-load_if_exists(paste0(g_output_dir,"/medicines_counts/ALL_RAM_record_counts_acne.rds"))
RAM_records_derm<-load_if_exists(paste0(g_output_dir,"/medicines_counts/ALL_RAM_record_counts_dermatitis.rds"))
RAM_records_psor<-load_if_exists(paste0(g_output_dir,"/medicines_counts/ALL_RAM_record_counts_psoriasis.rds"))

RAM_users_all<-load_if_exists(paste0(g_output_dir,"/medicines_counts/ALL_RAM_user_counts_overall.rds"))

RAM_users_acne<-load_if_exists(paste0(g_output_dir,"/medicines_counts/ALL_RAM_user_counts_acne.rds"))
RAM_users_derm<-load_if_exists(paste0(g_output_dir,"/medicines_counts/ALL_RAM_user_counts_dermatitis.rds"))
RAM_users_psor<-load_if_exists(paste0(g_output_dir,"/medicines_counts/ALL_RAM_user_counts_psoriasis.rds"))

### RETINOID COUNTS ###
retinoid_incidence<-load_if_exists(paste0(g_output_dir,"/medicines_counts/ALL_Retinoid_incidence_counts.rds"))
retinoid_prevalence<-load_if_exists(paste0(g_output_dir,"/medicines_counts/ALL_Retinoid_prevalence_counts.rds"))
retinoid_discontinued<-load_if_exists(paste0(g_output_dir,"/medicines_counts/ALL_Retinoid_discontinued_counts.rds"))

