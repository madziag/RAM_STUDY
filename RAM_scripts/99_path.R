#Author: Magdalena Gamba M.D.,Ema Alsina MSc.
#email: m.a.gamba@uu.nl,e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 31/01/2022

#This script sets and saves paths to folders needed for all subsequent scripts
# setwd('..') #in Data Characterization

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

setwd('..') #in ConcePTION
dir_base<-getwd()
# set the name of the study
StudyName<-"LOT4" #name of folder 
path_dir<-paste0(dir_base,"/CDMInstances/",StudyName,"/")
# path_dir<-paste0(dir_base,"/CDMInstances_preselect/") # use this option if you want to use the preselection files
# path<-path_dir

# Checks if folders exist. If they do not, creates them 
# Main folders (g_intermediate, g_output)
invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_intermediate")), dir.create(paste0(projectFolder, "/g_intermediate")), FALSE))
g_intermediate<-paste0(projectFolder, "/g_intermediate/")
invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output")), dir.create(paste0(projectFolder, "/g_output")), FALSE))
output_dir<-paste0(projectFolder, "/g_output/")
# Sets path to p_steps (to read codelists)
pre_dir<-paste0(projectFolder,"/p_steps/")
# folders + paths in g_intermediate
invisible(ifelse(!dir.exists(paste0(g_intermediate, "/populations")), dir.create(paste0(g_intermediate, "/populations")), FALSE))
populations_dir<-paste0(g_intermediate,"populations/")
invisible(ifelse(!dir.exists(paste0(g_intermediate, "/tmp", sep="")), dir.create(paste0(g_intermediate, "/tmp")), FALSE))
tmp<-paste0(g_intermediate,"tmp/")
invisible(ifelse(!dir.exists(paste0(g_intermediate, "/counts_dfs", sep="")), dir.create(paste0(g_intermediate, "/counts_dfs")), FALSE))
counts_dfs_dir<-paste0(g_intermediate,"counts_dfs/")
# Create folder to store incidence, prevalence and discontinued individual level records 
invisible(ifelse(!dir.exists(paste0(counts_dfs_dir,"retinoid_counts")),dir.create(paste0(counts_dfs_dir,"retinoid_counts")),FALSE))
retinoid_counts_dfs<-paste0(counts_dfs_dir,"retinoid_counts/")

# Creates treatment episodes directories
invisible(ifelse(!dir.exists(paste0(g_intermediate,"/retinoid_treatment_episodes")), dir.create(paste0(g_intermediate,"/retinoid_treatment_episodes")), FALSE))
retinoid_treatment_episodes<-paste0(g_intermediate,"retinoid_treatment_episodes/")
invisible(ifelse(!dir.exists(paste0(g_intermediate,"/RAM_treatment_episodes")), dir.create(paste0(g_intermediate,"/RAM_treatment_episodes")), FALSE))
RAM_treatment_episodes<-paste0(g_intermediate,"RAM_treatment_episodes/")
# Folders in g_intermediate/tmp
# CONCEPT SET FOLDERS
invisible(ifelse(!dir.exists(paste0(tmp, "conceptsets_dx")), dir.create(paste0(tmp, "conceptsets_dx")), FALSE))
conceptsets_DX_dir<-paste0(tmp, "conceptsets_dx/")
invisible(ifelse(!dir.exists(paste0(tmp, "conceptsets_atc")), dir.create(paste0(tmp, "conceptsets_atc")), FALSE))
conceptsets_ATC_dir<-paste0(tmp, "conceptsets_atc/")
invisible(ifelse(!dir.exists(paste0(tmp, "conceptsets_proc")), dir.create(paste0(tmp, "conceptsets_proc")), FALSE))
conceptsets_PROC_dir<-paste0(tmp, "conceptsets_proc/")

# MEDICINES TABLES FOLDER
# Temporary folder 
invisible(ifelse(!dir.exists(paste0(tmp, "events_atc")), dir.create(paste0(tmp, "events_atc")), FALSE))
events_tmp_ATC<-paste0(tmp, "events_atc/")
# Permanent folder
invisible(ifelse(!dir.exists(paste0(tmp, "medications")), dir.create(paste0(tmp, "medications")), FALSE))
medications_pop<-paste0(tmp, "medications/")

# Monthly counts
invisible(ifelse(!dir.exists(paste0(output_dir, "monthly_counts_atc")), dir.create(paste0(output_dir, "monthly_counts_atc")), FALSE))
monthly_counts_atc<-paste0(output_dir, "monthly_counts_atc")

# STERILITY FOLDERS
# Temporary folder
invisible(ifelse(!dir.exists(paste0(tmp, "events_sterility")), dir.create(paste0(tmp, "events_sterility")), FALSE))
events_tmp_sterility<-paste0(tmp, "events_sterility/")
# Permanent folder
invisible(ifelse(!dir.exists(paste0(tmp, "sterility")), dir.create(paste0(tmp, "sterility")), FALSE))
sterility_pop<-paste0(tmp, "sterility/")

# PLOT FOLDER
invisible(ifelse(!dir.exists(paste0(output_dir, "plots")), dir.create(paste0(output_dir, "plots")), FALSE))
plot_folder<-paste0(output_dir, "plots")

# MAIN OUTPUT FOLDERS
# 1. PRELIMINARY COUNTS 
invisible(ifelse(!dir.exists(paste0(output_dir, "preliminary_counts")), dir.create(paste0(output_dir, "preliminary_counts")), FALSE))
preliminary_counts_dir<-paste0(output_dir, "preliminary_counts")
# 2. BASELINE TABLES 
invisible(ifelse(!dir.exists(paste0(output_dir, "baseline_tables")), dir.create(paste0(output_dir, "baseline_tables")), FALSE))
baseline_tables_dir<-paste0(output_dir, "baseline_tables")
# 3. MEDICINES COUNTS 
invisible(ifelse(!dir.exists(paste0(output_dir, "medicines_counts")), dir.create(paste0(output_dir, "medicines_counts")), FALSE))
medicines_counts_dir<-paste0(output_dir, "medicines_counts")

# Move stratified records into stratified folders
# Create stratified folder
invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/","stratified")), dir.create(paste0(medicines_counts_dir,"/","stratified")), FALSE))
medicines_stratified_dir<-paste0(medicines_counts_dir,"/","stratified")
# Create stratified by age groups folder
invisible(ifelse(!dir.exists(paste0(medicines_stratified_dir,"/","age_group")), dir.create(paste0(medicines_stratified_dir,"/","age_group")), FALSE))
medicines_stratified_age_groups<-paste0(medicines_stratified_dir ,"/","age_group")
# Create stratified by indication folder 
invisible(ifelse(!dir.exists(paste0(medicines_stratified_dir,"/","indication")), dir.create(paste0(medicines_stratified_dir,"/","indication")), FALSE))
medicines_stratified_indication<-paste0(medicines_stratified_dir ,"/","indication")

# To save incidence and prevalence patient level records  
invisible(ifelse(!dir.exists(paste0(counts_dfs_dir,"RAM_Objective_1")),dir.create(paste0(counts_dfs_dir,"RAM_Objective_1")),FALSE))
objective1_temp_dir<-paste0(counts_dfs_dir,"RAM_Objective_1/")
# To save incidence and prevalence counts/rates
invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/RAM_Objective_1")),dir.create(paste0(medicines_counts_dir,"/RAM_Objective_1")),FALSE))
objective1_dir<-paste0(medicines_counts_dir,"/RAM_Objective_1")
# To save incidence and prevalence stratified counts
invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/RAM_Objective_1/Stratified")),dir.create(paste0(medicines_counts_dir,"/RAM_Objective_1/Stratified")),FALSE))
objective1_strat_dir<-paste0(medicines_counts_dir,"/RAM_Objective_1/Stratified")

# To save switching and discontinued patient level records  
invisible(ifelse(!dir.exists(paste0(counts_dfs_dir,"RAM_Objective_2")),dir.create(paste0(counts_dfs_dir,"RAM_Objective_2")),FALSE))
objective2_temp_dir<-paste0(counts_dfs_dir,"RAM_Objective_2/")
# To save switching and discontinued counts/rates
invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/RAM_Objective_2")),dir.create(paste0(medicines_counts_dir,"/RAM_Objective_2")),FALSE))
objective2_dir<-paste0(medicines_counts_dir,"/RAM_Objective_2")
# To save switching and discontinued stratified counts
invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/RAM_Objective_2/Stratified")),dir.create(paste0(medicines_counts_dir,"/RAM_Objective_2/Stratified")),FALSE))
objective2_strat_dir<-paste0(medicines_counts_dir,"/RAM_Objective_2/Stratified")

# To save concomitance patient level records  
invisible(ifelse(!dir.exists(paste0(counts_dfs_dir,"RAM_Objective_3")),dir.create(paste0(counts_dfs_dir,"RAM_Objective_3")),FALSE))
objective3_temp_dir<-paste0(counts_dfs_dir,"RAM_Objective_3/")
# To save concomitance patient level records  
invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/RAM_Objective_3")),dir.create(paste0(medicines_counts_dir,"/RAM_Objective_3")),FALSE))
objective3_dir<-paste0(medicines_counts_dir,"/RAM_Objective_3")
# To save concomitance patient level records  
invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/RAM_Objective_3/Stratified")),dir.create(paste0(medicines_counts_dir,"/RAM_Objective_3/Stratified")),FALSE))
objective3_strat_dir<-paste0(medicines_counts_dir,"/RAM_Objective_3/Stratified")

# To save concomitance patient level records  
invisible(ifelse(!dir.exists(paste0(counts_dfs_dir,"RAM_Objective_4")),dir.create(paste0(counts_dfs_dir,"RAM_Objective_4")),FALSE))
objective4_temp_dir<-paste0(counts_dfs_dir,"RAM_Objective_4/")
# To save concomitance patient level records  
invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/RAM_Objective_4")),dir.create(paste0(medicines_counts_dir,"/RAM_Objective_4")),FALSE))
objective4_dir<-paste0(medicines_counts_dir,"/RAM_Objective_4")
# To save concomitance patient level records  
invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/RAM_Objective_4/Stratified")),dir.create(paste0(medicines_counts_dir,"/RAM_Objective_4/Stratified")),FALSE))
objective4_strat_dir<-paste0(medicines_counts_dir,"/RAM_Objective_4/Stratified")