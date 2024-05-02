#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 07/12/2021
# Takes into account if user_input: multiple_regions = T/F
# If multiple_regions = T, for each region with records in CDMInstances folder 
## 1. Moves g_intermediate and g_output folders back into LOT4_scripts folder
## 2. Sources run_counts_final_each_pop for corresponding region (which runs individual scripts for each subpopulation)
## 3. Moves g_intermediate and g_output folders back to corresponding region

# Checks for multiple regions 
if(multiple_regions == T){
  # Gets a list of region names from the CDMInstances folder 
  regions<-list.dirs(path = multiple_regions_dir, full.names = FALSE, recursive = FALSE)
  # Loops over each region
  for(reg in 1:length(regions)){
    # Prints region loop is currently working on
    print("##################################################")
    print("##################################################")
    print(paste("############ RUNNING ANALYSIS FOR ", regions[reg], "############"))
    print("##################################################")
    print("##################################################")
    # Sets paths to data folder for each region
    path_dir<-paste0(multiple_regions_dir, regions[reg], "/")
    # Sources folders for each region 
    source(paste0(pre_dir,"info.R"))
    source(paste0(pre_dir,"parameters/study_parameters.R"))
    ## First removes empty g_intermediate/g_output
    if("g_intermediate" %in% list.files(projectFolder)){unlink(paste0(projectFolder,"/g_intermediate"), recursive = T)}
    if("g_output"       %in% list.files(projectFolder)){unlink(paste0(projectFolder,"/g_output")      , recursive = T)}
    # Moves g_intermediate and g_output folders from corresponding region folder into LOT4_scripts folder
    file.move(paste0(projectFolder, "/", regions[reg], "/g_intermediate"), paste0(projectFolder,"/g_intermediate"))
    file.move(paste0(projectFolder, "/", regions[reg], "/g_output"), paste0(projectFolder,"/g_output"))
    # Create Treatment Episodes folder 
    # Retinoid Treatment Episodes
    invisible(ifelse(!dir.exists(paste0(g_intermediate,"/retinoid_treatment_episodes")), dir.create(paste0(g_intermediate,"/retinoid_treatment_episodes")), FALSE))
    retinoid_treatment_episodes<-paste0(g_intermediate,"retinoid_treatment_episodes/")
    # RAM Treatment Episodes
    invisible(ifelse(!dir.exists(paste0(g_intermediate,"/RAM_treatment_episodes")), dir.create(paste0(g_intermediate,"/RAM_treatment_episodes")), FALSE))
    RAM_treatment_episodes<-paste0(g_intermediate,"RAM_treatment_episodes/")
    # Medicines Counts
    invisible(ifelse(!dir.exists(paste0(output_dir, "medicines_counts")), dir.create(paste0(output_dir, "medicines_counts")), FALSE))
    medicines_counts_dir<-paste0(output_dir, "medicines_counts")
    # For medicine counts data 
    invisible(ifelse(!dir.exists(paste0(g_intermediate, "/counts_dfs", sep="")), dir.create(paste0(g_intermediate, "/counts_dfs")), FALSE))
    counts_dfs_dir<-paste0(g_intermediate,"counts_dfs/")
    # Create folder to store incidence, prevalence and discontinued individual level records 
    invisible(ifelse(!dir.exists(paste0(counts_dfs_dir,"retinoid_counts")),dir.create(paste0(counts_dfs_dir,"retinoid_counts")),FALSE))
    retinoid_counts_dfs<-paste0(counts_dfs_dir,"retinoid_counts/")
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
    # baseline tables 
    invisible(ifelse(!dir.exists(paste0(output_dir, "baseline_tables")), dir.create(paste0(output_dir, "baseline_tables")), FALSE))
    baseline_tables_dir<-paste0(output_dir, "baseline_tables")
    # Source file
    source(paste0(pre_dir,"intermediate/run_counts_final_each_pop.R"))
    # Delete g_intermediate/g_output folders before moving the modified ones back 
    if("g_intermediate" %in% list.files(paste0(projectFolder,"/", regions[reg]))){unlink(paste0(projectFolder,"/", regions[reg],"/g_intermediate"), recursive = T)}
    if("g_output"       %in% list.files(paste0(projectFolder,"/", regions[reg]))){unlink(paste0(projectFolder,"/", regions[reg],"/g_output")      , recursive = T)}
    # Moves g_intermediate, g_output folders from LOT4_script folder to respective regional folders
    file.move(paste0(projectFolder,"/g_intermediate"), paste0(projectFolder, "/", regions[reg], "/g_intermediate"))
    file.move(paste0(projectFolder,"/g_output"), paste0(projectFolder, "/", regions[reg], "/g_output"))
  }
} else {
  # Sources files 
  source(paste0(pre_dir,"info.R"))
  source(paste0(pre_dir,"parameters/study_parameters.R"))
  # Sources run_counts_final_each_pop.R 
  source(paste0(pre_dir,"intermediate/run_counts_final_each_pop.R"))
}


