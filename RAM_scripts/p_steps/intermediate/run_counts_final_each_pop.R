#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 16/02/2022

# VERSION 3.2.1 <- THIS HAS SOME SCRIPTS COMMENTED OUT IN ORDER TO SPEED UP LAST MINUTE RE-RUN OF TO_RUN_FINAL_COUNTS.R. FOR VERSION 3.3, the commented out sourced scripts need to be uncommented so they can be re-run one final time.

# Takes into account subpopulations 
# Loads records with Retinoid use (depending on study type)
# Loads study_population 
# Runs individual scripts for each subpopulation
# Result: If SUBP -> TRUE then each folder will contain (if present) results coming from all indicated subpops. Resulting files are prefixed with the name of the subpop

# Loads study population/populations 
populations<-list.files(populations_dir, pattern = "study_population")
populations<-populations[grepl("retinoid", populations)]
# Move denominator file to tmp folder
for(file in list.files(path=output_dir, pattern="denominator", ignore.case = T)){file.move(paste0(output_dir,file), paste0(paste0(tmp, "/") ,file))}

# Loops over each subpopulation
for(pop in 1:length(populations)){
  # Loads study population
  retinoid_study_population<-readRDS(paste0(populations_dir, populations[pop]))
  # Make sure last exit data is 2019 if DAP == "PHARMO"
  if(is_PHARMO){retinoid_study_population<-retinoid_study_population[year(retinoid_study_population$exit_date) < 2020,]}else{retinoid_study_population<-retinoid_study_population}
  # Assign study population prefix name
  pop_prefix<-gsub("_retinoid_study_population.rds", "", populations[pop])

  if(nrow(retinoid_study_population)>0){
    # Creates Retinoid treatment episodes 
    source(paste0(pre_dir, "treatmentepisodes/retinoid_treatment_episodes.R"))
    # Creates RAM treatment episodes in Retinoid Users 
    source(paste0(pre_dir, "treatmentepisodes/RAM_treatment_episodes.R"))
    # Counts of prevalence, incidence, discontinuation - medicines use
    source(paste0(pre_dir, "counts/retinoid_incidence_prevalence_discontinuation.R"))
    # Counts of prevalence, incidence, discontinuation - medicines use
    source(paste0(pre_dir, "counts/RAM_incidence_prevalence_discontinuation.R"))
    # Counts of switchers and general concomitance 
    source(paste0(pre_dir, "counts/RAM_switching.R"))
    # Counts concomitance: general 
    source(paste0(pre_dir, "counts/RAM_general_concomitance_counts.R"))
    # Counts contraindicated RAMs
    source(paste0(pre_dir, "counts/RAM_contraindicated.R"))
    # Counts concomitance: teratogenic 
    source(paste0(pre_dir, "counts/RAM_teratogenic.R"))
    # flow chart 
    source(paste0(pre_dir, "flowchart.R"))
    # Creates baseline tables #
    source(paste0(pre_dir,"baseline/baseline_tables.R"))

    #source(paste0(pre_dir, "plots_mask.R"))
    # Converts all .rds files into .csv or .xlsx (indicated by user)
    #source(paste0(pre_dir, "write_output.R"))
  } else {
    print(paste0("There are no Retinoid/Valproate records for subpopulation: ", pop_prefix))
  }
}


# ### Creates csv/xslx folders inside main folders ###
# # Creates csv/xslx folder inside baseline tables, pregnancy counts and medicines counts folders
# invisible(ifelse(!dir.exists(paste0(baseline_tables_dir,"/",my_format,"_files")), dir.create(paste0(baseline_tables_dir,"/",my_format,"_files")), FALSE))
# baseline_tables_csv_xlsx <- paste0(baseline_tables_dir,"/",my_format,"_files")
# # Create folder inside medicines folder for csv or excel file format
# invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/",my_format,"_files")), dir.create(paste0(medicines_counts_dir,"/",my_format,"_files")), FALSE))
# medicines_counts_csv_xlsx <- paste0(medicines_counts_dir,"/",my_format,"_files")
# 
# ### Creates plot folders inside main folders
# # Create plots folder inside medicines counts folder
# invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/","plots")), dir.create(paste0(medicines_counts_dir,"/","plots")), FALSE))
# medicines_counts_plots <- paste0(medicines_counts_dir,"/","plots")
# 
