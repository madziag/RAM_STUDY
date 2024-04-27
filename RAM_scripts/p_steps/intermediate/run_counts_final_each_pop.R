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

  if(length(nrow(retinoid_study_population))>0){
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
    # # Counts unrelated 
    # source(paste0(pre_dir, "7_FinalCounts/unrelated_counts.R"))
    # # Creates baseline tables #
    # # source(paste0(pre_dir,"7_FinalCounts/create_baseline_tables.R"))



    #source(paste0(pre_dir, "plots_mask.R"))
    # Converts all .rds files into .csv or .xlsx (indicated by user)
    #source(paste0(pre_dir, "write_output.R"))
  } else {
    print(paste0("There are no Retinoid/Valproate records for subpopulation: ", pop_prefix))
  }
}


### Creates csv/xslx folders inside main folders ###
# Creates csv/xslx folder inside baseline tables, pregnancy counts and medicines counts folders
invisible(ifelse(!dir.exists(paste0(baseline_tables_dir,"/",my_format,"_files")), dir.create(paste0(baseline_tables_dir,"/",my_format,"_files")), FALSE))
baseline_tables_csv_xlsx <- paste0(baseline_tables_dir,"/",my_format,"_files")
# Create folder inside medicines folder for csv or excel file format
invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/",my_format,"_files")), dir.create(paste0(medicines_counts_dir,"/",my_format,"_files")), FALSE))
medicines_counts_csv_xlsx <- paste0(medicines_counts_dir,"/",my_format,"_files")

### Creates plot folders inside main folders
# Create plots folder inside medicines counts folder
invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/","plots")), dir.create(paste0(medicines_counts_dir,"/","plots")), FALSE))
medicines_counts_plots <- paste0(medicines_counts_dir,"/","plots")

# ### Moves csv/xslx/plot files with matching pattern to corresponding folders
# # baseline tables
# for (file in list.files(path=paste0(output_dir,my_format,"_files"), pattern="baseline", ignore.case = T)){file.copy(paste0(output_dir,my_format,"_files/", file),baseline_tables_csv_xlsx)}
# # medicine_counts_incidence_prevalence_discontinuation/med_use_during_contraception_episode_counts
# for (file in list.files(path=paste0(output_dir,my_format,"_files"), pattern=paste0(c("prevalence", "incidence", "discontinued", "med_use_during_contraception_episodes", "switched_to_alt_meds", "alt_med_retin", "alt_med_valp"), collapse="|"), ignore.case = T)){file.copy(paste0(output_dir,my_format,"_files/", file), medicines_counts_csv_xlsx)}
# #for (file in list.files(path=paste0(output_dir,"plots"), pattern=paste0(c("prevalence", "incidence", "discontinued", "med_use_during_contraception_episodes", "switched_to_alt_meds", "kaplan"), collapse="|"), ignore.case = T)){file.copy(paste0(output_dir,"plots/",file), medicines_counts_plots)}
# for (file in list.files(path=paste0(output_dir,"plots"), pattern=paste0(c("prevalence", "incidence", "discontinued", "med_use_during_contraception_episodes", "switched_to_alt_meds","alt_med_retin", "alt_med_valp"), collapse="|"), ignore.case = T)){file.copy(paste0(output_dir,"plots/",file), medicines_counts_plots)}
# # pregnancy_tests_within_90_days_of_medicine_use_counts
# for (file in list.files(path=paste0(output_dir,my_format,"_files"), pattern="pgtest", ignore.case = T)){file.copy(paste0(output_dir,my_format,"_files/", file),pregnancy_test_counts_csv_xlsx)}
# for (file in list.files(path=paste0(output_dir,"plots"), pattern="pgtest", ignore.case = T)){file.copy(paste0(output_dir,"plots/",file), pregnancy_test_counts_plots)}
# # contraceptive_use_within_90_days_of_medicine_use_counts
# for (file in list.files(path=paste0(output_dir,my_format,"_files"), pattern="contraception_prior", ignore.case = T)){file.copy(paste0(output_dir,my_format,"_files/", file), contraceptive_counts_csv_xlsx)}
# for (file in list.files(path=paste0(output_dir,"plots"), pattern="contraception_prior", ignore.case = T)){file.copy(paste0(output_dir,"plots/",file), contraceptive_counts_plots)}
# #pregnancies_started_during_treatment_episode_counts/med_use_during_pregnancy_counts/all_pregnancies_counts
# for (file in list.files(path=paste0(output_dir,my_format,"_files"), pattern="preg_starts_during_tx_episodes|med_use_during_pregnancy|all_pregnancies", ignore.case = T)){file.copy(paste0(output_dir,my_format,"_files/", file),preg_med_counts_csv_xlsx )}
# for (file in list.files(path=paste0(output_dir,"plots"), pattern="preg_starts_during_tx_episodes|med_use_during_pregnancy|all_pregnancies", ignore.case = T)){file.copy(paste0(output_dir,"plots/",file),preg_med_counts_plots )}
# 
# # Removes csv/xlsx, plots and monthly counts folders from LOT4_script (after everything has been copied to corresponding folders)
# for (file in list.files(path=paste0(output_dir), pattern=paste0(c("plots", paste0(my_format,"_files"), "monthly_counts"), collapse="|"), ignore.case = T)){unlink(paste0(output_dir,file), recursive = TRUE)}
# 
# # Deletes temp files
# for(file in list.files(path = tmp, pattern ="events_")){unlink(paste0(tmp, file), recursive = TRUE)}
# 
