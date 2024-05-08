#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 16/02/2022

# Takes into account subpopulations 
# Runs individual scripts for each subpopulation
# Result: If SUBP -> TRUE then each folder will contain (if present) results coming from all indicated subpops. Resulting files are prefixed with the name of the subpop

# Loads study population/populations 
populations<-list.files(populations_dir, pattern = "study_population")
populations<-populations[!grepl("retinoid", populations)]

# Loops over each subpopulation
for(pop in 1:length(populations)){
  # Loads study population
  study_population<-readRDS(paste0(populations_dir, populations[pop]))
  # Assign study population prefix name
  pop_prefix<-gsub("_study_population.rds", "", populations[pop])
  # Creates sterility list 
  source(paste0(pre_dir,"sterility/create_sterility_list.R"))
  # Creates entry and exit dates
  source(paste0(pre_dir,"denominators/create_entry_exit.R"))
  # Creates denominator file
  source(paste0(pre_dir,"denominators/denominator_monthly_WOCBP.R"))
  # Finds records matching retinoid and RAM ATCs - creates dataset of all retinoids and RAMS in study population 
  source(paste0(pre_dir,"counts/monthly_counts_ATC.R")) # This creates datasets of Retinoids as well as RAMs
  # Creates denominator file
  source(paste0(pre_dir,"denominators/denominator_monthly_retinoid_users.R"))
  # Flowchart
  source(paste0(pre_dir,"flowchart_base_population.R"))
  }

# Moves all counts, plots, formatted files to preliminary_counts folder
pattern1 = c("monthly_counts", "plots", paste0(my_format,"_files"))
# files_to_move <- list.files(path=output_dir, pattern=paste0(pattern1, collapse="|"))
for(file in list.files(path=output_dir, pattern=paste0(pattern1, collapse="|"), ignore.case = T)){file.move(paste0(output_dir,file), paste0(paste0(preliminary_counts_dir, "/") ,file))}
# Deletes temp files
for(file in list.files(path = tmp, pattern ="events_")){unlink(paste0(tmp, file), recursive = TRUE)}
# Delete Flowchart files 
for(file in list.files(path = output_dir, pattern ="FlowChart")){unlink(paste0(output_dir, file), recursive = TRUE)}
# Delete Study_population_folder 
for(file in list.files(path = output_dir, pattern ="STUDY_SOURCE_POPULATION")){unlink(paste0(output_dir, file), recursive = TRUE)}




