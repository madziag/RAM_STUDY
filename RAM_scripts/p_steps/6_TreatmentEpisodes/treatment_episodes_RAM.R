# #Author: Ema Alsina MSc.
# #email: e.m.alsina-2@umcutrecht.nl
# #Organisation: UMC Utrecht, Utrecht, The Netherlands
# #Date: 18/12/2021
# 
# # This script does two things:
# # 1. it loads RAM created by running "to_run_source_pop_counts.R", for separate subpopulations and regions if necessary.
# # It finds which RAM's where used by Retinoid users (study population is Retinoid users of WOCBP)
# # It then applys createDOT or a fixed duration value to create an estimated end date of treatment for every record
# # 2. It creates a new data frame where each row is not a record, but instead a treatment episode.
# 


# Creates treatment episodes directory
invisible(ifelse(!dir.exists(paste0(g_intermediate,"treatment_episodes_alt_meds")), dir.create(paste0(g_intermediate,"treatment_episodes_alt_meds")), FALSE))

### Load base population (retinoid users of WOCBP)
retinoid_user_pop <- as.data.table(readRDS(paste0(populations_dir, pop_prefix,"_study_population_retinoids.rds")))[,c("person_id")]
# Get RAM records for retinoid user population 
# Looks for RAM files in medications folder 
RAM_files <- list.files(paste0(tmp,"medications/"), pattern=paste0(pop_prefix, "_altmed"))
RAM_all <- as.data.table(do.call(rbind,lapply(paste0(medications_pop, RAM_files), readRDS)))

# merge files: retinoid users also used RAM 
RAM_in_retinoid_users <- merge(retinoid_user_pop, RAM_all, by="person_id")

# Start treatment episode creation 
my_name<-levels(factor(RAM_in_retinoid_users$Code))
split_data<-split(RAM_in_retinoid_users, RAM_in_retinoid_users$Code)

# Loops over each RAM ATC codes -> creates treatment episodes for each unique code 
for (i in 1:length(split_data)){
  
  cma_data<-split_data[[i]]
  # Get ATC code
  ATC_code<-unique(cma_data$Code)
  # assumed duration column values are assigned dependent on user input DAP_specific_DOT
  if(DAP_specific_DOT==T){source(paste0(pre_dir, "parameters/DAP_specific_assumed_durations.R"))}else{cma_data[,assumed_duration:=30]}
  
  cma_data$Date<-as.Date(cma_data$Date, format="%Y%m%d")
  # Creates treatment episodes
  my_treat_episode<-compute.treatment.episodes(data= cma_data,
                                               ID.colname = "person_id",
                                               event.date.colname = "Date",
                                               event.duration.colname = "assumed_duration",
                                               event.daily.dose.colname = NA,
                                               medication.class.colname = "Code",
                                               carryover.within.obs.window = TRUE,
                                               carry.only.for.same.medication = TRUE,
                                               consider.dosage.change =FALSE,
                                               #change between medicines counts as a new treatment episode
                                               medication.change.means.new.treatment.episode = TRUE,
                                               dosage.change.means.new.treatment.episode = FALSE,
                                               maximum.permissible.gap = 30,
                                               maximum.permissible.gap.unit = c("days", "weeks", "months", "years", "percent")[1],
                                               maximum.permissible.gap.append.to.episode = FALSE,
                                               followup.window.start = 0,
                                               followup.window.start.unit = c("days", "weeks", "months", "years")[1],
                                               followup.window.duration = 365 * 12,
                                               followup.window.duration.unit = c("days", "weeks", "months", "years")[1],
                                               event.interval.colname = "event.interval",
                                               gap.days.colname = "gap.days",
                                               date.format = "%Y-%m-%d",
                                               parallel.backend = c("none", "multicore", "snow", "snow(SOCK)", "snow(MPI)",
                                                                    "snow(NWS)")[1],
                                               parallel.threads = "auto",
                                               suppress.warnings = FALSE,
                                               return.data.table = FALSE
  ) 
  
  # Converts treatment episode to data table
  my_treat_episode<-as.data.table(my_treat_episode)
  # Merges with study population to get entry and exit dates (study population has been loaded in the wrapper script)
  my_treat_episode1<-as.data.table(merge(my_treat_episode, study_population[,c("person_id", "entry_date","exit_date")], by = "person_id"))
  # Converts date values to IDate format
  my_treat_episode1[,episode.start:= as.IDate(episode.start,"%Y%m%d")][,episode.end:= as.IDate(episode.end,"%Y%m%d")]
  my_treat_episode1[,entry_date:= as.IDate(entry_date,"%Y%m%d")][,exit_date := as.IDate(exit_date,"%Y%m%d")]
  # Exclude rows where episode.end is before entry.date-90
  # Therefore, keep records that have a episode.start < entry.date, unless the above exclusion criterion is met  
  my_treat_episode1<-my_treat_episode1[episode.end > entry_date - 90,]
  #  IF (episode.end > exit.date) {episode.end<-exit.date}
  my_treat_episode1<-my_treat_episode1[episode.end > exit_date, episode.end:= exit_date]
  #  IF (episode.start >= exit.date) EXCLUDE row
  my_treat_episode1<-my_treat_episode1[episode.start < exit_date,]
  # Episode end must be > than episode.start
  my_treat_episode1<-my_treat_episode1[episode.end > episode.start,]
  # Add column for ATC code 
  my_treat_episode1[,ATC:=ATC_code]
  # Remove unnecessary columns
  my_treat_episode1[,entry_date:=NULL][,exit_date:=NULL]

  # Saves files (only if df is not empty)
  if (nrow(my_treat_episode1)>0){
    saveRDS(my_treat_episode1, (paste0(g_intermediate, "treatment_episodes_alt_meds/", pop_prefix, "_", my_name[i],"_CMA_treatment_episodes.rds")))
    saveRDS(summary(my_treat_episode1), (paste0(g_intermediate, "treatment_episodes_alt_meds/", pop_prefix, "_", my_name[i],"_summary_treatment_episodes.rds")))
  }
}

# Binds all retinoid/valproate treatment episodes to one
# Gets a list of all files in treatment episode folder 
my_files<-list.files(paste0(g_intermediate, "treatment_episodes_alt_meds/"), pattern="CMA")
my_files<- my_files[!grepl("ALL_AltMed_CMA_treatment_episodes", my_files)]
if(pop_prefix == "PC"){my_files<-my_files[!grepl("PC_HOSP", my_files)]}
if(pop_prefix == "PC_HOSP"){my_files<-my_files[grepl("PC_HOSP", my_files)]}

# Checks if any Retinoid treatment episodes in list
if(length(my_files)>0){
  # Loads files
  all_altmed_list<-lapply(paste0(g_intermediate, "treatment_episodes_alt_meds/", my_files), readRDS)
  # Binds files 
  all_altmed_episodes<-rbindlist(all_altmed_list)
  # Saves files: result is RAM treatment episodes folder 
  if(nrow(all_altmed_episodes>0)){saveRDS(all_altmed_episodes, (paste0(g_intermediate, "treatment_episodes_alt_meds/", pop_prefix, "_AltMed_CMA_treatment_episodes.rds")))}
}

# Clean up 
rm(list = grep("^all_|^my_treat|my_files|my_name|^cma|altmeds_all|^split", ls(), value = TRUE))
