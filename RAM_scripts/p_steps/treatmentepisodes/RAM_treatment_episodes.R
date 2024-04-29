#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 29/04/2024

# This script does two things:
# 1. it loads RAM created by running "to_run_source_pop_counts.R", for separate subpopulations and regions if necessary.
# It finds which RAM's where used by Retinoid users (study population is Retinoid users of WOCBP)
# It then applys createDOT or a fixed duration value to create an estimated end date of treatment for every record
# 2. It creates a new data frame where each row is not a record, but instead a treatment episode.


# Get RAM records for retinoid user population 
# Looks for RAM files in medications folder 
# Reads in Retinoid medication data
RAM_meds_all<-as.data.table(do.call(rbind,lapply(paste0(medications_pop, list.files(medications_pop, pattern=paste0(pop_prefix, "_altmed"))), readRDS)))
# Get RAM meds in Retinoid users 
RAM_meds_in_retinoid_users<-merge(retinoid_study_population[,"person_id"], RAM_meds_all, by="person_id")
my_name<-levels(factor(RAM_meds_in_retinoid_users$Code))
split_data<-split(RAM_meds_in_retinoid_users, RAM_meds_in_retinoid_users$Code)

# Loops over each RAM ATC codes -> creates treatment episodes for each unique code 
for (i in 1:length(split_data)){
  
  cma_data<-split_data[[i]]
  # Get ATC code
  ATC_code<-unique(cma_data$Code)
  # assumed duration column values are assigned dependent on user input DAP_specific_DOT
  if(DAP_specific_DOT==T){source(paste0(pre_dir, "parameters/DAP_specific_assumed_durations.R"))}else{cma_data[,assumed_duration:=30]}
  
  cma_data$Date<-as.Date(cma_data$Date, format="%Y%m%d")
  # Creates treatment episodes
  treat_episode<-compute.treatment.episodes(data= cma_data,
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
  treat_episode<-as.data.table(treat_episode)
  # Merges with study population to get entry and exit dates (study population has been loaded in the wrapper script)
  treat_episode1<-as.data.table(merge(treat_episode, retinoid_study_population[,c("person_id", "entry_date","exit_date")], by = "person_id"))
  # Converts date values to IDate format
  treat_episode1[,episode.start:= as.IDate(episode.start,"%Y%m%d")][,episode.end:= as.IDate(episode.end,"%Y%m%d")]
  treat_episode1[,entry_date:= as.IDate(entry_date,"%Y%m%d")][,exit_date := as.IDate(exit_date,"%Y%m%d")]
  # Exclude rows where episode.end is before entry.date-90
  # Therefore, keep records that have a episode.start < entry.date, unless the above exclusion criterion is met  
  treat_episode1<-treat_episode1[episode.end > entry_date - 90,]
  #  IF (episode.end > exit.date) {episode.end<-exit.date}
  treat_episode1<-treat_episode1[episode.end > exit_date, episode.end:= exit_date]
  #  IF (episode.start >= exit.date) EXCLUDE row
  treat_episode1<-treat_episode1[episode.start < exit_date,]
  # Episode end must be > than episode.start
  treat_episode1<-treat_episode1[episode.end > episode.start,]
  # Add column for ATC code 
  treat_episode1[,ATC:=ATC_code]
  # Remove unnecessary columns
  treat_episode1[,entry_date:=NULL][,exit_date:=NULL]

  # Saves files (only if df is not empty)
  if (nrow(treat_episode1)>0){
    saveRDS(treat_episode1, (paste0(RAM_treatment_episodes, pop_prefix, "_", my_name[i],"_CMA_RAM_treatment_episodes.rds")))
    saveRDS(summary(treat_episode1), (paste0(RAM_treatment_episodes, pop_prefix, "_", my_name[i],"_summary_RAM_treatment_episodes.rds")))
  }
}

# Binds all RAM treatment episodes to one
# Gets a list of all files in treatment episode folder 
RAM_episode_files<-list.files(RAM_treatment_episodes, pattern="CMA")
if(pop_prefix == "PC"){RAM_episode_files<-RAM_episode_files[!grepl("PC_HOSP", RAM_episode_files)]}
if(pop_prefix == "PC_HOSP"){RAM_episode_files<-RAM_episode_files[grepl("PC_HOSP", RAM_episode_files)]}

# Checks if any Retinoid treatment episodes in list
if(length(RAM_episode_files)>0){
  # Binds all RAM files 
  RAM_episodes<-rbindlist(lapply(paste0(RAM_treatment_episodes, RAM_episode_files), readRDS))
  # Saves files: result is RAM treatment episodes folder 
  if(nrow(RAM_episodes>0)){saveRDS(RAM_episodes, (paste0(RAM_treatment_episodes, pop_prefix, "_RAM_CMA_treatment_episodes.rds")))}
}


# Clean up 
rm(list= grep("^cma|^split|^treat|RAM_meds_all|RAM_episodes|RAM_meds_", ls(), value = TRUE))