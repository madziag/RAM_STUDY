#Author: Ema Alsina MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 18/12/2021

# This script does two things:
# 1. it loads retinoid concept set data sets created by running "to_run_source_pop_counts.R", for separate subpopulations and regions if necessary.
# It then applys createDOT or a fixed duration value to create an estimated end date of treatment for every record
# 2. It creates a new data frame where each row is not a record, but instead a treatment episode.

#INPUTS 
#Retinoid.rds

# Reads in Retinoid medication data
retinoid_meds<-readRDS(paste0(medications_pop, pop_prefix, "_Retinoid_MEDS.rds"))
my_name<-levels(factor(retinoid_meds$Code))
split_data<-split(retinoid_meds, retinoid_meds$Code)

# Loops over each retinoid ATC codes -> creates treatment episodes for each unique code 
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
                                               #change between retinoids counts as a new treatment episode
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
  # Converts date values to IDate format
  treat_episode[,episode.start:= as.IDate(episode.start,"%Y%m%d")][,episode.end:= as.IDate(episode.end,"%Y%m%d")]
  # Merges with study population to get entry and exit dates (study population has been loaded in the wrapper script)
  treat_episode1<-as.data.table(merge(treat_episode, retinoid_study_population[,c("person_id", "entry_date","exit_date")], by = "person_id"))
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
  # Remove unnecessary rows
  treat_episode1[,entry_date:=NULL][,exit_date:=NULL]
  
  # Saves files (only if df is not empty)
  if (nrow(treat_episode1)>0){
    saveRDS(treat_episode1, (paste0(retinoid_treatment_episodes, pop_prefix, "_", my_name[i],"_CMA_retinoid_treatment_episodes.rds")))
    saveRDS(summary(treat_episode1), (paste0(retinoid_treatment_episodes, pop_prefix, "_", my_name[i],"_summary_retinoid_treatment_episodes.rds")))
  }
}

# Binds all retinoidtreatment episodes to one
# Gets a list of all files in treatment episode folder 
retinoid_episode_files<-list.files(retinoid_treatment_episodes, pattern="CMA")
if(pop_prefix == "PC"){retinoid_episode_files<-retinoid_episode_files[!grepl("PC_HOSP", retinoid_episode_files)]}
if(pop_prefix == "PC_HOSP"){retinoid_episode_files<-retinoid_episode_files[grepl("PC_HOSP", retinoid_episode_files)]}

## Retinoid files 
retinoid_episode_files<-retinoid_episode_files[grepl(c("D05BB02|D11AH04|D10BA01"), retinoid_episode_files)]

# Checks if any Retinoid treatment episodes in list
if(length(retinoid_episode_files)>0){
  # Binds all retinoid files 
  retinoid_episodes<-rbindlist(lapply(paste0(retinoid_treatment_episodes, retinoid_episode_files), readRDS))
  # Saves files: result is Retinoid treatment episodes (for all retinoid ATC codes - D05BB02|D11AH04|D10BA01)
  if(nrow(retinoid_episodes>0)){saveRDS(retinoid_episodes, (paste0(retinoid_treatment_episodes, pop_prefix, "_Retinoid_CMA_treatment_episodes.rds")))}
}

# Clean up 
rm(list= grep("^actual|^cma|^split|^treat|retinoid_episode|retinoid_meds", ls(), value = TRUE))



