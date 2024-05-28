#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 29/04/2024

# Script Function:  
# Creates RAM treatment episodes: 
## 1. Loads RAM meds data created by running monthly_counts_ATC.R (per region/subpop for BIFAP)
## 2. Applies duration value as defined by DAP in DAP_specific_assumed_durations.R
## 3. Creates data where each row is a treatment episode - created for each ATC code and later joined into one data frame, with column for ATC code each row pertains to. 

# Input: 
## RAM meds data created by monthly_counts_ATC.R

# Criteria for what treatment episodes are kept or discarded 
## Treatment episode end has to be after study entry date - 90 
## If treatment episode end is after exit study date, then change treatment episode end to exit study date 
## If treatment episode start is after exit study date, then exclude episode. 
## Treatment episode end has to come after treatment episode start
########################################################################################################################
########################################################################################################################
########################################################################################################################
# Read in RAM meds data
RAM_meds<-as.data.table(do.call(rbind,lapply(paste0(medications_pop,list.files(medications_pop, pattern=paste0(pop_prefix, "_altmed"))), readRDS)))
# Rename ATC column 
setnames(RAM_meds,"Code","ATC.RAM")

# Keep RAM meds that occur in Retinoid users only, and only if RAM prescriptions occur after Retinoid incidence use
# Read in retinoid incidence data
retinoid_prevalence_data<-as.data.table(readRDS(paste0(retinoid_counts_dfs, pop_prefix,"_Retinoid_prevalence_data.rds")))
# Rename episode start column
setnames(retinoid_prevalence_data, old=c("episode.day","episode.start"), new=c("episode.day.retinoid", "episode.start.retinoid")) 
# Get first retinoid use in entry into study (could be incidence or prevalent use)
retinoid_prevalence_data<-unique(retinoid_prevalence_data, by=c("person_id"))
# Merge retinoid prevalence data with RAM meds
# Merge these incident retinoid treatment episodes with RAM episodes so that we have both Retinoid and RAM dates per row
RAM_meds<-merge(retinoid_prevalence_data[,c("person_id", "episode.start.retinoid", "episode.day.retinoid")],RAM_meds,by="person_id")
# To make treatment episodes, we use all the RAM's regardless of whether they appear before Retinoid or not. 
# Get levels of ATC codes 
my_name<-levels(factor(RAM_meds$ATC.RAM))
# Split data according to these levels 
split_data<-split(RAM_meds, RAM_meds$ATC.RAM)

# Loops over each RAM ATC codes -> creates treatment episodes for each unique code 
for (i in 1:length(split_data)){
  # Create treatment episode for each ATC code
  cma_data<-split_data[[i]]
  # Get ATC code
  ATC_code<-unique(cma_data$ATC.RAM)
  # Add durations to the data where duration is missing (according to DAP definitions if DAP_specific_DOT = TRUE)
  if(DAP_specific_DOT==T){source(paste0(pre_dir, "parameters/DAP_specific_assumed_durations.R"))}else{cma_data[,assumed_duration:=30]}
  # Change date format
  cma_data$Date<-as.Date(cma_data$Date, format="%Y%m%d")
  # Creates treatment episodes
  treat_episode<-compute.treatment.episodes(data= cma_data,
                                            ID.colname = "person_id",
                                            event.date.colname = "Date",
                                            event.duration.colname = "assumed_duration",
                                            event.daily.dose.colname = NA,
                                            medication.class.colname = "ATC.RAM",
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
  # Convert date values to IDate format
  treat_episode[,episode.start:= as.IDate(episode.start,"%Y%m%d")][,episode.end:= as.IDate(episode.end,"%Y%m%d")]
  # Merges with study population to get entry and exit dates (study population has been loaded in the wrapper script)
  treat_episode1<-as.data.table(merge(treat_episode, retinoid_study_population[,c("person_id","birth_date" ,"entry_date","exit_date")], by = "person_id"))
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
  treat_episode1[,ATC.RAM:=ATC_code]

  # Saves files (only if df is not empty)
  if (nrow(treat_episode1)>0){
    saveRDS(treat_episode1, (paste0(RAM_treatment_episodes, pop_prefix, "_", my_name[i],"_CMA_RAM_treatment_episodes.rds")))
    saveRDS(summary(treat_episode1), (paste0(RAM_treatment_episodes, pop_prefix, "_", my_name[i],"_summary_RAM_treatment_episodes.rds")))
  }
}

# Bind all retinoid treatment episodes into one data frame
## Get list of all retinoid files in treatment episodes folder 
RAM_episode_files<-list.files(RAM_treatment_episodes, pattern="CMA")
# Filter for subpop
if(pop_prefix == "PC"){RAM_episode_files<-RAM_episode_files[!grepl("PC_HOSP", RAM_episode_files)]}
if(pop_prefix == "PC_HOSP"){RAM_episode_files<-RAM_episode_files[grepl("PC_HOSP", RAM_episode_files)]}

# If any RAMs in the list: 
if(length(RAM_episode_files)>0){
  # Binds all RAM files 
  RAM_episodes<-rbindlist(lapply(paste0(RAM_treatment_episodes, RAM_episode_files), readRDS))
  # Saves back in treatment episode folder - result is one data frame with treatment episodes for all RAM ATC's 
  if(nrow(RAM_episodes>0)){saveRDS(RAM_episodes, (paste0(RAM_treatment_episodes, pop_prefix, "_RAM_CMA_treatment_episodes.rds")))}
}

#########################
####### Flowchart #######
#########################
# Counts of all RAM Meds in retinoid users 
# First get all RAMs only after first retinoid exposure

RAM_meds_after_retinoid<-RAM_meds[Date>=episode.day.retinoid,]
RAM_flowchart_allRAM_users<-nrow(unique(RAM_meds_after_retinoid,by="person_id"))
RAM_flowchart_allRAM_records<-nrow(RAM_meds_after_retinoid)
#########################
####### Flowchart #######
#########################

# Clean up 
rm(list= grep("^cma|^split|^treat|RAM_episodes|RAM_meds_", ls(), value = TRUE))