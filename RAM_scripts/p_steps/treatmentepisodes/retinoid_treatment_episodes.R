#Author: Ema Alsina MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 18/12/2021

# Script Function:  
# Creates retinoid treatment episodes: 
## 1. Loads retinoid meds data created by running monthly_counts_ATC.R (per region/subpop for BIFAP)
## 2. Applies duration value as defined by DAP in DAP_specific_assumed_durations.R
## 3. Creates data where each row is a treatment episode - created for each ATC code and later joined into one data frame, with column for ATC code each row pertains to. 

# Input: 
## Retinoid meds data created by monthly_counts_ATC.R

# Criteria for what treatment episodes are kept or discarded 
## Treatment episode end has to be after study entry date - 90 
## If treatment episode end is after exit study date, then change treatment episode end to exit study date 
## If treatment episode start is after exit study date, then exclude episode. 
## Treatment episode end has to come after treatment episode start

########################################################################################################################
########################################################################################################################
########################################################################################################################

# Read in retinoid meds data
retinoid_meds<-as.data.table(readRDS(paste0(medications_pop, pop_prefix, "_Retinoid_MEDS.rds")))
# Rename ATC column 
setnames(retinoid_meds,"Code","ATC.retinoid")
# Get levels of ATC codes 
my_name<-levels(factor(retinoid_meds$ATC.retinoid))
# Split data according to these levels 
split_data<-split(retinoid_meds, retinoid_meds$ATC.retinoid)


# Create treatment episode for each ATC code
for (i in 1:length(split_data)){
  # Get data for current ATC code i
  cma_data<-split_data[[i]]
  # Get ATC code
  ATC_code<-unique(cma_data[,ATC.retinoid])
  # Add durations to the data where duration is missing (according to DAP definitions if DAP_specific_DOT = TRUE)
  if(DAP_specific_DOT==T){source(paste0(pre_dir, "parameters/DAP_specific_assumed_durations.R"))}else{cma_data[,assumed_duration:=30]}
  # Change date format
  cma_data$Date<-as.Date(cma_data$Date, format="%Y%m%d")
  # Create treatment episodes
  treat_episode<-compute.treatment.episodes(data= cma_data,
                                            ID.colname = "person_id",
                                            event.date.colname = "Date",
                                            event.duration.colname = "assumed_duration",
                                            event.daily.dose.colname = NA,
                                            medication.class.colname = "ATC.retinoid",
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
  
  # Convert treatment episode to data table
  treat_episode<-as.data.table(treat_episode)
  # Convert date values to IDate format
  treat_episode[,episode.start:= as.IDate(episode.start,"%Y%m%d")][,episode.end:= as.IDate(episode.end,"%Y%m%d")]
  # Merge with study population to get entry and exit dates (study population has been loaded in the wrapper script)
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
  treat_episode1[,ATC.retinoid:=ATC_code]

  # Saves files (only if df is not empty)
  if (nrow(treat_episode1)>0){
    saveRDS(treat_episode1, (paste0(retinoid_treatment_episodes, pop_prefix, "_", my_name[i],"_CMA_retinoid_treatment_episodes.rds")))
    saveRDS(summary(treat_episode1), (paste0(retinoid_treatment_episodes, pop_prefix, "_", my_name[i],"_summary_retinoid_treatment_episodes.rds")))
  }
}

# Bind all retinoid treatment episodes into one data frame
## Get list of all retinoid files in treatment episodes folder 
retinoid_episode_files<-list.files(retinoid_treatment_episodes, pattern="CMA")
# Filter for subpop
if(pop_prefix == "PC"){retinoid_episode_files<-retinoid_episode_files[!grepl("PC_HOSP", retinoid_episode_files)]}
if(pop_prefix == "PC_HOSP"){retinoid_episode_files<-retinoid_episode_files[grepl("PC_HOSP", retinoid_episode_files)]}
## Filter for retinoid files only
retinoid_episode_files<-retinoid_episode_files[grepl(c("D05BB02|D11AH04|D10BA01"), retinoid_episode_files)]

# If any retinoids in the list: 
if(length(retinoid_episode_files)>0){
  # Binds all files into one
  retinoid_episodes<-rbindlist(lapply(paste0(retinoid_treatment_episodes, retinoid_episode_files), readRDS))
  # Saves back in treatment episode folder - result is one data frame with treatment episodes for all retinoid ATC's 
  if(nrow(retinoid_episodes>0)){saveRDS(retinoid_episodes, (paste0(retinoid_treatment_episodes, pop_prefix, "_Retinoid_CMA_treatment_episodes.rds")))}
}


# Clean up 
rm(list= grep("^actual|^cma|^split|^treat|retinoid_episode", ls(), value = TRUE))
