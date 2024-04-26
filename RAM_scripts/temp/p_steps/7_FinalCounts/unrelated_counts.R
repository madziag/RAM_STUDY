### Alternative Medications ###
# 1. Load Alternative Meds Treatment Episodes 
# Looks for treatment_episode files in treatment_episodes folder (actual files will be loaded in the for loop)
tx_episode_files_altmed<-list.files(paste0(g_intermediate, "treatment_episodes_alt_meds/"), pattern = paste0(pop_prefix, "_AltMed_CMA"), ignore.case = T)
# Filters by current subpopulation 
tx_episode_files_altmed<-tx_episode_files_altmed[grepl(pop_prefix, tx_episode_files_altmed)]
if(populations[pop] == "PC_study_population.rds"){tx_episode_files_altmed<-tx_episode_files_altmed[!grepl("PC_HOSP", tx_episode_files_altmed)]}
# Reads in file
df_episodes_altmeds<-as.data.table(readRDS(paste0(g_intermediate,"treatment_episodes_alt_meds/", pop_prefix, "_AltMed_CMA_treatment_episodes.rds")))
############################ TESTING ######################################
# df_episodes_altmeds[order(person_id, ATC, episode.start)]
# df_episodes_altmeds[, next.episode.start:= shift(episode.start, type = "lead" ), by = c("person_id", "ATC")]
# df_episodes_altmeds[,diff:=next.episode.start - episode.end]
############################ TESTING ######################################
# Is the treatment episode within entry and exit dates 
# Merge with study population to get entry and exit dates 
df_episodes_altmeds<-merge(df_episodes_altmeds, study_population[,c("person_id", "birth_date", "entry_date","exit_date")], by = "person_id")
# Changes columns to correct data type/add column that indicates rownumber
df_episodes_altmeds[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)][,entry_date:=as.IDate(entry_date)][,exit_date:=as.IDate(exit_date)]
# Removes duplicates if any
df_episodes_altmeds<-df_episodes_altmeds[!duplicated(df_episodes_altmeds)]
# Removes all episode ends that fall outside entry_date and exit_date
df_episodes_altmeds<-df_episodes_altmeds[episode.end>entry_date & episode.end<=exit_date,]
# Rename columns
setnames(df_episodes_altmeds, "episode.ID", "episode.ID.altmed")
setnames(df_episodes_altmeds, "episode.start", "episode.start.altmed")
setnames(df_episodes_altmeds, "end.episode.gap.days", "end.episode.gap.days.altmed")
setnames(df_episodes_altmeds, "episode.duration", "episode.duration.altmed")
setnames(df_episodes_altmeds, "episode.end", "episode.end.altmed")
setnames(df_episodes_altmeds, "ATC", "ATC.altmed")
# Drop columns you dont need
df_episodes_altmeds[, entry_date:=NULL][,exit_date:=NULL]

### Retinoids ###
### Alternative Medications ###
# Looks for treatment_episode files in treatment_episodes folder (actual files will be loaded in the for loop)
tx_episode_files_retinoid<-list.files(paste0(g_intermediate, "treatment_episodes/"), pattern = "Retinoid_CMA", ignore.case = T)
# Filters by current subpopulation 
tx_episode_files_retinoid<-tx_episode_files_retinoid[grepl(pop_prefix, tx_episode_files_retinoid)]
if(populations[pop] == "PC_study_population.rds"){tx_episode_files_retinoid<-tx_episode_files_retinoid[!grepl("PC_HOSP", tx_episode_files_retinoid)]}
# Reads in file
df_episodes_retinoid<-as.data.table(readRDS(paste0(g_intermediate,"treatment_episodes/", pop_prefix, "_Retinoid_CMA_treatment_episodes.rds")))
# Is the treatment episode within entry and exit dates 
# Merge with study population to get entry and exit dates 
df_episodes_retinoid<-merge(df_episodes_retinoid, study_population[,c("person_id", "birth_date", "entry_date","exit_date")], by = "person_id")
# Changes columns to correct data type/add column that indicates rownumber
df_episodes_retinoid[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)][,entry_date:=as.IDate(entry_date)][,exit_date:=as.IDate(exit_date)]
# Removes duplicates if any
df_episodes_retinoid<-df_episodes_retinoid[!duplicated(df_episodes_retinoid)]
# Removes all episode ends that fall outside entry_date and exit_date
df_episodes_retinoid<-df_episodes_retinoid[episode.end>entry_date & episode.end<=exit_date,]
# Rename columns
setnames(df_episodes_retinoid, "episode.ID", "episode.ID.retinoid")
setnames(df_episodes_retinoid, "episode.start", "episode.start.retinoid")
setnames(df_episodes_retinoid, "end.episode.gap.days", "end.episode.gap.days.retinoid")
setnames(df_episodes_retinoid, "episode.duration", "episode.duration.retinoid")
setnames(df_episodes_retinoid, "episode.end", "episode.end.retinoid")
setnames(df_episodes_retinoid, "ATC", "ATC.retinoid")
# Drop columns you dont need
df_episodes_retinoid[, entry_date:=NULL][,exit_date:=NULL][,birth_date:=NULL]

# Create column next.episode.start.retinoid
df_episodes_retinoid[order(person_id, ATC.retinoid, episode.start.retinoid)]
df_episodes_retinoid[, next.episode.start.retinoid:= shift(episode.start.retinoid, type = "lead" ), by = c("person_id", "ATC.retinoid")]

### Merge altmed and retinoid treatment episodes on personid
df_episodes_all <- merge(df_episodes_retinoid, df_episodes_altmeds, by="person_id")
setcolorder(df_episodes_all, c("person_id", "episode.start.retinoid", "episode.end.retinoid", "episode.start.altmed", "episode.end.altmed"))

#### TESTING DATA FRAME #####
library(readxl)
TestData <- read_excel("C:/Users/mgamb/Desktop/TestData.xlsx", 
                       col_types = c("text", "date", "date", 
                                     "date", "date", "numeric", "numeric", 
                                     "numeric", "text", "date", "numeric", 
                                     "numeric", "numeric", "text", "date"))

#### TESTING DATA FRAME #####
df_episodes_all <-as.data.table(TestData)
df_episodes_all <-as.data.table(TestData)
df_episodes_all[,episode.start.retinoid:=as.IDate(episode.start.retinoid)]
df_episodes_all[,episode.end.retinoid:=as.IDate(episode.end.retinoid)]
df_episodes_all[,next.episode.start.retinoid :=as.IDate(next.episode.start.retinoid)]
df_episodes_all[,episode.start.altmed:=as.IDate(episode.start.altmed)]
df_episodes_all[,episode.end.altmed:=as.IDate(episode.end.altmed)]

###  Unrelated conditions
# treatment initiation of a RAM ≥90 days after the oral retinoid end date
df_episodes_all[,unrelated:=ifelse(episode.start.altmed-episode.end.retinoid >=90, 1, 0)]

# Keep only concomitant users
df_episodes_unrelated<- df_episodes_all[unrelated==1,]   


