# Retinoid Study Population Counts
# Taken from retinoid treatment episodes
# Person is counted in month-year if he contributes at least once in that month-year. 
# If a person has 2 different Retinoid prescriptions in a month-year, he is still counted only once. 
# Loads RAM treatment episodes
retinoid_episodes<-readRDS(paste0(retinoid_treatment_episodes, pop_prefix, "_Retinoid_CMA_treatment_episodes.rds"))
# Change columns to correct data type/add column that indicates rownumber
retinoid_episodes[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)][,entry_date:=as.IDate(entry_date)][,exit_date:=as.IDate(exit_date)]
# Add row numbers to each row 
retinoid_episodes[,rowID:=.I]
# Expands data to get every day of treatment per patient (will also be used to add age_groups)
retinoid_episodes_expanded<-setDT(retinoid_episodes)[,list(idnum = person_id, episode.day = seq(episode.start, episode.end, by = "day")), by = "rowID"]
# Merges back with original data to get all columns 
retinoid_episodes_expanded<- merge(retinoid_episodes, retinoid_episodes_expanded, by = "rowID")
# Create year-months columns based on episode.day
retinoid_episodes_expanded[,year:=year(episode.day)][,month:=month(episode.day)]
# Removes any records where episode.day falls outside of entry & exit dates
retinoid_episodes_expanded<-retinoid_episodes_expanded[episode.day>=entry_date & episode.day<=exit_date,]
# Create year-months columns based on episode.day
retinoid_episodes_expanded[,year:=year(episode.day)][,month:=month(episode.day)]
# Keep 1 person per month-year regardless of ATC code 
retinoid_episodes_per_month<-unique(retinoid_episodes_expanded,by=c("person_id","year","month"))
# Perform counts per month
retinoid_denominator<-retinoid_episodes_per_month[,.N, by = .(year,month)]
# Adjust for PHARMO
if(is_PHARMO){retinoid_denominator<-retinoid_denominator[year < 2020,]} else {retinoid_denominator<-retinoid_denominator[year < 2021,]}
# Create YM variable 
retinoid_denominator<-within(retinoid_denominator,YM<-sprintf("%d-%02d",year,month))
# Rearrange columns
retinoid_denominator<-retinoid_denominator[,c("YM","N")]
# Rename column
setnames(retinoid_denominator,"N","Freq")