#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 29/04/2024

##################################################################################################################################################
############################################ Retinoid Prevalence, Incidence & Discontinuation ####################################################
##################################################################################################################################################

## Prevalent (current) Retinoid use (monthly)
# Numerator -> Number of female subjects in cohort with Retinoid episode overlapping the month by at least 1 day
# Denominator -> Total number of female subjects in cohort for at least 1 day in the month (denominator file)
# Records needed -> 1. Retinoid treatment episode files 2. Denominator file

## Incident (starters) Retinoid use (monthly) 
# Numerator -> Number of female subjects in cohort with a Retinoid episode start in the month
# Denominator -> Total number of female subjects in cohort for at least 1 day in the month (denominator file)
# Records needed -> 1. Retinoid treatment episode files 2. Denominator files

## Discontinuation of Retinoid use (monthly)
# Numerator -> Number of female subjects in cohort who discontinue Retinoid use in the month
# *** Definition of discontinuation: 
# *** Subjects with only 1 treatment episode -> 
# ****** if exit date from study <= 90 days from treatment episode end date -> DOES NOT COUNT AS A DISCONTINUED USER 
# ****** if exit date from study > 90 days from treatment episode end date -> COUNTS AS A DISCONTINUED USER 
# *** Subjects with more than 1 treatment episode -> 
# ****** If not the last of the treatment episode series e.g. if user has 3 treatment episodes, this refers to episodes 1 & 2 -> 
# ********* if next treatment episode start <= 90 days from the previous treatment episode end -> DOES NOT COUNT AS A DISCONTINUED USER 
# ********* if next treatment episode start > 90 days from the previous treatment episode end -> COUNTS AS A DISCONTINUED USER 
# ****** If it is the last of the treatment episode series e.g. if user has 3 treatment episodes, this refers to episode 3 -> 
# ********* if exit date from study <= 90 days from treatment episode end date -> DOES NOT COUNT AS A DISCONTINUED USER 
# ********* if exit date from study > 90 days from treatment episode end date -> COUNTS AS A DISCONTINUED USER 
# Denominator ->  Number of prevalent (current) users that month 
# Records needed -> 1. Retinoid treatment episode files 2. Prevalent counts (calculated above) 

##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################
# Load denominator and empty df
source(paste0(pre_dir,"denominators/load_denominator.R"))
# Load retinoid treatment episodes
retinoid_episodes<-readRDS(paste0(retinoid_treatment_episodes, pop_prefix, "_Retinoid_CMA_treatment_episodes.rds"))
# Changes columns to correct data type/add column that indicates rownumber
retinoid_episodes[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)][,entry_date:=as.IDate(entry_date)][,exit_date:=as.IDate(exit_date)]
# Add row numbers to each row 
retinoid_episodes[,rowID:=.I]
# Creates a version of df_episodes for incidence counts (we do not need an expanded df for incidence counts)
retinoid_episodes_for_incidence<-retinoid_episodes
# Creates a version of df_episodes for discontinued counts (we do not need an expanded df for discontinued counts)
retinoid_episodes_for_discontinued<-retinoid_episodes
# Expands data to get every day of treatment per patient (will also be used to add age_groups)
retinoid_episodes_expanded<-setDT(retinoid_episodes)[,list(idnum = person_id, episode.day = seq(episode.start, episode.end, by = "day")), by = "rowID"]
# Merges back with original data to get all columns 
retinoid_episodes_expanded<- merge(retinoid_episodes, retinoid_episodes_expanded, by = "rowID")
# Create year-months columns based on episode.day
retinoid_episodes_expanded[,year:=year(episode.day)][,month:=month(episode.day)]

##################################################################################################
################################## Calculates Prevalence  ########################################
##################################################################################################
### Numerator = Number of female subjects in cohort with retinoid episode overlapping the month by at least 1 day 
# Removes duplicates - keeps only the earliest record of person_id, year, month => we get the first record of person for every month in every year
# Removes any records where episode.day falls outside of entry & exit dates
retinoid_episodes_expanded<-retinoid_episodes_expanded[episode.day>=entry_date & episode.day<=exit_date,]
# Creates data where each row represents a month of treatment within the treatment episode (patient is represented once per month)
retinoid_prevalence<-retinoid_episodes_expanded[!duplicated(retinoid_episodes_expanded[,c("person_id", "episode.start", "year", "month")])]
# Removes unnecessary columns
retinoid_prevalence<-retinoid_prevalence[,-c("rowID", "idnum")]
# Prevalence Counts
retinoid_prevalence_counts<-retinoid_prevalence[,.N, by = .(year,month)]
# Adjust for PHARMO
if(is_PHARMO){retinoid_prevalence_counts<-retinoid_prevalence_counts[year < 2020,]} else {retinoid_prevalence_counts<-retinoid_prevalence_counts[year < 2021,]}
# Merge with empty df (for counts that do not have counts for all months and years of study)
retinoid_prevalence_counts<-as.data.table(merge(x = empty_df, y = retinoid_prevalence_counts, by = c("year", "month"), all.x = TRUE))
# Fills in missing values with 0
retinoid_prevalence_counts[is.na(N), N:=0]
# Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
retinoid_prevalence_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
# Masking is not applied before stratification
retinoid_prevalence_counts[,masked:=0]
### Denominator = Total number of female subjects in the cohort for at least 1 day in the month
##### ->  denominator file has already been read in before the for loop, variable name = denominator
### Merges numerator file with denominator file
retinoid_prevalence_rates<-merge(x = retinoid_prevalence_counts, y = denominator, by = c("year", "month"), all.x = TRUE)
# Calculates rates
retinoid_prevalence_rates<-retinoid_prevalence_rates[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
# Keeps necessary columns 
retinoid_prevalence_rates<-retinoid_prevalence_rates[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
# Saves files in medicine counts folder
saveRDS(retinoid_prevalence_rates, paste0(medicines_counts_dir, "/", pop_prefix, "_Retinoid_prevalence_counts.rds"))
# Saves df in counts_df folder 
saveRDS(retinoid_prevalence, paste0(retinoid_counts_dfs, pop_prefix, "_Retinoid_prevalence_data.rds"))


##################################################################################################
################################## Calculates Incidence ##########################################
##################################################################################################
### Numerator = Number of female subjects in cohort with a retinoid episode start in the month 
# Deduplicate df_episodes_expanded to only include records patients with the first start date 
retinoid_incidence<-retinoid_episodes_for_incidence[!duplicated(retinoid_episodes_for_incidence)]
# Order df by date, then keep only the first treatment episode for each patient id
retinoid_incidence<-retinoid_incidence[order(person_id, episode.start)]
retinoid_incidence<-retinoid_incidence[, head(.SD, 1), by = "person_id"]
# Removes all episode starts that fall outside entry_date and exit_date
retinoid_incidence<-retinoid_incidence[episode.start>=entry_date & episode.start<=exit_date,]
# Creates year and month columns
retinoid_incidence[,year:=year(episode.start)][,month:=month(episode.start)]
# Removes unnecessary columns
retinoid_incidence<-retinoid_incidence[,-c("rowID")]
# Incidence Counts
retinoid_incidence_counts<-retinoid_incidence[,.N, by = .(year,month)]
# Adjust for PHARMO
if(is_PHARMO){retinoid_incidence_counts<-retinoid_incidence_counts[year < 2020,]} else {retinoid_incidence_counts<-retinoid_incidence_counts[year < 2021,]}
# Merge with empty df (for counts that do not have counts for all months and years of study)
retinoid_incidence_counts<-as.data.table(merge(x = empty_df, y = retinoid_incidence_counts, by = c("year", "month"), all.x = TRUE))
# Fills in missing values with 0
retinoid_incidence_counts[is.na(N), N:=0]
# Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
retinoid_incidence_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
# Masking is not applied before stratification
retinoid_incidence_counts[,masked:=0]
### Denominator = Total number of female subjects in the cohort for at least 1 day in the month
##### ->  denominator file has already been read in before the for loop, variable name = denominator
### Merges numerator file with denominator file
retinoid_incidence_rates<-merge(x = retinoid_incidence_counts, y = denominator, by = c("year", "month"), all.x = TRUE)
# Calculates rates
retinoid_incidence_rates<-retinoid_incidence_rates[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
# Keeps necessary columns 
retinoid_incidence_rates<-retinoid_incidence_rates[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
# Saves files in medicine counts folder
saveRDS(retinoid_incidence_rates, paste0(medicines_counts_dir, "/", pop_prefix, "_Retinoid_incidence_counts.rds"))
# Saves incidence dfs
saveRDS(retinoid_incidence, paste0(retinoid_counts_dfs, pop_prefix, "_Retinoid_incidence_data.rds"))


##################################################################################################
############################# Calculates Discontinuation  ########################################
##################################################################################################
### Numerator = Number of female subjects in cohort who discontinue retinoid in the month
retinoid_discontinued<-retinoid_episodes_for_discontinued[!duplicated(retinoid_episodes_for_discontinued)]
# Order by person id, episode start
retinoid_discontinued<-retinoid_discontinued[order(person_id, episode.start)]
# Create new column with the date of the start of the next episode (per patient) if any. If none exists, this becomes an NA
retinoid_discontinued[, next.episode.start:= shift(episode.start, type = "lead" ), by = person_id]
# Exclude records where exit_date (from study) falls within 90 days after episode.end -> this is not considered a discontinuation
retinoid_discontinued<-retinoid_discontinued[exit_date - episode.end > 90,]
# Any records left with NA in the next.episode.start column are now true discontinue-rs and col discontinued is assigned the value 1
retinoid_discontinued<-retinoid_discontinued[is.na(next.episode.start), discontinued := 1]
# Exclude records where next.episode.start (if any) falls within 90 days after episode.end
# If difference between next.episode.start and episode end > 90 then this is a true discontinuer and col discontinued is assigned the value 1
# if the difference is less than 90 then it is assigned the value 0
### discontinuation window is set in 3_to_run_final_counts
retinoid_discontinued<-retinoid_discontinued[!is.na(next.episode.start), discontinued := ifelse(next.episode.start-episode.end > discontinuation_window, 1, 0)]
# Get only rows where the value of discontinued == 1 (true discontinue-rs)
retinoid_discontinued<-retinoid_discontinued[discontinued == 1,]
# Removes all episode ends that fall outside entry_date and exit_date
retinoid_discontinued<-retinoid_discontinued[episode.end>entry_date & episode.end<=exit_date,]
# Creates year and month columns
retinoid_discontinued[,year:=year(episode.end)][,month:=month(episode.end)]
# Removes unnecessary columns
retinoid_discontinued<-retinoid_discontinued[,-c("rowID", "next.episode.start", "discontinued")]
# Performs discontinued counts 
retinoid_discontinued_counts<-retinoid_discontinued[,.N, by = .(year,month)]
# Adjust for PHARMO
if(is_PHARMO){retinoid_discontinued_counts<-retinoid_discontinued_counts[year < 2020,]} else {retinoid_discontinued_counts<-retinoid_discontinued_counts[year < 2021,]}
# Merge with empty df (for counts that do not have counts for all months and years of study)
retinoid_discontinued_counts<-as.data.table(merge(x = empty_df, y = retinoid_discontinued_counts, by = c("year", "month"), all.x = TRUE))
# Fills in missing values with 0
retinoid_discontinued_counts[is.na(N), N:=0]
# Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
retinoid_discontinued_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
# Masking is not applied before stratification
retinoid_discontinued_counts[,masked:=0]
# Create YM variable 
retinoid_discontinued_counts<-within(retinoid_discontinued_counts, YM<- sprintf("%d-%02d", year, month))
### Denominator = Number of prevalent retinoid users that month
##### ->  denominator file has already been created in for loop variable name = prevalence_all_counts
# Prepare denominator 
retinoid_prevalence_counts1<-retinoid_prevalence_rates[,c("YM", "N")]
setnames(retinoid_prevalence_counts1, "N", "Freq")
### Merges numerator file with denominator file
retinoid_discontinued_rates<-merge(x = retinoid_discontinued_counts, y = retinoid_prevalence_counts1, by = c("YM"), all.x = TRUE)
# Calculates rates
retinoid_discontinued_rates<-retinoid_discontinued_rates[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*100][is.nan(rates)|is.na(rates), rates:=0]
# Keeps necessary columns 
retinoid_discontinued_rates<-retinoid_discontinued_rates[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
# Saves files in medicine counts folder
saveRDS(retinoid_discontinued_rates, paste0(medicines_counts_dir, "/", pop_prefix, "_Retinoid_discontinued_counts.rds"))
# Saves discontinued dfs
saveRDS(retinoid_discontinued, paste0(retinoid_counts_dfs, pop_prefix, "_Retinoid_discontinued_data.rds"))

# Clean up 
rm(list = grep("^retinoid_discont|^retinoid_inc|^retinoid_prev|^retinoid_episodes_|retinoid_episodes", ls(), value = TRUE))