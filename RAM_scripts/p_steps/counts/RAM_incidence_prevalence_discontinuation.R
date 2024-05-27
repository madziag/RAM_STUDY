#Author: Magdalena Gamba 
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 29/04/2024

#############################################################################################
#############################################################################################
############# Incidence, Prevalence, Discontinuation ########################################
#############################################################################################
#############################################################################################

##### Monthly prevalence rates (PR) of RAM #####
# Numerator = number of RAM users per calendar month (with at least one day of exposure that month) - User is counted only once per month-year regardless of multiple RAM codes
# denominator= subset of WOCBP with at least one retinoid prescription within study period (retinoid prescription has to fall between entry and exit study dates)
# Unit = person-month. 

##### Monthly incidence rates (IR) of RAM #####
# Numerator = number of new RAM users (no use the year prior) per calendar month (at least one day of exposure that month) - User is counted only once per month-year regardless of multiple RAM codes
# denominator= subset of WOCBP with at least one retinoid prescription within study period (retinoid prescription has to fall between entry and exit study dates)
# Unit = person-month. 

##### Monthly discontinuation rates (IR) of RAM #####
## Conditions 
# a. Not receiving any other prescription or dispensing of the same drug within 90 days after the end date of the last treatment episode - User is counted only once per month-year regardless of multiple RAM codes
# Numerator = number of subjects discontinuing RAM per calendar month
# Denominator = number of all RAM users during the same month (at least one day of exposure that month) (prevalence)
# Unit = person-month.  

#############################################################################################
################################## Data Preparation #########################################
#############################################################################################
# Loads denominator and empty df
source(paste0(pre_dir,"denominators/load_denominator.R"))

# Loads RAM treatment episodes
RAM_episodes<-readRDS(paste0(RAM_treatment_episodes, pop_prefix, "_RAM_CMA_treatment_episodes.rds"))
# Changes columns to correct data type/add column that indicates row number
RAM_episodes[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)][,entry_date:=as.IDate(entry_date)][,exit_date:=as.IDate(exit_date)][,birth_date:=as.IDate(birth_date)]
# Add row numbers to each row 
RAM_episodes[,rowID:=.I]
# Creates a version of df_episodes for incidence counts (we do not need an expanded df for incidence counts)
RAM_episodes_for_incidence<-RAM_episodes
# Creates a version of df_episodes for discontinued counts (we do not need an expanded df for discontinued counts)
RAM_episodes_for_discontinued<-RAM_episodes

##################################################################################################
################################## Calculates Prevalence #########################################
##################################################################################################
# Expands data to get every day of treatment per patient (will also be used to add age_groups)
RAM_episodes_expanded<-setDT(RAM_episodes)[,list(idnum = person_id, episode.day = seq(episode.start, episode.end, by = "day")), by = "rowID"]
# Merges back with original data to get all columns 
RAM_episodes_expanded<- merge(RAM_episodes, RAM_episodes_expanded, by = "rowID")
# Create year-months columns based on episode.day
RAM_episodes_expanded[,year:=year(episode.day)][,month:=month(episode.day)]
# Create age-groups
# Create column with patients age at episode.start
RAM_episodes_expanded[,current_age:= floor((episode.day - birth_date)*10/365.25)/10]
# We change the min of age group 12-20.99 to 11 to assign a group to those with tx days in 2009 (otherwise it would fall in a group that is not defined)
RAM_episodes_expanded[current_age >= 11 & current_age < 21, age_group:= "12-20.99"]
RAM_episodes_expanded[current_age >= 21 & current_age < 31, age_group:= "21-30.99"]
RAM_episodes_expanded[current_age >= 31 & current_age < 41, age_group:= "31-40.99"]
RAM_episodes_expanded[current_age >= 41 & current_age < 56, age_group:= "41-55.99"]

### Numerator = Number of female subjects in cohort with RAM episode overlapping the month by at least 1 day 
# Removes any records where episode.day falls outside of entry & exit dates
RAM_episodes_expanded<-RAM_episodes_expanded[episode.day>=entry_date & episode.day<=exit_date,]
# Removes duplicates - keeps only the earliest record of person_id, year, month => we get the first record of person for every month in every year
# Creates data where each row represents a month of treatment within the treatment episode (patient is represented once per month)
# Person is counted only once per year-month regardless of ATC code
RAM_prevalence<-RAM_episodes_expanded[!duplicated(RAM_episodes_expanded[,c("person_id","episode.start","year","month")])]

# Removes unnecessary columns
RAM_prevalence<-RAM_prevalence[,-c("rowID","idnum","episode.day")]
# Reorder columns
setcolorder(RAM_prevalence, c("person_id", "episode.ID" , "episode.start","end.episode.gap.days","episode.duration","episode.end","ATC.RAM","birth_date","entry_date","exit_date","current_age", "age_group","year","month"))
# Prevalence Counts
RAM_prevalence_counts<-RAM_prevalence[,.N, by = .(year,month)]
# Adjust for PHARMO
if(is_PHARMO){RAM_prevalence_counts<-RAM_prevalence_counts[year < 2020,]} else {RAM_prevalence_counts<-RAM_prevalence_counts[year < 2021,]}
# Merge with empty df (for counts that do not have counts for all months and years of study)
RAM_prevalence_counts<-as.data.table(merge(x = empty_df, y = RAM_prevalence_counts, by = c("year", "month"), all.x = TRUE))
# Fills in missing values with 0
RAM_prevalence_counts[is.na(N), N:=0]
# Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
RAM_prevalence_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
# Masking is not applied before stratification
RAM_prevalence_counts[,masked:=0]
### Denominator = Total number of female subjects in the cohort for at least 1 day in the month
##### ->  denominator file has already been read in before the for loop, variable name = denominator
### Merges numerator file with denominator file
RAM_prevalence_rates<-merge(x = RAM_prevalence_counts, y = retinoid_denominator, by = c("year", "month"), all.x = TRUE)
# Calculates rates
RAM_prevalence_rates<-RAM_prevalence_rates[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
# Keeps necessary columns 
RAM_prevalence_rates<-RAM_prevalence_rates[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
# Saves files in medicine counts folder
saveRDS(RAM_prevalence_rates, paste0(objective1_dir, "/", pop_prefix, "_RAM_prevalence_counts.rds"))
# Saves df in counts_df folder 
saveRDS(RAM_prevalence, paste0(objective1_temp_dir, pop_prefix, "_RAM_prevalence_data.rds"))
  
################ Prevalence by Age Group ###################  
# Count prevalence by age, month, year
prevalence_by_age<-RAM_prevalence[,.N, by = .(year,month, age_group)]

# for each unique age-group, create a counts df with rates 
for(group in 1:length(unique(prevalence_by_age$age_group))){
  # Create a subset of age group
  each_group<-prevalence_by_age[age_group==unique(prevalence_by_age$age_group)[group]]
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
  # Fills in missing values with 0
  each_group[is.na(N),N:=0][is.na(age_group),age_group:=unique(prevalence_by_age$age_group)[group]]
  # Adjust for PHARMO
  if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
  # Create YM variable 
  each_group<-within(each_group,YM<-sprintf("%d-%02d",year,month))

  # Prepare denominator
  prevalence_count_min <- RAM_prevalence_rates[,c("YM","N")]
  setnames(prevalence_count_min,"N","Freq")
  
  # Merge age-group subset count with all prevalent counts 
  age_group_prevalence_count<-merge(x=each_group,y=prevalence_count_min,by=c("YM"),all.x=TRUE)
  # Masking set at 0
  age_group_prevalence_count[,masked:=0]
  # If masking applies
  if(mask==T){age_group_prevalence_count[N>0&N<5,N:=5][Freq>0&Freq<5,Freq:=5][,masked:=1]}
  # Calculates rates
  age_group_prevalence_count<-age_group_prevalence_count[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates),rates:=0]
  # Drop columns you don't need 
  age_group_prevalence_count<-age_group_prevalence_count[,c("YM","N","Freq","rates","masked")]
  
  # Save files in medicine counts folder
  saveRDS(age_group_prevalence_count, (paste0(objective1_strat_dir,"/", pop_prefix,"_RAM_prevalence_counts_", unique(prevalence_by_age$age_group)[group],"_age_group.rds")))
}

##################################################################################################
################################## Calculates Incidence #########################################
##################################################################################################
### Numerator = Number of female subjects in cohort with a RAM episode start in the month 
# Deduplicate df_episodes_expanded to only include records patients with the first start date 
RAM_incidence<-RAM_episodes_for_incidence[!duplicated(RAM_episodes_for_incidence)]

# Create column with patients age at episode.start
RAM_incidence[,current_age:= floor((episode.start - birth_date)*10/365.25)/10]
# We change the min of age group 12-20.99 to 11 to assign a group to those with tx days in 2009 (otherwise it would fall in a group that is not defined)
RAM_incidence[current_age >= 11 & current_age < 21, age_group:= "12-20.99"]
RAM_incidence[current_age >= 21 & current_age < 31, age_group:= "21-30.99"]
RAM_incidence[current_age >= 31 & current_age < 41, age_group:= "31-40.99"]
RAM_incidence[current_age >= 41 & current_age < 56, age_group:= "41-55.99"]

# Order data by person id and episode start
RAM_incidence<-RAM_incidence[order(person_id, episode.start)]
# Create column previous episode end by person id
RAM_incidence<-RAM_incidence[, previous.episode.end:= shift(episode.end, type = "lag" ), by = c("person_id")]
# Incident use is when: 
## a. episode.id = 1 (patients first treatment episode)
## b. difference between the episode.start of the following episode is more than 365 days after episode end of previous episode
RAM_incidence<-RAM_incidence[, incident_user:=ifelse(episode.ID==1 | (!is.na(previous.episode.end) & episode.start-previous.episode.end>365), 1, 0)]
# Create a subset of only the incident users 
RAM_incidence<-RAM_incidence[incident_user==1,]
# Creates year and month columns
RAM_incidence[,year:=year(episode.start)][,month:=month(episode.start)]
# If episode start falls outside of patients entry and exit into study dates, then remove it
RAM_incidence<-RAM_incidence[episode.start>= entry_date & episode.start<=exit_date,]
# Removes unnecessary columns
RAM_incidence<-RAM_incidence[,-c("rowID", "previous.episode.end", "incident_user")]
# For indication counts 
RAM_incidence_per_indication<-RAM_incidence
# Remove duplicates -> Patient is counted only 1x per month-year -ATC?
RAM_incidence<-unique(RAM_incidence, by=c("person_id", "year", "month"))
# Incidence Counts
RAM_incidence_counts<-RAM_incidence[,.N, by = .(year,month)]
# Adjust for PHARMO
if(is_PHARMO){RAM_incidence_counts<-RAM_incidence_counts[year < 2020,]} else {RAM_incidence_counts<-RAM_incidence_counts[year < 2021,]}
# Merge with empty df (for counts that do not have counts for all months and years of study)
RAM_incidence_counts<-as.data.table(merge(x = empty_df, y = RAM_incidence_counts, by = c("year", "month"), all.x = TRUE))
# Fills in missing values with 0
RAM_incidence_counts[is.na(N), N:=0]
# Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
RAM_incidence_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
# Masking is not applied before stratification
RAM_incidence_counts[,masked:=0]
### Denominator = Total number of female subjects in the cohort for at least 1 day in the month
##### ->  denominator file has already been read in before the for loop, variable name = denominator
### Merges numerator file with denominator file
RAM_incidence_rates<-merge(x = RAM_incidence_counts, y = retinoid_denominator, by = c("year", "month"), all.x = TRUE)
# Calculates rates
RAM_incidence_rates<-RAM_incidence_rates[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
# Keeps necessary columns 
RAM_incidence_rates<-RAM_incidence_rates[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
# Saves files in medicine counts folder
saveRDS(RAM_incidence_rates, paste0(objective1_dir, "/", pop_prefix, "_RAM_incidence_counts.rds"))
# Saves incidence dfs
saveRDS(RAM_incidence, paste0(objective1_temp_dir, pop_prefix, "_RAM_incidence_data.rds"))


################ incidence by Age Group ###################  
# Count incidence by age, month, year
incidence_by_age<-RAM_incidence[,.N, by = .(year,month, age_group)]

# for each unique age-group, create a counts df with rates 
for(group in 1:length(unique(incidence_by_age$age_group))){
  # Create a subset of age group
  each_group<-incidence_by_age[age_group==unique(incidence_by_age$age_group)[group]]
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
  # Fills in missing values with 0
  each_group[is.na(N),N:=0][is.na(age_group),age_group:=unique(incidence_by_age$age_group)[group]]
  # Create YM variable 
  each_group<-within(each_group,YM<-sprintf("%d-%02d",year,month))
  # Prepare denominator
  incidence_count_min <- RAM_incidence_rates[,c("YM","N")]
  setnames(incidence_count_min,"N","Freq")
  # Merge age-group subset count with all prevalent counts 
  age_group_incidence_count<-merge(x=each_group,y=incidence_count_min,by=c("YM"),all.x=TRUE)
  # Masking set at 0
  age_group_incidence_count[,masked:=0]
  # If masking applies
  if(mask==T){age_group_incidence_count[N>0&N<5,N:=5][Freq>0&Freq<5,Freq:=5][,masked:=1]}
  # Calculates rates
  age_group_incidence_count<-age_group_incidence_count[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates),rates:=0]
  # Adjust for PHARMO
  if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
  # Drop columns you don't need 
  age_group_incidence_count<-age_group_incidence_count[,c("YM","N","Freq","rates","masked")]
  
  # Save files in medicine counts folder
  saveRDS(age_group_incidence_count, (paste0(objective1_strat_dir,"/", pop_prefix,"_RAM_incidence_counts_", unique(incidence_by_age$age_group)[group],"_age_group.rds")))
}

##################################################################################################
############################# Calculates Discontinuation #########################################
##################################################################################################
### Numerator = Number of female subjects in cohort who discontinue RAM in the month
RAM_discontinued<-RAM_episodes_for_discontinued[!duplicated(RAM_episodes_for_discontinued)]
# Create column with patients age at episode.start
RAM_discontinued[,current_age:= floor((episode.end - birth_date)*10/365.25)/10]
# We change the min of age group 12-20.99 to 11 to assign a group to those with tx days in 2009 (otherwise it would fall in a group that is not defined)
RAM_discontinued[current_age >= 11 & current_age < 21, age_group:= "12-20.99"]
RAM_discontinued[current_age >= 21 & current_age < 31, age_group:= "21-30.99"]
RAM_discontinued[current_age >= 31 & current_age < 41, age_group:= "31-40.99"]
RAM_discontinued[current_age >= 41 & current_age < 56, age_group:= "41-55.99"]
# Order by person id, episode start
RAM_discontinued<-RAM_discontinued[order(person_id, episode.start)]
# Create new column with the date of the start of the next episode (per patient) if any. If none exists, this becomes an NA
RAM_discontinued[, next.episode.start:= shift(episode.start, type = "lead" ), by = person_id]
# Exclude records where exit_date (from study) falls within 90 days after episode.end -> this is not considered a discontinuation
RAM_discontinued<-RAM_discontinued[exit_date - episode.end > 90,]
# Any records left with NA in the next.episode.start column are now true discontinue-rs and col discontinued is assigned the value 1
RAM_discontinued<-RAM_discontinued[is.na(next.episode.start), discontinued := 1]
# Exclude records where next.episode.start (if any) falls within 90 days after episode.end
# If difference between next.episode.start and episode end > 90 then this is a true discontinuer and col discontinued is assigned the value 1
# if the difference is less than 90 then it is assigned the value 0
### discontinuation window is set in 3_to_run_final_counts
RAM_discontinued<-RAM_discontinued[!is.na(next.episode.start), discontinued := ifelse(next.episode.start-episode.end > discontinuation_window, 1, 0)]
# Get only rows where the value of discontinued == 1 (true discontinue-rs)
RAM_discontinued<-RAM_discontinued[discontinued == 1,]
# Creates year and month columns
RAM_discontinued[,year:=year(episode.end)][,month:=month(episode.end)]
# Removes all episode ends that fall outside entry_date and exit_date
RAM_discontinued<-RAM_discontinued[episode.end>entry_date & episode.end<=exit_date,]
# Removes unnecessary columns
RAM_discontinued<-RAM_discontinued[,-c("rowID", "next.episode.start", "discontinued")]
# For indication counts 
RAM_discontinued_per_indication<-RAM_discontinued
# Remove duplicates -> Patient is counted only 1x per month-year -ATC?
RAM_discontinued<-unique(RAM_discontinued, by=c("person_id", "year", "month"))
# Performs discontinued counts 
RAM_discontinued_counts<-RAM_discontinued[,.N, by = .(year,month)]
# Adjust for PHARMO
if(is_PHARMO){RAM_discontinued_counts<-RAM_discontinued_counts[year < 2020,]} else {RAM_discontinued_counts<-RAM_discontinued_counts[year < 2021,]}
# Merge with empty df (for counts that do not have counts for all months and years of study)
RAM_discontinued_counts<-as.data.table(merge(x = empty_df, y = RAM_discontinued_counts, by = c("year", "month"), all.x = TRUE))
# Fills in missing values with 0
RAM_discontinued_counts[is.na(N), N:=0]
# Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
RAM_discontinued_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
# Masking is not applied before stratification
RAM_discontinued_counts[,masked:=0]
# Create YM variable 
RAM_discontinued_counts<-within(RAM_discontinued_counts, YM<- sprintf("%d-%02d", year, month))
### Denominator = Number of prevalent RAM users that month
##### ->  denominator file has already been created in for loop variable name = prevalence_all_counts
# Prepare denominator 
RAM_prevalence_counts1<-RAM_prevalence_rates[,c("YM", "N")]
setnames(RAM_prevalence_counts1, "N", "Freq")
### Merges numerator file with denominator file
RAM_discontinued_rates<-merge(x = RAM_discontinued_counts, y = RAM_prevalence_counts1, by = c("YM"), all.x = TRUE)
# Calculates rates
RAM_discontinued_rates<-RAM_discontinued_rates[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
# Keeps necessary columns 
RAM_discontinued_rates<-RAM_discontinued_rates[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
# Saves files in medicine counts folder
saveRDS(RAM_discontinued_rates, paste0(objective2_dir, "/", pop_prefix, "_RAM_discontinued_counts.rds"))
# Saves discontinued dfs
saveRDS(RAM_discontinued, paste0(objective2_temp_dir, pop_prefix, "_RAM_discontinued_data.rds"))

################ Discontinued by Age Group ###################  
# Count discontinued by age, month, year
discontinued_by_age<-RAM_discontinued[,.N, by = .(year,month, age_group)]

# for each unique age-group, create a counts df with rates 
for(group in 1:length(unique(discontinued_by_age$age_group))){
  # Create a subset of age group
  each_group<-discontinued_by_age[age_group==unique(discontinued_by_age$age_group)[group]]
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
  # Fills in missing values with 0
  each_group[is.na(N),N:=0][is.na(age_group),age_group:=unique(discontinued_by_age$age_group)[group]]
  # Create YM variable 
  each_group<-within(each_group,YM<-sprintf("%d-%02d",year,month))
  # Prepare denominator
  discontinued_count_min <- RAM_discontinued_rates[,c("YM","N")]
  setnames(discontinued_count_min,"N","Freq")
  # Merge age-group subset count with all prevalent counts 
  age_group_discontinued_count<-merge(x=each_group,y=discontinued_count_min,by=c("YM"),all.x=TRUE)
  # Masking set at 0
  age_group_discontinued_count[,masked:=0]
  # If masking applies
  if(mask==T){age_group_discontinued_count[N>0&N<5,N:=5][Freq>0&Freq<5,Freq:=5][,masked:=1]}
  # Calculates rates
  age_group_discontinued_count<-age_group_discontinued_count[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates),rates:=0]
  # Adjust for PHARMO
  if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
  # Drop columns you don't need 
  age_group_discontinued_count<-age_group_discontinued_count[,c("YM","N","Freq","rates","masked")]
  
  # Save files in medicine counts folder
  saveRDS(age_group_discontinued_count, (paste0(objective2_strat_dir,"/", pop_prefix,"_RAM_discontinued_counts_", unique(discontinued_by_age$age_group)[group],"_age_group.rds")))
}

# Clean up 
rm(list = grep("^age_group|^RAM_discont|^RAM_inc|^RAM_prev|^incidence_|^prevalence_|^discontinued_|each_group|RAM_episodes|retinoid_incidence_data", ls(), value = TRUE))
















    

  
  