#############################################################################################
#############################################################################################
########################## Switching #######################################################
#############################################################################################
#############################################################################################

##### Monthly switching rates (IR) of RAM #####
## Conditions 
# a. Retinoid Treatment Episode needs to end in discontinuation (i.e. no retinoid episode within 90 days (discontinuation window) after episode end) 
# b. Alt medicine episode start = retinoid episode end
# c. Alt medicine episode start within 90 days after retinoid episode end
# d. Alt medicine episode start within 30 days before retinoid episode end (30 days overlap)
## Version 1
# Numerator = RAM incident users who switched from an oral retinoid 
# Denominator = number of WOCBP who ever used a retinoid the actual month (at least one day of exposure that month) (Retinoid Prevalent Users) 
# Unit = person-month
## Version 2
# Numerator = RAM incident users who switched from an oral retinoid 
# Denominator = number of WOCBP who ever discontinued a retinoid the actual month (at least one day of exposure that month). 
# Unit = person-month.  

#############################################################################################
################################## Data Preparation #########################################
#############################################################################################
### Data Loading
## RAM Prevalent Data 
RAM_prevalence_data<-as.data.table(readRDS(paste0(objective1_temp_dir, pop_prefix,"_RAM_prevalence_data.rds")))
## Retinoid Discontinuation data 
retinoid_discontinued_data<-as.data.table(readRDS(paste0(retinoid_counts_dfs, pop_prefix,"_Retinoid_discontinued_data.rds")))
## Denominators 
# Retinoid Prevalent Counts
retinoid_prevalence_counts<-as.data.table(readRDS(paste0(medicines_counts_dir, "/",pop_prefix, "_Retinoid_prevalence_counts.rds")))
# Retinoid Discontinued Counts 
retinoid_discontinued_counts<-as.data.table(readRDS(paste0(medicines_counts_dir, "/",pop_prefix, "_Retinoid_discontinued_counts.rds")))


### Data Cleaning 
## RAM Incident data
# Drop unneeded columns
RAM_prevalence_data<-RAM_prevalence_data[,c("person_id", "episode.start", "episode.end",  "ATC.RAM", "birth_date", "entry_date", "exit_date")]
# Change date format
RAM_prevalence_data[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)][,entry_date:=as.IDate(entry_date)][,exit_date:=as.IDate(exit_date)]
# Rename columns
setnames(RAM_prevalence_data, old = c("episode.start","episode.end"), new = c("episode.start.RAM","episode.end.RAM"))
# Remove duplicates
RAM_prevalence_data<-unique(RAM_prevalence_data, by = c("person_id", "episode.start.RAM", "episode.end.RAM", "ATC.RAM"))

## Retinoid Discontinuation data
retinoid_discontinued_data<-retinoid_discontinued_data[,c("person_id", "episode.start", "episode.end", "ATC.retinoid")]
# Change date format
retinoid_discontinued_data[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)]
# Rename columns
setnames(retinoid_discontinued_data, old = c("episode.start","episode.end"), new = c("episode.start.retinoid","episode.end.retinoid"))
# Remove duplicates
retinoid_discontinued_data<-unique(retinoid_discontinued_data, by = c("person_id", "episode.start.retinoid", "episode.end.retinoid"))

## Denominators 
## Retinoid Prevalent Counts
retinoid_prevalence_counts<-retinoid_prevalence_counts[,c("YM", "N")]   
# Rename variables
setnames(retinoid_prevalence_counts,"N", "Freq")
## Retinoid Discontinued Counts 
retinoid_discontinued_counts<-retinoid_discontinued_counts[,c("YM", "N")]
# Rename variables 
setnames(retinoid_discontinued_counts,"N", "Freq")

### Merge Retinoid and RAM to compare treatment periods 
RAM_retinoid_use<-merge(RAM_prevalence_data, retinoid_discontinued_data, by="person_id", allow.cartesian=TRUE)
RAM_retinoid_use<-RAM_retinoid_use[episode.start.RAM>=entry_date & episode.start.RAM<=exit_date,]

# Get the switchers
RAM_retinoid_use[,switcher:=ifelse(episode.start.RAM>episode.end.retinoid & episode.start.RAM-episode.end.retinoid <90, 1, 
                                   ifelse(episode.start.RAM==episode.end.retinoid, 1,
                                          ifelse(episode.start.RAM>episode.start.retinoid & episode.end.retinoid-episode.start.RAM<30 & episode.end.retinoid-episode.start.RAM>0, 1, 0)))]
# Get data with switchers only 
RAM_switcher <- RAM_retinoid_use[switcher==1,]

if(nrow(RAM_switcher)>0){
  # Create column with patients age at episode.start
  RAM_switcher[,current_age:= floor((episode.start.RAM - birth_date)*10/365.25)/10]
  # We change the min of age group 12-20.99 to 11 to assign a group to those with tx days in 2009 (otherwise it would fall in a group that is not defined)
  RAM_switcher[current_age >= 11 & current_age < 21, age_group:= "12-20.99"]
  RAM_switcher[current_age >= 21 & current_age < 31, age_group:= "21-30.99"]
  RAM_switcher[current_age >= 31 & current_age < 41, age_group:= "31-40.99"]
  RAM_switcher[current_age >= 41 & current_age < 56, age_group:= "41-55.99"]
  # Creates year and month columns
  RAM_switcher[,year:=year(episode.start.RAM)][,month:=month(episode.start.RAM)]
  # Removes all episode ends that fall outside entry_date and exit_date
  RAM_switcher<-RAM_switcher[episode.start.RAM>entry_date & episode.start.RAM<=exit_date,]
  # Drop unnecessary columns
  RAM_switcher<-RAM_switcher[,-c("switcher")]
  # rearrange columns
  setcolorder(RAM_switcher, c("person_id", "episode.start.RAM","episode.end.RAM","ATC.RAM","episode.start.retinoid","episode.end.retinoid","ATC.retinoid","birth_date","entry_date","exit_date","current_age", "age_group","year","month"))
  # For indication counts 
  RAM_switcher_per_indication<-RAM_switcher
  # Remove duplicates -> Patient is counted only 1x per month-year -ATC?
  RAM_switcher<-unique(RAM_switcher, by=c("person_id", "year", "month"))
   ### Switcher Version 1: Denominator => Retinoid Prevalence ###
  
  # Switching Counts 
  RAM_switcher_counts<-RAM_switcher[,.N, by = .(year,month)]
  # Adjust for PHARMO
  if(is_PHARMO){RAM_switcher_counts<-RAM_switcher_counts[year < 2020,]} else {RAM_switcher_counts<-RAM_switcher_counts[year < 2021,]}
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  RAM_switcher_counts <-as.data.table(merge(x = empty_df, y = RAM_switcher_counts, by = c("year", "month"), all.x = TRUE))
  # Fills in missing values with 0
  RAM_switcher_counts[is.na(RAM_switcher_counts[,N]), N:=0]
  # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
  RAM_switcher_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
  # Masking is not applied before stratification
  RAM_switcher_counts[,masked:=0]
  # Create YM variable 
  RAM_switcher_counts<-within(RAM_switcher_counts, YM<- sprintf("%d-%02d", year, month))
  # Denominator => Number of prevalent RAM users that month
  RAM_switcher_rates1<-merge(x = RAM_switcher_counts, y = retinoid_prevalence_counts, by = c("YM"), all.x = TRUE)
  # Calculates rates
  RAM_switcher_rates1<-RAM_switcher_rates1[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
  # Keeps necessary columns 
  RAM_switcher_rates1<-RAM_switcher_rates1[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
  # Saves files in medicine counts folder
  saveRDS(RAM_switcher_rates1, paste0(objective2_dir, "/", pop_prefix, "_RAM_switcher_1_counts.rds"))
  # Saves discontinued dfs
  saveRDS(RAM_switcher, paste0(objective2_temp_dir, pop_prefix, "_RAM_switcher_data.rds"))
  
  ### Switcher Version 2: Denominator => Retinoid Discontinuation ###
  # Denominator => Number of discontinued users that month
  RAM_switcher_rates2<-merge(x = RAM_switcher_counts, y = retinoid_discontinued_counts, by = c("YM"), all.x = TRUE)
  # Calculates rates
  RAM_switcher_rates2<-RAM_switcher_rates2[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
  # Keeps necessary columns 
  RAM_switcher_rates2<-RAM_switcher_rates2[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
  # Saves files in medicine counts folder
  saveRDS(RAM_switcher_rates2, paste0(objective2_dir, "/", pop_prefix, "_RAM_switcher_2_counts.rds"))
  
  ################ switcher by Age Group ###################  
  # Count switcher by age, month, year
  switcher_by_age<-RAM_switcher[,.N, by = .(year,month, age_group)]
  
  # for each unique age-group, create a counts df with rates 
  for(group in 1:length(unique(switcher_by_age$age_group))){
    # Create a subset of age group
    each_group<-switcher_by_age[age_group==unique(switcher_by_age$age_group)[group]]
    # Merge with empty df (for counts that do not have counts for all months and years of study)
    each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
    # Fills in missing values with 0
    each_group[is.na(N),N:=0][is.na(age_group),age_group:=unique(switcher_by_age$age_group)[group]]
    # Create YM variable 
    each_group<-within(each_group,YM<-sprintf("%d-%02d",year,month))
    # Prepare denominator
    switcher_count_min <- RAM_switcher_rates1[,c("YM","N")]
    setnames(switcher_count_min,"N","Freq")
    # Merge age-group subset count with all prevalent counts 
    age_group_switcher_count<-merge(x=each_group,y=switcher_count_min,by=c("YM"),all.x=TRUE)
    # Masking set at 0
    age_group_switcher_count[,masked:=0]
    # If masking applies
    if(mask==T){age_group_switcher_count[N>0&N<5,N:=5][Freq>0&Freq<5,Freq:=5][,masked:=1]}
    # Calculates rates
    age_group_switcher_count<-age_group_switcher_count[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates),rates:=0]
    # Adjust for PHARMO
    if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
    # Drop columns you don't need 
    age_group_switcher_count<-age_group_switcher_count[,c("YM","N","Freq","rates","masked")]
    
    # Save files in medicine counts folder
    saveRDS(age_group_switcher_count, (paste0(objective2_strat_dir,"/", pop_prefix,"_RAM_switcher_counts_", unique(switcher_by_age$age_group)[group],"_age_group.rds")))
  }
  
} else {
  print("There are no switchers from Retinoids to RAM")
  RAM_flowchart_switcher<-0
  RAM_flowchart_switcher_psoriasis<-0
  RAM_flowchart_switcher_acne<-0
  RAM_flowchart_switcher_dermatitis<-0
}

# Clean up 
rm(list = grep("^age_group|each_group|RAM_episodes|RAM_episodes|retinoid_d|RAM_switch|RAM_ret|RAM_incidence|RAM_meds_in|switcher_by_age|switcher_count_min|retinoid_prevalence_counts|switcher_by|RAM_prevalence_data", ls(), value = TRUE))









