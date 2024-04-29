#############################################################################################
#############################################################################################
######################################## Concomitance ########################################
#############################################################################################
#############################################################################################

##### Monthly general concomitant rates (IR) of RAM #####
## Conditions 
# RAM episode
# Retinoid doesn't have to end in discontinuation 
# Scenario 1: RAM episode starts in Retinoid episode (>=30 days before retinoid episode end) -> DONE
# Scenario 2: RAM episode start begins after retinoid episode start and RAM episode end stops before RAM episode end
# Scenario 3: RAM episode start begins begins before Retinoid episode start and RAM episode ends after next Retinoid episode start

## Numerator = number of users of RAM + Retinoid per calendar month 
## Denominator = number of retinoid users the same month (at least one day of exposure that month) - prevalence?
## Unit = person-month


#############################################################################################
################################## Data Preparation #########################################
#############################################################################################
### Data Loading
## RAM Prevalent Data (dedpuplicated)
RAM_prevalence_data<-as.data.table(readRDS(paste0(objective1_temp_dir, pop_prefix,"_RAM_prevalence_data.rds")))
## Retinoid Prevalent Data (deduplicated)
retinoid_prevalence_data<-as.data.table(readRDS(paste0(retinoid_counts_dfs, pop_prefix,"_Retinoid_prevalence_data.rds")))
## Denominators 
# Retinoid Prevalent Counts
retinoid_prevalence_counts<-as.data.table(readRDS(paste0(medicines_counts_dir, "/",pop_prefix, "_Retinoid_prevalence_counts.rds")))


### Data Cleaning 
## RAM Prevalence
# Remove duplicates
RAM_prevalence_data<-unique(RAM_prevalence_data, by = c("person_id", "episode.start", "episode.end", "ATC"))
# Change date format
RAM_prevalence_data[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)][,entry_date:=as.IDate(entry_date)][,exit_date:=as.IDate(exit_date)]
# Drop columns you dont need
RAM_prevalence_data<-RAM_prevalence_data[,c("person_id", "episode.start", "episode.end", "ATC", "birth_date", "entry_date", "exit_date")]
# Rename columns
setnames(RAM_prevalence_data, old = c("episode.start","episode.end","ATC"), new = c("episode.start.RAM","episode.end.RAM","ATC.RAM"))


## Retinoid Treatment Episodes
# Remove duplicates
retinoid_prevalence_data<-unique(retinoid_prevalence_data, by = c("person_id", "episode.start", "episode.end", "ATC"))
# Change date format
retinoid_prevalence_data[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)]
# Drop columns you dont need
retinoid_prevalence_data<-retinoid_prevalence_data[,c("person_id", "episode.start", "episode.end", "ATC", "end.episode.gap.days")]
# Rename columns
setnames(retinoid_prevalence_data, old = c("episode.start","episode.end","ATC", "end.episode.gap.days"), new = c("episode.start.retinoid","episode.end.retinoid","ATC.retinoid", "end.episode.gap.days.retinoid"))
# Create column next.episode.start.retinoid
retinoid_prevalence_data<-retinoid_prevalence_data[order(person_id, ATC.retinoid, episode.start.retinoid)]
retinoid_prevalence_data[, next.episode.start.retinoid:= shift(episode.start.retinoid, type = "lead" ), by = c("person_id", "ATC.retinoid")]
## Denominators 
## Retinoid Prevalent Counts
retinoid_prevalence_counts<-retinoid_prevalence_counts[,c("YM", "N")]   
# Rename variables
setnames(retinoid_prevalence_counts,"N", "Freq")

### Merge Retinoid and RAM to compare treatment periods 
RAM_retinoid_use<-merge(RAM_prevalence_data, retinoid_prevalence_data, by="person_id", allow.cartesian = TRUE)
# Filter out dates that fall outside entry and exit dates
RAM_retinoid_use<-RAM_retinoid_use[episode.start.RAM>=entry_date & episode.start.RAM<=exit_date,]

###  Concomitance Conditions 
# RAM episode
# Retinoid doesn't have to end in discontinuation 
# Scenario 1: RAM episode starts in Retinoid episode (>=30 days before retinoid episode end) -> DONE
# Scenario 2: RAM episode start begins after retinoid episode start and RAM episode end stops before RAM episode end
# Scenario 3: RAM episode start begins begins before Retinoid episode start and RAM episode ends after next Retinoid episode start

# Get the concomitant users 
RAM_retinoid_use[,concomit:= ifelse(episode.start.RAM>episode.start.retinoid & episode.start.RAM<episode.end.retinoid & !is.na(next.episode.start.retinoid) & episode.end.RAM>next.episode.start.retinoid & end.episode.gap.days.retinoid<=discontinuation_window, 1,
                               ifelse(episode.start.RAM>=episode.start.retinoid & episode.end.RAM<=episode.end.retinoid,1,
                                      ifelse(episode.start.RAM>episode.start.retinoid & episode.end.retinoid-episode.start.RAM>=30,1,0)))]
# Keep only concomitant users
RAM_concomit<- RAM_retinoid_use[concomit==1,]

# Concomitant counts 
if(nrow(RAM_concomit)>0){
  
  # Create column with patients age at episode.start
  RAM_concomit[,current_age:= floor((episode.start.RAM - birth_date)*10/365.25)/10]
  # We change the min of age group 12-20.99 to 11 to assign a group to those with tx days in 2009 (otherwise it would fall in a group that is not defined)
  RAM_concomit[current_age >= 11 & current_age < 21, age_group:= "12-20.99"]
  RAM_concomit[current_age >= 21 & current_age < 31, age_group:= "21-30.99"]
  RAM_concomit[current_age >= 31 & current_age < 41, age_group:= "31-40.99"]
  RAM_concomit[current_age >= 41 & current_age < 56, age_group:= "41-55.99"]
  # Creates year and month columns
  RAM_concomit[,year:=year(episode.start.RAM)][,month:=month(episode.start.RAM)]
  # Removes all episode ends that fall outside entry_date and exit_date
  RAM_concomit<-RAM_concomit[episode.start.RAM>entry_date & episode.start.RAM<=exit_date,]
  # column cleanup
  RAM_concomit<-RAM_concomit[,-c("end.episode.gap.days.retinoid","next.episode.start.retinoid","concomit")]
  # rearrange columns
  setcolorder(RAM_concomit, c("person_id", "episode.start.RAM","episode.end.RAM","ATC.RAM","episode.start.retinoid","episode.end.retinoid","ATC.retinoid","birth_date","entry_date","exit_date","current_age", "age_group","year","month"))
  # For indication counts # all contraindication records
  RAM_concomit_per_indication<-RAM_concomit
  ### General Concomitance ###
  # Get 1 per person id per month-year
  RAM_concomit_user<- unique(RAM_concomit, by=c("person_id", "year", "month"))
  # For flowchart 
  RAM_flowchart_concomit_users<-length(unique(RAM_concomit_user$person_id))
  # For flow chart
  RAM_flowchart_concomit_records<-nrow(RAM_concomit)
  # Concomitant Counts 
  RAM_concomit_counts<-RAM_concomit_user[,.N, by = .(year,month)]
  # Adjust for PHARMO
  if(is_PHARMO){RAM_concomit_counts<-RAM_concomit_counts[year < 2020,]} else {RAM_concomit_counts<-RAM_concomit_counts[year < 2021,]}
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  RAM_concomit_counts <-as.data.table(merge(x = empty_df, y = RAM_concomit_counts, by = c("year", "month"), all.x = TRUE))
  # Fills in missing values with 0
  RAM_concomit_counts[is.na(RAM_concomit_counts[,N]), N:=0]
  # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
  RAM_concomit_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
  # Masking is not applied before stratification
  RAM_concomit_counts[,masked:=0]
  # Create YM variable 
  RAM_concomit_counts<-within(RAM_concomit_counts, YM<- sprintf("%d-%02d", year, month))
  # Denominator => Number of prevalent RAM users that month
  RAM_concomit_rates<-merge(x = RAM_concomit_counts, y = retinoid_prevalence_counts, by = c("YM"), all.x = TRUE)
  # Calculates rates
  RAM_concomit_rates<-RAM_concomit_rates[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
  # Keeps necessary columns 
  RAM_concomit_rates<-RAM_concomit_rates[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
  # Saves files in medicine counts folder
  saveRDS(RAM_concomit_rates, paste0(objective3_dir, "/", pop_prefix, "_RAM_general_concomit_per_user_counts.rds"))
  # Saves concomitant data (all)
  saveRDS(RAM_concomit_per_indication, paste0(objective3_temp_dir, pop_prefix, "_RAM_general_concomit_data.rds")) 
  
  ################ concomitance by Age Group ###################  
  # Count concomitance by age, month, year
  concomit_by_age<-RAM_concomit[,.N, by = .(year,month, age_group)]
  
  # for each unique age-group, create a counts df with rates 
  for(group in 1:length(unique(concomit_by_age$age_group))){
    # Create a subset of age group
    each_group<-concomit_by_age[age_group==unique(concomit_by_age$age_group)[group]]
    # Merge with empty df (for counts that do not have counts for all months and years of study)
    each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
    # Fills in missing values with 0
    each_group[is.na(N),N:=0][is.na(age_group),age_group:=unique(concomit_by_age$age_group)[group]]
    # Create YM variable 
    each_group<-within(each_group,YM<-sprintf("%d-%02d",year,month))
    # Prepare denominator
    concomit_count_min <- RAM_concomit_rates[,c("YM","N")]
    setnames(concomit_count_min,"N","Freq")
    # Merge age-group subset count with all prevalent counts 
    age_group_concomit_count<-merge(x=each_group,y=concomit_count_min,by=c("YM"),all.x=TRUE)
    # Masking set at 0
    age_group_concomit_count[,masked:=0]
    # If masking applies
    if(mask==T){age_group_concomit_count[N>0&N<5,N:=5][Freq>0&Freq<5,Freq:=5][,masked:=1]}
    # Calculates rates
    age_group_concomit_count<-age_group_concomit_count[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates),rates:=0]
    # Adjust for PHARMO
    if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
    # Drop columns you don't need 
    age_group_concomit_count<-age_group_concomit_count[,c("YM","N","Freq","rates","masked")]
    
    # Save files in medicine counts folder
    saveRDS(age_group_concomit_count, (paste0(objective3_strat_dir,"/", pop_prefix,"_RAM_concomit_counts_", unique(concomit_by_age$age_group)[group],"_age_group.rds")))
  } 
  
  

  ################ concomit by Indication ###################  
  # Removes any records where episode.day falls outside of entry & exit dates
  RAM_concomit_per_indication<-RAM_concomit_per_indication[episode.start.RAM>=entry_date & episode.start.RAM<=exit_date,]
  # Create subsets for each indication 
  RAM_concomit_per_indication_psoriasis<-RAM_concomit_per_indication[ATC.RAM%in%c("D05AC01","D05AD02","D05BA02","D05BX51","D07AB01",
                                                                                  "D07AB02","D07AB03","D07AB04","D07AB05","D07AB06",
                                                                                  "D07AB07","D07AB08","D07AB09","D07AB10","D07AB11",
                                                                                  "D07AB19","D07AB21","D07AB30","D07AC01","D07AC02",
                                                                                  "D07AC03","D07AC04","D07AC05","D07AC06","D07AC07",
                                                                                  "D07AC08","D07AC09","D07AC10","D07AC11","D07AC12",
                                                                                  "D07AC13","D07AC14","D07AC15","D07AC16","D07AC17",
                                                                                  "D07AC18","D07AC19","D07AC21","D07AD01","D07AD02",
                                                                                  "D11AH01","D11AH02","L04AA32","L04AB01","L04AB02",
                                                                                  "L04AB04","L04AB05","L04AC05","L04AC10","L04AC12",
                                                                                  "L04AC13","L04AC16","L04AC17","L04AD01","L04AX07"),]
  
  RAM_concomit_per_indication_acne<-RAM_concomit_per_indication[ATC.RAM%in%c("D07AA01","D07AB19","D10AA01","D10AA02","D10AA03",
                                                                             "D10AE01","D10AF01","D10AF02","D10AF51","D10AF52",
                                                                             "H02AA01","H02AA02","H02AA03","H02AB01","H02AB02",
                                                                             "H02AB03","H02AB04","H02AB05","H02AB08","H02AB09",
                                                                             "H02AB10","H02AB11","H02AB13","H02AB14","H02AB15",
                                                                             "H02AB17","J01AA08","J01FA01","J01FA10","S01AA17","S01AA26"),]
  
  RAM_concomit_per_indication_dermatitis<-RAM_concomit_per_indication[ATC.RAM%in%c("D07AB01","D07AB02", "D07AB03","D07AB04","D07AB05",
                                                                                   "D07AB06","D07AB07","D07AB08","D07AB09","D07AB10",
                                                                                   "D07AB11","D07AB19","D07AB21","D07AB30","D11AH01",
                                                                                   "D11AH02","H02AB06","H02AB07","L04AD01","L04AX01","L04AX03"),]
  
  RAM_concomit_per_indication_psoriasis[,indication:="psoriasis"]
  RAM_concomit_per_indication_acne[,indication:="acne"]
  RAM_concomit_per_indication_dermatitis[,indication:="dermatitis"]
  
  RAM_concomit_all_ind<-rbindlist(list(RAM_concomit_per_indication_psoriasis,RAM_concomit_per_indication_acne,RAM_concomit_per_indication_dermatitis))
  # To be counted once per person, Year-month, indication
  RAM_concomit_all_ind<-unique(RAM_concomit_all_ind,by=c("person_id", "year", "month","indication"))
  # flowchart
  RAM_flowchart_concomit_psoriasis<-length(unique(RAM_concomit_per_indication_psoriasis$person_id))
  RAM_flowchart_concomit_acne<-length(unique(RAM_concomit_per_indication_acne$person_id))
  RAM_flowchart_concomit_dermatitis<-length(unique(RAM_concomit_per_indication_dermatitis$person_id))
  
  rm(RAM_concomit_per_indication_psoriasis,RAM_concomit_per_indication_acne,RAM_concomit_per_indication_dermatitis)
  
  # Count concomit by age, month, year
  concomit_by_indication<-RAM_concomit_all_ind[,.N, by = .(year,month, indication)]
  
  for(group in 1:length(unique(concomit_by_indication$indication))){
    # Create a subset of age group
    each_group<-concomit_by_indication[indication==unique(concomit_by_indication$indication)[group]]
    # Merge with empty df (for counts that do not have counts for all months and years of study)
    each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
    # Fills in missing values with 0
    each_group[is.na(N),N:=0][is.na(indication),indication:=unique(concomit_by_indication$indication)[group]]
    # Adjust for PHARMO
    if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
    # Create YM variable 
    each_group<-within(each_group,YM<-sprintf("%d-%02d",year,month))
    
    # Prepare denominator
    concomit_count_min <- RAM_concomit_rates[,c("YM","N")]
    setnames(concomit_count_min,"N","Freq")
    
    # Merge age-group subset count with all prevalent counts 
    indication_concomit_count<-merge(x=each_group,y=concomit_count_min,by=c("YM"),all.x=TRUE)
    # Masking set at 0
    indication_concomit_count[,masked:=0]
    # If masking applies
    if(mask==T){indication_concomit_count[N>0&N<5,N:=5][Freq>0&Freq<5,Freq:=5][,masked:=1]}
    # Calculates rates
    indication_concomit_count<-indication_concomit_count[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates),rates:=0]
    # Drop columns you don't need 
    indication_concomit_count<-indication_concomit_count[,c("YM","N","Freq","rates","masked")]
    
    # Save files in medicine counts folder
    saveRDS(indication_concomit_count, (paste0(objective3_strat_dir,"/", pop_prefix,"_RAM_concomit_counts_", unique(concomit_by_indication$indication)[group],"_indication_group.rds")))
    
  }
  
  
} else {
  print("There was no concomitant use of RAM and Retinoids")
}








# Get Unrelated 
# treatment initiation of a RAM ≥90 days after the oral retinoid end date
RAM_retinoid_use[,unrelated:=ifelse(episode.start.RAM-episode.end.retinoid>=90 | episode.start.RAM-episode.end.retinoid<= -90, 1, 0)]

# Keep only unrelated 
RAM_unrelated <- RAM_retinoid_use[unrelated==1,]   
# Get unique users
RAM_unrelated_users<-unique(RAM_unrelated, by=c("person_id"))
# Get unique records
RAM_unrelated_records<-unique(RAM_unrelated)
# Flowchart
RAM_flowchart_unrelated_users<-nrow(RAM_unrelated_users)
RAM_flowchart_unrelated_records<-nrow(RAM_unrelated_records) 
# Clean up 
rm(list = grep("^age_group|concomit_by|concomit_count|each_group|RAM_concomit|RAM_prev|RAM_ret|RAM_unre|retinoid_prev", ls(), value = TRUE))










