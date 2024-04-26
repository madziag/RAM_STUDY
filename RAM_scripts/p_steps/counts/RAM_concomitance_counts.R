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
## RAM Incident data
RAM_episodes<-as.data.table(readRDS(paste0(RAM_treatment_episodes, pop_prefix,"_RAM_CMA_treatment_episodes.rds")))
## Retinoid Discontinuation data 
retinoid_episodes<-as.data.table(readRDS(paste0(retinoid_treatment_episodes, pop_prefix,"_Retinoid_CMA_treatment_episodes.rds")))
## Denominators 
# Retinoid Prevalent Counts
retinoid_prevalence_counts<-as.data.table(readRDS(paste0(medicines_counts_dir, "/",pop_prefix, "_Retinoid_prevalence_counts.rds")))



  
### Data Cleaning 
## RAM Treatment Episodes
# Drop unneeded columns
RAM_episodes<-RAM_episodes[,c("person_id", "episode.start", "episode.end",  "ATC")]
# Change date format
RAM_episodes[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)]
# Rename columns
setnames(RAM_episodes, old = c("episode.start","episode.end","ATC"), new = c("episode.start.RAM","episode.end.RAM","ATC.RAM"))
# Remove duplicates
RAM_episodes<-unique(RAM_episodes, by = c("person_id", "episode.start.RAM", "episode.end.RAM"))

## Retinoid Treatment Episodes
retinoid_episodes<-retinoid_episodes[,-c("episode.duration", "episode.ID")]
# Change date format
retinoid_episodes[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)]
# Rename columns
setnames(retinoid_episodes, old = c("episode.start","episode.end","ATC", "end.episode.gap.days"), new = c("episode.start.retinoid","episode.end.retinoid","ATC.retinoid", "end.episode.gap.days.retinoid"))
# Remove duplicates
retinoid_episodes<-unique(retinoid_episodes, by = c("person_id", "episode.start.retinoid", "episode.end.retinoid"))
# Create column next.episode.start.retinoid
retinoid_episodes<-retinoid_episodes[order(person_id, ATC.retinoid, episode.start.retinoid)]
retinoid_episodes[, next.episode.start.retinoid:= shift(episode.start.retinoid, type = "lead" ), by = c("person_id", "ATC.retinoid")]
## Denominators 
## Retinoid Prevalent Counts
retinoid_prevalence_counts<-retinoid_prevalence_counts[,c("YM", "N")]   
# Rename variables
setnames(retinoid_prevalence_counts,"N", "Freq")

### Merge Retinoid and RAM to compare treatment periods 
RAM_retinoid_use<-merge(RAM_episodes, retinoid_episodes, by="person_id")
# Merges with study population to get birth_date, entry and exit dates
RAM_retinoid_use<-merge(RAM_retinoid_use, retinoid_study_population[,c("person_id", "birth_date", "entry_date","exit_date")], by = "person_id")
# Filter out dates that fall outside entry and exit dates
RAM_retinoid_use<-RAM_retinoid_use[episode.start.RAM>=entry_date & episode.start.RAM<=exit_date,]

#### TESTING CODE ####
RAM_retinoid_use[person_id=="ConCDM_SIM_200421_00925" & episode.start.RAM=="2011-12-02", episode.start.retinoid:=as.IDate("2011-10-02")]
RAM_retinoid_use[person_id=="ConCDM_SIM_200421_00925" & episode.start.RAM=="2011-12-02", episode.end.retinoid:=as.IDate("2012-02-10")]
RAM_retinoid_use[person_id=="ConCDM_SIM_200421_00925" & episode.start.RAM=="2011-12-02", episode.end.altmed:=as.IDate("2012-01-01")]
RAM_retinoid_use[person_id=="ConCDM_SIM_200421_00925" & episode.start.retinoid=="2019-02-01", episode.start.RAM:=as.IDate("2019-03-15")]
RAM_retinoid_use[person_id=="ConCDM_SIM_200421_00925" & episode.start.retinoid=="2019-02-01", episode.end.altmed:=as.IDate("2019-06-20")]


###  Concomitance Conditions 
# RAM episode
# Retinoid doesn't have to end in discontinuation 
# Scenario 1: RAM episode starts in Retinoid episode (>=30 days before retinoid episode end) -> DONE
# Scenario 2: RAM episode start begins after retinoid episode start and RAM episode end stops before RAM episode end
# Scenario 3: RAM episode start begins begins before Retinoid episode start and RAM episode ends after next Retinoid episode start

# Get the concomitant users 
RAM_retinoid_use[,concomit:= ifelse(episode.start.RAM>episode.start.retinoid & episode.start.RAM<episode.end.retinoid & !is.na(next.episode.start.retinoid) & episode.end.altmed>next.episode.start.retinoid & end.episode.gap.days.retinoid<=discontinuation_window, 1,
                               ifelse(episode.start.RAM>=episode.start.retinoid & episode.end.altmed<=episode.end.retinoid,1,
                                      ifelse(episode.start.RAM>episode.start.retinoid & episode.end.retinoid-episode.start.RAM>=30,1,0)))]
# Keep only concomitant users
RAM_concomit<- RAM_retinoid_use[concomit==1,]

if(nrow(RAM_concomit)>0){
  
  # Create column with patients age at episode.start
  RAM_concomit[,current_age:= floor((episode.start.RAM - birth_date)*10/365.25)/10]
  # We change the min of age group 12-20.99 to 11 to assign a group to those with tx days in 2009 (otherwise it would fall in a group that is not defined)
  RAM_concomit[current_age >= 11 & current_age < 21, age_group:= "12-20.99"]
  RAM_concomit[current_age >= 21 & current_age < 31, age_group:= "21-30.99"]
  RAM_concomit[current_age >= 31 & current_age < 41, age_group:= "31-40.99"]
  RAM_concomit[current_age >= 41 & current_age < 56, age_group:= "41-55.99"]
  # Removes all episode ends that fall outside entry_date and exit_date
  RAM_concomit<-RAM_concomit[episode.start.RAM>entry_date & episode.start.RAM<=exit_date,]
  ### General Concomitance ###
  # Creates year and month columns
  RAM_concomit[,year:=year(episode.start.RAM)][,month:=month(episode.start.RAM)]
  # Switching Counts 
  RAM_concomit_counts<-RAM_concomit[,.N, by = .(year,month)]
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
  saveRDS(RAM_concomit_rates, paste0(objective3_dir, "/", pop_prefix, "_RAM_general_concomit_counts.rds"))
  # Saves discontinued dfs
  saveRDS(RAM_concomit, paste0(objective3_temp_dir, pop_prefix, "_RAM_general_concomit_data.rds")) 
  ################ concomit by Age Group ###################  
  # Count concomit by age, month, year
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
    # if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
    each_group<-each_group[year<2021,]
    # Drop columns you don't need 
    age_group_concomit_count<-age_group_concomit_count[,c("YM","N","Freq","rates","masked")]
    
    # Save files in medicine counts folder
    saveRDS(age_group_concomit_count, (paste0(objective3_strat_dir,"/", pop_prefix,"_RAM_concomit_counts_", unique(concomit_by_age$age_group)[group],"_age_group.rds")))
  } 
  
} else {
  print("There was no concomitant use of RAM and Retinoids")
}

# Clean up 
# rm(list = grep("^age_group|df_episode|^altmed|^df_|^each_group|^incidence|^prevalence|study_pop_meds", ls(), value = TRUE))










