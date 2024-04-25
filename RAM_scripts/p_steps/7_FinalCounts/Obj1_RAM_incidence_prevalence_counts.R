#############################################################################################
#############################################################################################
######################################## Objective 1 ########################################
#############################################################################################
#############################################################################################

##### Monthly incidence rates (IR) of RAM #####
# Numerator = number of new RAM users (no use the year prior) per calendar month (at least one day of exposure that month)
# denominator= number of WOCBP who ever used a retinoid at risk that month. 
# Unit = person-month. 

##### Monthly prevalence rates (PR) of RAM #####
# Numerator = number of RAM users per calendar month (with at least one day of exposure that month)
# denominator= number of WOCBP who ever used a retinoid of such calendar month 
# Unit = person-month. 

#############################################################################################
#############################################################################################

### 1. Load needed records: 
# RAM treatment episodes
df_episodes<-as.data.table(readRDS(paste0(g_intermediate,"treatment_episodes_alt_meds/", pop_prefix, "_AltMed_CMA_treatment_episodes.rds")))

# denominator
source(paste0(pre_dir,"4_denominator/load_denominator.R"))

### 2. Create folders:
# To save incidence and prevalence patient level records  
invisible(ifelse(!dir.exists(paste0(counts_dfs_dir,"RAM_Objective_1")),dir.create(paste0(counts_dfs_dir,"RAM_Objective_1")),FALSE))
objective1_temp_dir<-paste0(counts_dfs_dir,"RAM_Objective_1/")
# To save incidence and prevalence counts/rates
invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/RAM_Objective_1")),dir.create(paste0(medicines_counts_dir,"/RAM_Objective_1")),FALSE))
objective1_dir<-paste0(medicines_counts_dir,"/RAM_Objective_1")
# To save incidence and prevalence stratified counts
invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/RAM_Objective_1/Stratified")),dir.create(paste0(medicines_counts_dir,"/RAM_Objective_1/Stratified")),FALSE))
objective1_strat_dir<-paste0(medicines_counts_dir,"/RAM_Objective_1/Stratified")

### 3. Calculate RAM incidence and Prevalence
# Calculates incidence for each unique RAM ATC in data set 
for (i in 1:length(unique(df_episodes$ATC))){
  
  ##################################################################################################
  ######################################## Data Preparation ########################################
  ##################################################################################################
  
  # create a subset for each of the unique RAM ATC values
  df_episodes_subset<-setDT(df_episodes)[ATC == unique(df_episodes$ATC)[i]]
  # Merge with study population to get birth date, entry and exit dates (study pop previously loaded in wrapper script)
  df_episodes_subset<-merge(df_episodes_subset, study_population[,c("person_id", "birth_date", "entry_date","exit_date")], by = "person_id")
  # Change columns to correct data type
  df_episodes_subset[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)]
  df_episodes_subset[,entry_date:=as.IDate(entry_date)][,exit_date:=as.IDate(exit_date)]
  # Add column to indicate row number
  df_episodes_subset[,rowID:=.I]
  # Drop columns you don't need 
  df_episodes_subset<-df_episodes_subset[,-c("end.episode.gap.days", "episode.duration")]
  
  #######################################################################################################
  ######################################## Incidence Calculation ########################################
  #######################################################################################################
  # Order data by person id and episode start
  df_incidence<-df_episodes_subset[order(person_id, episode.start)]
  # Create column previous episode end by person id
  df_incidence<-df_incidence[, previous.episode.end:= shift(episode.end, type = "lag" ), by = c("person_id")]
  # Incident use is when: 
  ## a. episode.id = 1 (patients first treatment episode)
  ## b. difference between the episode.start of the following episode is more than 365 days after episode end of previous episode
  df_incidence<-df_incidence[, incident_user:=ifelse(episode.ID==1 | (!is.na(previous.episode.end) & episode.start-previous.episode.end>365), 1, 0)]
  # Create a subset of only the incident users 
  df_incidence<-df_incidence[incident_user==1,]
  # If episode start falls outside of patients entry and exit into study dates, then remove it
  df_incidence<-df_incidence[episode.start>= entry_date & episode.start<=exit_date,]
  
  # Create column with patients age at episode.start
  df_incidence[,current_age:= floor((episode.start - birth_date)*10/365.25)/10]
  # We change the min of age group 12-20.99 to 11 to assign a group to those with tx days in 2009 (otherwise it would fall in a group that is not defined)
  df_incidence[current_age >= 11 & current_age < 21, age_group:= "12-20.99"]
  df_incidence[current_age >= 21 & current_age < 31, age_group:= "21-30.99"]
  df_incidence[current_age >= 31 & current_age < 41, age_group:= "31-40.99"]
  df_incidence[current_age >= 41 & current_age < 56, age_group:= "41-55.99"]
  
  # Create year and month columns (for counts)
  df_incidence[,year:=year(episode.start)][,month:=month(episode.start)]
  
  # Count incidence by year and month
  incidence_count<-df_incidence[,.N, by = .(year,month)]
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  incidence_count<-as.data.table(merge(x=empty_df, y=incidence_count, by=c("year", "month"), all.x=TRUE))
  # Fills in missing values with 0
  incidence_count[is.na(N), N:=0]
  # Merge incident count with denominator_file
  incidence_count<-merge(x=incidence_count, y=denominator_retinoid, by=c("year", "month"), all.x=TRUE)
  # Calculate rates
  incidence_count<-incidence_count[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
  # Adjust for PHARMO
  # if(is_PHARMO){incidence_count<-incidence_count[year < 2020,]} else {incidence_count<-incidence_count[year < 2021,]}
  incidence_count<-incidence_count[year < 2021,]
  # Drop columns you don't need 
  incidence_count<-incidence_count[,c("YM", "N", "Freq", "rates")]
  
  # Save patient level incident records in g_intermediate
  saveRDS(df_incidence, (paste0(objective1_temp_dir,"/", pop_prefix,"_", unique(df_episodes$ATC)[i], "_incidence_counts_df.rds")))
  # Save incident count in g_output
  saveRDS(incidence_count, (paste0(objective1_dir,"/", pop_prefix,"_", unique(df_episodes$ATC)[i], "_incidence_counts.rds")))

  ################ Stratified by Age Groups ###################
  if(nrow(df_incidence)>0){
    # Count incidence by age, month, year
    incidence_by_age<-df_incidence[,.N, by = .(year,month, age_group)]
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
      incidence_count_min <- incidence_count[,c("YM","N")]
      setnames(incidence_count_min,"N","Freq")
      # Merge age-group subset count with all incident counts 
      age_group_incident_count<-merge(x=each_group,y=incidence_count_min,by=c("YM"),all.x=TRUE)
      # Masking set at 0
      age_group_incident_count[,masked:=0]
      # If masking applies
      if(mask==T){age_group_incident_count[N>0&N<5,N:=5][Freq>0&Freq<5,Freq:=5][,masked:=1]}
      # Calculates rates
      age_group_incident_count<-age_group_incident_count[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates),rates:=0]
      # Adjust for PHARMO
      # if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
      each_group<-each_group[year<2021,]
      # Drop columns you don't need 
      age_group_incident_count<-age_group_incident_count[,c("YM","N","Freq","rates","masked")]
      
      # Save files in medicine counts folder
      saveRDS(age_group_incident_count, (paste0(objective1_strat_dir,"/", pop_prefix,"_", unique(df_episodes$ATC)[i], "_incidence_counts_",  unique(incidence_by_age$age_group)[group],"_age_group.rds")))
    }
  }
  
  ########################################################################################################
  ######################################## Prevalence Calculation ########################################
  ########################################################################################################
  # Expand data to get every day of treatment per patient (will also be used to add age_groups)
  df_episodes_subset_expanded<-setDT(df_episodes_subset)[,list(idnum = person_id, episode.day = seq(episode.start, episode.end, by = "day")), by = "rowID"]
  # Merge back with original data to get all columns 
  df_episodes_subset_expanded<- merge(df_episodes_subset, df_episodes_subset_expanded, by = "rowID")
  # If episode day falls outside of patients entry and exit into study dates, then remove it
  df_episodes_subset_expanded<-df_episodes_subset_expanded[episode.day>=entry_date & episode.day<=exit_date,]
  ### Add columns age groups ### 
  # Creates a column with patients age on every day of in the treatment episode
  df_episodes_subset_expanded[,current_age:= floor((episode.day - birth_date)*10/365.25)/10]
  # Add column which groups each patient into an age group, for each day of their treatment
  # We change the min of age group 12-20.99 to 11 to assign a group to those with tx days in 2009 (otherwise it would fall in a group that is not defined)
  df_episodes_subset_expanded[current_age >= 11 & current_age < 21, age_group:= "12-20.99"]
  df_episodes_subset_expanded[current_age >= 21 & current_age < 31, age_group:= "21-30.99"]
  df_episodes_subset_expanded[current_age >= 31 & current_age < 41, age_group:= "31-40.99"]
  df_episodes_subset_expanded[current_age >= 41 & current_age < 56, age_group:= "41-55.99"]
  
  # Create year and months columns based on episode.day
  df_episodes_subset_expanded[,year:=year(episode.day)][,month:=month(episode.day)]
  # Drop columns you don't need 
  df_episodes_subset_expanded<-df_episodes_subset_expanded[,-c("birth_date", "idnum", "episode.ID", "current_age")]
  
  # Remove duplicates - keep only the earliest record of person_id, year, month => we get the first record of person for every month in every year
  # => data set where each row represents a month of treatment within the treatment episode (patient is represented once per month)
  df_prevalence<-df_episodes_subset_expanded[!duplicated(df_episodes_subset_expanded[,c("person_id", "episode.start", "year", "month")])]
  
  # Count prevalence by year and month
  prevalence_count<-df_prevalence[,.N, by = .(year,month)]
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  prevalence_count<-as.data.table(merge(x=empty_df, y=prevalence_count, by=c("year", "month"), all.x=TRUE))
  # Fills in missing values with 0
  prevalence_count[is.na(N), N:=0]
  # Merge prevalent count with denominator file
  prevalence_count<-merge(x = prevalence_count, y=denominator_retinoid, by=c("year", "month"), all.x=TRUE)
  # Calculate rates
  prevalence_count<-prevalence_count[,rates:=round(as.numeric(N)/as.numeric(Freq), 5)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
  # Adjust for PHARMO
  # if(is_PHARMO){prevalence_count<-prevalence_count[year < 2020,]} else {prevalence_count<-prevalence_count[year < 2021,]}
  prevalence_count<-prevalence_count[year < 2021,]
  # Drop columns you don't need 
  prevalence_count<-prevalence_count[,c("YM", "N", "Freq", "rates")]
  
  # Save patient level prevalent records in g_intermediate
  saveRDS(df_prevalence, (paste0(objective1_temp_dir,"/", pop_prefix,"_", unique(df_episodes$ATC)[i], "_prevalence_counts_df.rds")))
  # Save prevalent count in g_output
  saveRDS(prevalence_count, (paste0(objective1_dir,"/", pop_prefix,"_", unique(df_episodes$ATC)[i], "_prevalence_counts.rds")))

  ################ Stratified by Age Groups ###################  
  if(nrow(df_prevalence)>0){
    # Count prevalence by age, month, year
    prevalence_by_age<-df_prevalence[,.N, by = .(year,month, age_group)]
    
    # for each unique age-group, create a counts df with rates 
    for(group in 1:length(unique(prevalence_by_age$age_group))){
      # Create a subset of age group
      each_group<-prevalence_by_age[age_group==unique(prevalence_by_age$age_group)[group]]
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
      # Fills in missing values with 0
      each_group[is.na(N),N:=0][is.na(age_group),age_group:=unique(incidence_by_age$age_group)[group]]
      # Create YM variable 
      each_group<-within(each_group,YM<-sprintf("%d-%02d",year,month))
      # Prepare denominator
      prevalence_count_min <- prevalence_count[,c("YM","N")]
      setnames(prevalence_count_min,"N","Freq")
      # Merge age-group subset count with all prevalent counts 
      age_group_prevalence_count<-merge(x=each_group,y=prevalence_count_min,by=c("YM"),all.x=TRUE)
      # Masking set at 0
      age_group_prevalence_count[,masked:=0]
      # If masking applies
      if(mask==T){age_group_prevalence_count[N>0&N<5,N:=5][Freq>0&Freq<5,Freq:=5][,masked:=1]}
      # Calculates rates
      age_group_prevalence_count<-age_group_prevalence_count[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates),rates:=0]
      # Adjust for PHARMO
      # if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
      each_group<-each_group[year<2021,]
      # Drop columns you don't need 
      age_group_prevalence_count<-age_group_prevalence_count[,c("YM","N","Freq","rates","masked")]

      # Save files in medicine counts folder
      saveRDS(age_group_prevalence_count, (paste0(objective1_strat_dir,"/", pop_prefix,"_", unique(df_episodes$ATC)[i], "_prevalence_counts_", unique(prevalence_by_age$age_group)[group],"_age_group.rds")))
    }
  }
}

# Clean up 
rm(list = grep("^age_group|df_episode|^altmed|^df_|^each_group|^incidence|^prevalence|study_pop_meds", ls(), value = TRUE))


    


  





    

  
  