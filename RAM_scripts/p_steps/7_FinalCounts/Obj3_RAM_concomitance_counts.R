#############################################################################################
#############################################################################################
######################################## Objective 3 ########################################
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
#############################################################################################
### 1. Load needed records: 
# RAM treatment episodes 
df_RAM <-as.data.table(readRDS(paste0(g_intermediate,"treatment_episodes_alt_meds/", pop_prefix, "_AltMed_CMA_treatment_episodes.rds")))
# Retinoid treatment episodes
df_retinoid <-as.data.table(readRDS(paste0(g_intermediate,"treatment_episodes/", pop_prefix, "_Retinoid_CMA_treatment_episodes.rds")))
# Denominator 
source(paste0(pre_dir,"4_Denominator/load_denominator.R"))

### 2. Create folders:
# To save incidence and prevalence patient level records  
invisible(ifelse(!dir.exists(paste0(counts_dfs_dir,"RAM_Objective_3")),dir.create(paste0(counts_dfs_dir,"RAM_Objective_3")),FALSE))
objective3_temp_dir<-paste0(counts_dfs_dir,"RAM_Objective_3/")
# To save incidence and prevalence counts/rates
invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/RAM_Objective_3")),dir.create(paste0(medicines_counts_dir,"/RAM_Objective_3")),FALSE))
objective3_dir<-paste0(medicines_counts_dir,"/RAM_Objective_3")
# To save incidence and prevalence stratified counts
invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/RAM_Objective_3/Stratified")),dir.create(paste0(medicines_counts_dir,"/RAM_Objective_3/Stratified")),FALSE))
objective3_strat_dir<-paste0(medicines_counts_dir,"/RAM_Objective_3/Stratified")

##################################################################################################
######################################## Data Preparation ########################################
##################################################################################################
## RAM treatment episodes 
# Merge with study population to get entry and exit dates 
df_RAM<-merge(df_RAM, study_population[,c("person_id", "birth_date", "entry_date","exit_date")], by = "person_id")
# Changes columns to correct data type
df_RAM[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)]
df_RAM[,entry_date:=as.IDate(entry_date)][,exit_date:=as.IDate(exit_date)]
# Rename columns
setnames(df_RAM, "episode.start", "episode.start.altmed")
setnames(df_RAM, "episode.end", "episode.end.altmed")
setnames(df_RAM, "ATC", "ATC.altmed")
setnames(df_RAM, "episode.ID", "episode.ID.altmed")
setnames(df_RAM, "end.episode.gap.days","end.episode.gap.days.altmed")
setnames(df_RAM, "episode.duration", "episode.duration.altmed")

## Retinoid treatment episodes
# Changes columns to correct data type
df_retinoid[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)]
# Rename columns
setnames(df_retinoid, "episode.start", "episode.start.retinoid")
setnames(df_retinoid, "episode.end", "episode.end.retinoid")
setnames(df_retinoid, "ATC", "ATC.retinoid")
setnames(df_retinoid, "episode.ID", "episode.ID.retinoid")
setnames(df_retinoid, "end.episode.gap.days","end.episode.gap.days.retinoid")
setnames(df_retinoid, "episode.duration", "episode.duration.retinoid")

# Create column next.episode.start.retinoid
df_retinoid[order(person_id, ATC.retinoid, episode.start.retinoid)]
df_retinoid[, next.episode.start.retinoid:= shift(episode.start.retinoid, type = "lead" ), by = c("person_id", "ATC.retinoid")]


## Merge RAM and retinoid treatment episodes on person id
df_episodes <- merge(df_RAM, df_retinoid, by="person_id")
df_episodes <- df_episodes[episode.start.altmed>=entry_date & episode.start.altmed<=exit_date,]

# RAM records in Retinoid Population 
study_population_retinoid<-as.data.table(readRDS(paste0(populations_dir, pop_prefix, "_study_population_retinoids.rds")))[,c("person_id")]
# Read in all altmeds 
RAM_meds<-rbindlist(lapply(paste0(medications_pop, list.files(medications_pop, pattern=paste0(pop_prefix, "_altmed"))), readRDS))
# RAM meds in Retinoid users
RAM_meds_retinoid_users <- merge(study_population_retinoid, RAM_meds, by = "person_id")
RAM_meds_retinoid_users_counts <-RAM_meds_retinoid_users[,.N, by = .(year(Date),month(Date))]
# Merge with empty df (for counts that do not have counts for all months and years of study)
RAM_meds_retinoid_users_counts <-as.data.table(merge(x = empty_df, y = RAM_meds_retinoid_users_counts, by = c("year", "month"), all.x = TRUE))
# Fills in missing values with 0
RAM_meds_retinoid_users_counts[is.na(RAM_meds_retinoid_users_counts[,N]), N:=0]
# Creates YM variable
RAM_meds_retinoid_users_counts<-within(RAM_meds_retinoid_users_counts,YM<-sprintf("%d-%02d",year,month))
setnames(RAM_meds_retinoid_users_counts, "N", "Freq")

#### TESTING CODE ####
df_episodes[person_id=="ConCDM_SIM_200421_00925" & episode.start.altmed=="2011-12-02", episode.start.retinoid:=as.IDate("2011-10-02")]
df_episodes[person_id=="ConCDM_SIM_200421_00925" & episode.start.altmed=="2011-12-02", episode.end.retinoid:=as.IDate("2012-02-10")]
df_episodes[person_id=="ConCDM_SIM_200421_00925" & episode.start.altmed=="2011-12-02", episode.end.altmed:=as.IDate("2012-01-01")]
df_episodes[person_id=="ConCDM_SIM_200421_00925" & episode.start.retinoid=="2019-02-01", episode.start.altmed:=as.IDate("2019-03-15")]
df_episodes[person_id=="ConCDM_SIM_200421_00925" & episode.start.retinoid=="2019-02-01", episode.end.altmed:=as.IDate("2019-06-20")]

# # #### TESTING DATA FRAME #####
# library(readxl)
# TestData <- read_excel("C:/Users/mgamb/Desktop/TestData.xlsx",
#                        col_types = c("text", "date", "date",
#                                      "date", "date", "numeric", "numeric",
#                                      "numeric", "text", "date", "numeric",
#                                      "numeric", "numeric", "text", "date"))
# 
# #### TESTING DATA FRAME #####
# df_episodes <-as.data.table(TestData)
# df_episodes <-as.data.table(TestData)
# df_episodes[,episode.start.retinoid:=as.IDate(episode.start.retinoid)]
# df_episodes[,episode.end.retinoid:=as.IDate(episode.end.retinoid)]
# df_episodes[,next.episode.start.retinoid :=as.IDate(next.episode.start.retinoid)]
# df_episodes[,episode.start.altmed:=as.IDate(episode.start.altmed)]
# df_episodes[,episode.end.altmed:=as.IDate(episode.end.altmed)]

###  Concomitance Conditions 
# RAM episode
# Retinoid doesn't have to end in discontinuation 
# Scenario 1: RAM episode starts in Retinoid episode (>=30 days before retinoid episode end) -> DONE
# Scenario 2: RAM episode start begins after retinoid episode start and RAM episode end stops before RAM episode end
# Scenario 3: RAM episode start begins begins before Retinoid episode start and RAM episode ends after next Retinoid episode start

# Get the concomitant users 
df_episodes[,concomit:= ifelse(episode.start.altmed>episode.start.retinoid & episode.start.altmed<episode.end.retinoid & !is.na(next.episode.start.retinoid) & episode.end.altmed>next.episode.start.retinoid & end.episode.gap.days.retinoid<=discontinuation_window, 1,
                               ifelse(episode.start.altmed>=episode.start.retinoid & episode.end.altmed<=episode.end.retinoid,1,
                                      ifelse(episode.start.altmed>episode.start.retinoid & episode.end.retinoid-episode.start.altmed>=30,1,0)))]

# Keep only concomitant users
df_concomit<- df_episodes[concomit==1,]
df_concomit[,episode.start.month.altmed:=month(episode.start.altmed)][,episode.start.year.altmed:=year(episode.start.altmed)]
df_concomit_per_user<-unique(df_concomit, by=c("person_id", "ATC.altmed","episode.start.month.altmed", "episode.start.month.altmed"))

#######################################################################################################
######################################## Concomitance Counts ##########################################
#######################################################################################################
### Calculate concomitant rates

# Calculates concomitance for each unique RAM ATC in data set 
if(nrow(df_concomit_per_user)>0){
  for (i in 1:length(unique(df_concomit_per_user$ATC.altmed))){
    # Create a subset of the concomitance df (based on RAM ATC )
    df_temp<-df_concomit_per_user[ATC.altmed==df_concomit_per_user$ATC.altmed[i],]
    
    #####################################################
    ##### GENERAL MONTHLY CONCOMITANCE ##################  
    #####################################################
    #### 1 #### 
    # Numerator 
    # Count the number of concomitance users in subset per year-month
    concomit_count<-df_temp[,.N, by = .(year(episode.start.altmed),month(episode.start.altmed))]
    # Merge with empty df (for counts that do not have counts for all months and years of study)
    concomit_count <-as.data.table(merge(x = empty_df, y = concomit_count, by = c("year", "month"), all.x = TRUE))
    # Fills in missing values with 0
    concomit_count[is.na(concomit_count[,N]), N:=0]
    # Creates YM variable
    concomit_count<-within(concomit_count,YM<-sprintf("%d-%02d",year,month))
    
    # Denominator (Retinoid Prevalent counts )
    prevalence_count<-as.data.table(readRDS(paste0(medicines_counts_dir, "/", pop_prefix, "_Retinoid_prevalence_counts.rds")))
    # Clean up denominator 
    prevalence_count<-prevalence_count[,c("YM", "N")] 
    # Rename variables
    setnames(prevalence_count,"N", "Freq")
    
    # Merge concomit count with prevalence count (denom)
    concomit_count1<-merge(x=concomit_count, y=prevalence_count, by=c("YM"), all.x=TRUE)
    # Calculate rates
    concomit_count1<-concomit_count1[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][is.nan(rates)|is.na(rates), rates:=0][,rates:=rates*1000]
    # Adjust for PHARMO
    # if(is_PHARMO){concomit_count1<-concomit_count1[year < 2020,]} else {concomit_count1<-concomit_count1[year < 2021,]}
    # Drop columns you don't need 
    concomit_count1<-concomit_count1[,c("YM", "N", "Freq", "rates")]
    
    # Save patient level concomitance records in g_intermediate
    saveRDS(df_temp, (paste0(objective3_temp_dir,"/", pop_prefix,"_", df_concomit_per_user$ATC.altmed[i], "_concomit_counts_df.rds")))
    # Save concomitance count/rates in g_output
    saveRDS(concomit_count1, (paste0(objective3_dir,"/", pop_prefix,"_", df_concomit_per_user$ATC.altmed[i], "_concomit_counts_Obj3a.rds")))
    
    #############################################################
    ##### CONCOMITANCE OF CONTRAINDICATED RAM  ##################  
    #############################################################
    #### 2 #### CONTRAINDICATED USERS
    # For codes: "H02AB02", "L04AX03", "A11CA01", "J01AA07"
    if (df_concomit_per_user$ATC.altmed[i] %in% c("H02AB02", "L04AX03", "A11CA01", "J01AA07")){
      # Denominator (Retinoid Prevalent counts )
      RAM_prev_count<-as.data.table(readRDS(paste0(objective1_dir,"/",pop_prefix, "_",df_concomit_per_user$ATC.altmed[i], "_prevalence_counts.rds")))
      # Clean up denominator 
      RAM_prev_count<-RAM_prev_count[,c("YM", "N")] 
      # Rename variables
      setnames(RAM_prev_count,"N", "Freq")
      # Merge concomitance count with prevalence count (denom)
      concomit_count2<-merge(x=concomit_count, y=RAM_prev_count, by=c("YM"), all.x=TRUE)
      # Calculate rates
      concomit_count2<-concomit_count2[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][is.nan(rates)|is.na(rates), rates:=0][,rates:=rates*1000]
      # Adjust for PHARMO
      # if(is_PHARMO){concomit_count2<-concomit_count2[year < 2020,]} else {concomit_count2<-concomit_count2[year < 2021,]}
      concomit_count2<-concomit_count2[year < 2021,]
      # Drop columns you don't need 
      concomit_count2<-concomit_count2[,c("YM", "N", "Freq", "rates")]
      
      # Save concomitance count/rates in g_output
      saveRDS(concomit_count2, (paste0(objective3_dir,"/", pop_prefix,"_", df_concomit_per_user$ATC.altmed[i], "_concomit_counts_Obj3b.rds")))
      
    } else {
      print("There is no concomitant use with retinoids for the following ATC codes: H02AB02, L04AX03, A11CA01, J01AA07")
    }
    
    #### STRATIFIED BY AGE ####
    # Create column with patients age at RAM.episode.start
    df_temp[,current_age:= floor((episode.start.altmed - birth_date)*10/365.25)/10]
    # We change the min of age group 12-20.99 to 11 to assign a group to those with tx days in 2009 (otherwise it would fall in a group that is not defined)
    df_temp[current_age >= 11 & current_age < 21, age_group:= "12-20.99"]
    df_temp[current_age >= 21 & current_age < 31, age_group:= "21-30.99"]
    df_temp[current_age >= 31 & current_age < 41, age_group:= "31-40.99"]
    df_temp[current_age >= 41 & current_age < 56, age_group:= "41-55.99"]
    
    if(nrow(df_temp)>0){
      # Count temp by age, month, year
      temp_by_age<-df_temp[,.N, by = .(year(episode.start.altmed),month(episode.start.altmed), age_group)]
      # for each unique age-group, create a counts df with rates 
      for(group in 1:length(unique(temp_by_age$age_group))){
        # Create a subset of age group
        each_group<-temp_by_age[age_group==unique(temp_by_age$age_group)[group]]
        # Merge with empty df (for counts that do not have counts for all months and years of study)
        each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
        # Fills in missing values with 0
        each_group[is.na(N),N:=0][is.na(age_group),age_group:=unique(temp_by_age$age_group)[group]]
        # Create YM variable 
        each_group<-within(each_group,YM<-sprintf("%d-%02d",year,month))
        # Prepare denominator
        temp_count_min <- concomit_count1[,c("YM","N")]
        setnames(temp_count_min,"N","Freq")
        # Merge age-group subset count with all temp counts 
        age_group_temp_count<-merge(x=each_group,y=temp_count_min,by=c("YM"),all.x=TRUE)
        # Masking set at 0
        age_group_temp_count[,masked:=0]
        # If masking applies
        if(mask==T){age_group_temp_count[N>0&N<5,N:=5][Freq>0&Freq<5,Freq:=5][,masked:=1]}
        # Calculates rates
        age_group_temp_count<-age_group_temp_count[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates),rates:=0]
        # Adjust for PHARMO
        # if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
        each_group<-each_group[year<2021,]
        # Drop columns you don't need 
        age_group_temp_count<-age_group_temp_count[,c("YM","N","Freq","rates","masked")]
        
        # Save files in medicine counts folder
        saveRDS(age_group_temp_count, (paste0(objective3_strat_dir,"/", pop_prefix,"_", unique(df_concomit_per_user$ATC.altmed)[i], "_concomitant_counts_",  unique(temp_by_age$age_group)[group],"_age_group.rds")))
        
      }
    }
    
    
  }
} else {
  print("There are no concomitant Retinoid-RAM users")
}

#### 3 #### CONTRAINDICATED => RECORDS 

# Calculates concomitance for each unique RAM ATC in data set 
if(nrow(df_concomit)>0){
  for (i in 1:length(unique(df_concomit$ATC.altmed))){
    # For codes: "H02AB02", "L04AX03", "A11CA01", "J01AA07"
    if (df_concomit$ATC.altmed[i] %in% c("H02AB02", "L04AX03", "A11CA01", "J01AA07")){
      
      # Create a subset of the concomitance df (based on RAM ATC )
      df_temp<-df_concomit[ATC.altmed==df_concomit$ATC.altmed[i],]
      
      #####################################################
      ##### GENERAL MONTHLY CONCOMITANCE ##################  
      #####################################################
      
      # Numerator 
      # Count the number of concomitant users in subset per year-month
      concomit_count<-df_temp[,.N, by = .(year(episode.start.altmed),month(episode.start.altmed))]
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      concomit_count <-as.data.table(merge(x = empty_df, y = concomit_count, by = c("year", "month"), all.x = TRUE))
      # Fills in missing values with 0
      concomit_count[is.na(concomit_count[,N]), N:=0]
      
      # Denominator (Number of all RAM prescriptions or dispensations during the same month) # prepared above #RAM_meds_retinoid_users_counts
      # Merge concomit count with prevalence count (denom)
      concomit_count3<-merge(x=concomit_count, y=RAM_meds_retinoid_users_counts, by=c("year", "month"), all.x=TRUE)
      # Calculate rates
      concomit_count3<-concomit_count3[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][is.nan(rates)|is.na(rates), rates:=0][,rates:=rates*1000]
      # Adjust for PHARMO
      # if(is_PHARMO){concomit_count1<-concomit_count1[year < 2020,]} else {concomit_count1<-concomit_count1[year < 2021,]}
      # Drop columns you don't need 
      concomit_count3<-concomit_count1[,c("YM", "N", "Freq", "rates")]
      
      # Save patient level concomitance records in g_intermediate
      saveRDS(df_temp, (paste0(objective3_temp_dir,"/", pop_prefix,"_", df_concomit$ATC.altmed[i], "_concomit_counts_records_df.rds")))
      # Save concomitance count/rates in g_output
      saveRDS(concomit_count3, (paste0(objective3_dir,"/", pop_prefix,"_", df_concomit$ATC.altmed[i], "_concomit_counts_Obj3c.rds")))
      
    } else {
      print("There is no concomitant use with retinoids for the following ATC codes: H02AB02, L04AX03, A11CA01, J01AA07")
    }
  }
} 


# Clean up 
# rm(list = grep("^age_group|df_episode|^altmed|^df_|^each_group|^incidence|^prevalence|study_pop_meds", ls(), value = TRUE))










