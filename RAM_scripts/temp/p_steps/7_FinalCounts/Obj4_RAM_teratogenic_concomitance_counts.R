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
# ### 1. Load needed records: 
# # RAM treatment episodes 
# df_RAM <-as.data.table(readRDS(paste0(g_intermediate,"treatment_episodes_alt_meds/", pop_prefix, "_AltMed_CMA_treatment_episodes.rds")))
# # Retinoid treatment episodes
# df_retinoid <-as.data.table(readRDS(paste0(g_intermediate,"treatment_episodes/", pop_prefix, "_Retinoid_CMA_treatment_episodes.rds")))
# # Denominator 
# source(paste0(pre_dir,"4_Denominator/load_denominator.R"))

### 2. Create folders:
# To save incidence and prevalence patient level records  
invisible(ifelse(!dir.exists(paste0(counts_dfs_dir,"RAM_Objective_4")),dir.create(paste0(counts_dfs_dir,"RAM_Objective_4")),FALSE))
objective4_temp_dir<-paste0(counts_dfs_dir,"RAM_Objective_4/")
# To save incidence and prevalence counts/rates
invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/RAM_Objective_4")),dir.create(paste0(medicines_counts_dir,"/RAM_Objective_4")),FALSE))
objective4_dir<-paste0(medicines_counts_dir,"/RAM_Objective_4")
# To save incidence and prevalence stratified counts
invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/RAM_Objective_4/Stratified")),dir.create(paste0(medicines_counts_dir,"/RAM_Objective_4/Stratified")),FALSE))
objective4_strat_dir<-paste0(medicines_counts_dir,"/RAM_Objective_4/Stratified")

##################################################################################################
######################################## Data Preparation ########################################
##################################################################################################
## RAM treatment episodes 
# # Merge with study population to get entry and exit dates 
# df_RAM<-merge(df_RAM, study_population[,c("person_id", "birth_date", "entry_date","exit_date")], by = "person_id")
# # Changes columns to correct data type
# df_RAM[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)]
# df_RAM[,entry_date:=as.IDate(entry_date)][,exit_date:=as.IDate(exit_date)]
# # Rename columns
# setnames(df_RAM, "episode.start", "episode.start.altmed")
# setnames(df_RAM, "episode.end", "episode.end.altmed")
# setnames(df_RAM, "ATC", "ATC.altmed")
# setnames(df_RAM, "episode.ID", "episode.ID.altmed")
# setnames(df_RAM, "end.episode.gap.days","end.episode.gap.days.altmed")
# setnames(df_RAM, "episode.duration", "episode.duration.altmed")

# ## Retinoid treatment episodes
# # Changes columns to correct data type
# df_retinoid[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)]
# # Rename columns
# setnames(df_retinoid, "episode.start", "episode.start.retinoid")
# setnames(df_retinoid, "episode.end", "episode.end.retinoid")
# setnames(df_retinoid, "ATC", "ATC.retinoid")
# setnames(df_retinoid, "episode.ID", "episode.ID.retinoid")
# setnames(df_retinoid, "end.episode.gap.days","end.episode.gap.days.retinoid")
# setnames(df_retinoid, "episode.duration", "episode.duration.retinoid")

# # Create column next.episode.start.retinoid
# df_retinoid[order(person_id, ATC.retinoid, episode.start.retinoid)]
# df_retinoid[, next.episode.start.retinoid:= shift(episode.start.retinoid, type = "lead" ), by = c("person_id", "ATC.retinoid")]


# ## Merge RAM and retinoid treatment episodes on person id
# df_episodes <- merge(df_RAM, df_retinoid, by="person_id")
# df_episodes <- df_episodes[episode.start.altmed>=entry_date & episode.start.altmed<=exit_date,]
# 
# # RAM records in Retinoid Population 
# study_population_retinoid<-as.data.table(readRDS(paste0(populations_dir, pop_prefix, "_study_population_retinoids.rds")))[,c("person_id")]
# # Read in all altmeds 
# RAM_meds<-rbindlist(lapply(paste0(medications_pop, list.files(medications_pop, pattern=paste0(pop_prefix, "_altmed"))), readRDS))
# # RAM meds in Retinoid users
# RAM_meds_retinoid_users <- merge(study_population_retinoid, RAM_meds, by = "person_id")
# RAM_meds_retinoid_users_counts <-RAM_meds_retinoid_users[,.N, by = .(year(Date),month(Date))]
# # Merge with empty df (for counts that do not have counts for all months and years of study)
# RAM_meds_retinoid_users_counts <-as.data.table(merge(x = empty_df, y = RAM_meds_retinoid_users_counts, by = c("year", "month"), all.x = TRUE))
# # Fills in missing values with 0
# RAM_meds_retinoid_users_counts[is.na(RAM_meds_retinoid_users_counts[,N]), N:=0]
# # Creates YM variable
# RAM_meds_retinoid_users_counts<-within(RAM_meds_retinoid_users_counts,YM<-sprintf("%d-%02d",year,month))
# setnames(RAM_meds_retinoid_users_counts, "N", "Freq")

# #### TESTING CODE ####
# df_episodes[person_id=="ConCDM_SIM_200421_00925" & episode.start.altmed=="2011-12-02", episode.start.retinoid:=as.IDate("2011-10-02")]
# df_episodes[person_id=="ConCDM_SIM_200421_00925" & episode.start.altmed=="2011-12-02", episode.end.retinoid:=as.IDate("2012-02-10")]
# df_episodes[person_id=="ConCDM_SIM_200421_00925" & episode.start.altmed=="2011-12-02", episode.end.altmed:=as.IDate("2012-01-01")]
# df_episodes[person_id=="ConCDM_SIM_200421_00925" & episode.start.retinoid=="2019-02-01", episode.start.altmed:=as.IDate("2019-03-15")]
# df_episodes[person_id=="ConCDM_SIM_200421_00925" & episode.start.retinoid=="2019-02-01", episode.end.altmed:=as.IDate("2019-06-20")]

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

# # Get the concomitant users 
# df_episodes[,concomit:= ifelse(episode.start.altmed>episode.start.retinoid & episode.start.altmed<episode.end.retinoid & !is.na(next.episode.start.retinoid) & episode.end.altmed>next.episode.start.retinoid & end.episode.gap.days.retinoid<=discontinuation_window, 1,
#                                ifelse(episode.start.altmed>=episode.start.retinoid & episode.end.altmed<=episode.end.retinoid,1,
#                                       ifelse(episode.start.altmed>episode.start.retinoid & episode.end.retinoid-episode.start.altmed>=30,1,0)))]
# 
# # Keep only concomitant users
# df_concomit<- df_episodes[concomit==1,]
# df_concomit[,episode.start.month.altmed:=month(episode.start.altmed)][,episode.start.year.altmed:=year(episode.start.altmed)]
# df_concomit_per_user<-unique(df_concomit, by=c("person_id", "ATC.altmed","episode.start.month.altmed", "episode.start.month.altmed"))

#######################################################################################################
######################################## Concomitance Counts ##########################################
#######################################################################################################
### Calculate concomitant rates

# Calculates concomitance for each unique RAM ATC in data set 
if(nrow(df_concomit_per_user)>0){
  for (i in 1:length(unique(df_concomit_per_user$ATC.altmed))){

    #############################################################
    ##### CONCOMITANCE OF TERATOGENIC RAM  ##################  
    #############################################################
    #### TERATOGENIC USERS
    # For codes: "L04AA32","L04AX01","L04AX07","D05BA02","L04AX03","A11CA01",
    # "J01AA07","J01FA01","J01FA10","L04AD01","L04AB01","L04AB02",
    # "L04AC05","L04AC10","L04AC13","L04AC12","L04AC16","L04AC17"
    if (df_concomit_per_user$ATC.altmed[i] %in% c("L04AA32","L04AX01","L04AX07","D05BA02","L04AX03","A11CA01",
                                                  "J01AA07","J01FA01","J01FA10","L04AD01","L04AB01","L04AB02",
                                                  "L04AC05","L04AC10","L04AC13","L04AC12","L04AC16","L04AC17")){
      # Create a subset of the concomitance df (based on RAM ATC )
      df_temp<-df_concomit_per_user[ATC.altmed==df_concomit_per_user$ATC.altmed[i],]
      
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
      
      # Save concomitant count/rates in g_output
      saveRDS(concomit_count2, (paste0(objective4_dir,"/", pop_prefix,"_", df_concomit_per_user$ATC.altmed[i], "_concomit_counts_Obj4a.rds")))
      
    } else {
      print("There is no concomitant use with Teratogenic RAM's")
    }
 
  }
} else {
  print("There are no concomitant Retinoid-RAM users")
}

#### 3 #### TERATOGENIC => RECORDS 

# Calculates concomitance for each unique RAM ATC in data set 
if(nrow(df_concomit)>0){
  for (i in 1:length(unique(df_concomit$ATC.altmed))){
    # For codes: "L04AA32","L04AX01","L04AX07","D05BA02","L04AX03","A11CA01",
    # "J01AA07","J01FA01","J01FA10","L04AD01","L04AB01","L04AB02",
    # "L04AC05","L04AC10","L04AC13","L04AC12","L04AC16","L04AC17"
    if (df_concomit$ATC.altmed[i] %in% c("L04AA32","L04AX01","L04AX07","D05BA02","L04AX03","A11CA01",
                                         "J01AA07","J01FA01","J01FA10","L04AD01","L04AB01","L04AB02",
                                         "L04AC05","L04AC10","L04AC13","L04AC12","L04AC16","L04AC17")){
      
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
      
      # Save concomitant count/rates in g_output
      saveRDS(concomit_count3, (paste0(objective4_dir,"/", pop_prefix,"_", df_concomit$ATC.altmed[i], "_concomit_counts_Obj4b.rds")))
      
    } else {
      print("There is no concomitant use with retinoids for the following ATC codes: H02AB02, L04AX03, A11CA01, J01AA07")
    }
  }
} 


# Clean up 
# rm(list = grep("^age_group|df_episode|^altmed|^df_|^each_group|^incidence|^prevalence|study_pop_meds", ls(), value = TRUE))










