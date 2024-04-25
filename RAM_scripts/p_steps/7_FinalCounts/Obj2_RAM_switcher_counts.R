#############################################################################################
#############################################################################################
######################################## Objective 2 ########################################
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

##### Monthly discontinuation rates (IR) of RAM #####
## Conditions 
# a. Not receiving any other prescription or dispensing of the same drug within 90 days after the end date of the last treatment episode
# Numerator = number of subjects discontinuing RAM per calendar month
# Denominator = number of all RAM users during the same month (at least one day of exposure that month) (prevalence)
# Unit = person-month.  

#############################################################################################
#############################################################################################
### 1. Load needed records: 
# Denominator 
source(paste0(pre_dir,"4_denominator/load_denominator.R"))

# RAM incidence files 
RAM_incidence_files<-list.files(objective1_temp_dir, pattern = "incidence") # get list of incidence files 
RAM_incidence_files<-RAM_incidence_files[grepl(pop_prefix, RAM_incidence_files)] # filter by subpop (BIFAP)
if(populations[pop] == "PC_study_population.rds"){RAM_incidence_files<-RAM_incidence_files[!grepl("PC_HOSP", RAM_incidence_files)]}
RAM_incidence_list<-lapply(paste0(objective1_temp_dir, RAM_incidence_files), readRDS) # Read in all the files 

df_RAM_incidence<-as.data.table(rbindlist(RAM_incidence_list)) # Create df that binds all RAM Incidence files 

# Retinoid discontinued files
df_retinoid_discontinued<-as.data.table(readRDS(paste0(counts_dfs_dir,"objective_1/", pop_prefix, "_Retinoid_discontinued_counts_df.rds")))

### 2. Create folders:
# To save incidence and prevalence patient level records  
invisible(ifelse(!dir.exists(paste0(counts_dfs_dir,"RAM_Objective_2")),dir.create(paste0(counts_dfs_dir,"RAM_Objective_2")),FALSE))
objective2_temp_dir<-paste0(counts_dfs_dir,"RAM_Objective_2/")
# To save incidence and prevalence counts/rates
invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/RAM_Objective_2")),dir.create(paste0(medicines_counts_dir,"/RAM_Objective_2")),FALSE))
objective2_dir<-paste0(medicines_counts_dir,"/RAM_Objective_2")
# To save incidence and prevalence stratified counts
invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/RAM_Objective_2/Stratified")),dir.create(paste0(medicines_counts_dir,"/RAM_Objective_2/Stratified")),FALSE))
objective2_strat_dir<-paste0(medicines_counts_dir,"/RAM_Objective_2/Stratified")


##################################################################################################
######################################## Data Preparation ########################################
##################################################################################################
### 3. Prepare RAM treatment episodes
# Change columns to correct data type
df_RAM_incidence[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)]
df_RAM_incidence[,entry_date:=as.IDate(entry_date)][,exit_date:=as.IDate(exit_date)]
df_RAM_incidence <- df_RAM_incidence[,c("person_id", "episode.start", "episode.end",  "ATC", "birth_date", "entry_date", "exit_date")]

# Rename columns
setnames(df_RAM_incidence, "episode.start", "episode.start.altmed")
setnames(df_RAM_incidence, "episode.end", "episode.end.altmed")
setnames(df_RAM_incidence, "ATC", "ATC.altmed")

### 3. Prepare Retinoid treatment episodes
# Change columns to correct data type
df_retinoid_discontinued <- df_retinoid_discontinued[,c("person_id", "episode.start", "episode.end", "ATC")]

df_retinoid_discontinued[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)]
# Rename columns
setnames(df_retinoid_discontinued, "episode.start", "episode.start.retinoid")
setnames(df_retinoid_discontinued, "episode.end", "episode.end.retinoid")
setnames(df_retinoid_discontinued, "ATC", "ATC.retinoid")

### 5. Merge RAM and retinoid treatment episodes on person id
df_episodes <- merge(df_retinoid_discontinued, df_RAM_incidence, by="person_id")
df_episodes <- df_episodes[episode.start.altmed>=entry_date & episode.start.altmed<=exit_date,]

### TESTING DATA FRAME #####
### TESTING DATA FRAME #####
#### TESTING DATA FRAME #####
# library(readxl)
# TestData <- as.data.table(read_excel("C:/Users/mgamb/Desktop/TestData.xlsx",
#                                      col_types = c("text", "date", "date",
#                                                    "date", "date", "numeric", "numeric",
#                                                    "numeric", "text", "date", "numeric",
#                                                    "numeric", "numeric", "text", "date")))
# 
# 
# df_episodes <-as.data.table(TestData)
# df_episodes[,episode.start.retinoid:=as.IDate(episode.start.retinoid)]
# df_episodes[,episode.end.retinoid:=as.IDate(episode.end.retinoid)]
# df_episodes[,next.episode.start.retinoid :=as.IDate(next.episode.start.retinoid)]
# df_episodes[,episode.start.altmed:=as.IDate(episode.start.altmed)]
# df_episodes[,episode.end.altmed:=as.IDate(episode.end.altmed)]

#### TESTING DATA FRAME #####
#### TESTING DATA FRAME #####
#### TESTING DATA FRAME #####

# #### TESTING CODE ####
# df_episodes[person_id=="ConCDM_SIM_200421_00925" & episode.start.altmed=="2011-12-02", episode.start.retinoid:=as.IDate("2011-10-02")]
# df_episodes[person_id=="ConCDM_SIM_200421_00925" & episode.start.altmed=="2011-12-02", episode.end.retinoid:=as.IDate("2011-11-01")]
# df_episodes[person_id=="ConCDM_SIM_200421_00925" & episode.start.altmed=="2011-12-02", episode.end.altmed:=as.IDate("2012-01-01")]
# df_episodes[person_id=="ConCDM_SIM_200421_00925" & episode.start.retinoid=="2019-02-01", episode.start.altmed:=as.IDate("2019-07-15")]
# df_episodes[person_id=="ConCDM_SIM_200421_00925" & episode.start.retinoid=="2019-02-01", episode.end.altmed:=as.IDate("2020-02-14")]
# 
# #### TESTING CODE ####
# Get the switchers
df_episodes[,switcher:=ifelse(episode.start.altmed>episode.end.retinoid & episode.start.altmed-episode.end.retinoid <90, 1, 
                              ifelse(episode.start.altmed==episode.end.retinoid, 1,
                                     ifelse(episode.start.altmed>episode.start.retinoid & episode.end.retinoid-episode.start.altmed<30 & episode.end.retinoid-episode.start.altmed>0, 1, 0)))]
df_switcher <- df_episodes[switcher==1,]

#######################################################################################################
######################################## Switching Calculation ########################################
#######################################################################################################

### 6. Calculate RAM switching and discontinuation 
# Calculates switching and discontinuation for each unique RAM ATC in data set 
if(nrow(df_switcher)>0){
  for (i in 1:length(unique(df_switcher$ATC.altmed))){
    #################################################
    ##### VERSION 1 ##### denom = Retinoid prevalence 
    #################################################
    # Create a subset of the switcher df (based on RAM ATC )
    df_temp<-df_switcher[ATC.altmed==df_switcher$ATC.altmed[i],]
    
    # Numerator 
    # Count the number of switchers in subset per year-month
    switch_count<-df_temp[,.N, by = .(year(episode.start.altmed),month(episode.start.altmed))]
    # Merge with empty df (for counts that do not have counts for all months and years of study)
    switch_count <-as.data.table(merge(x = empty_df, y = switch_count, by = c("year", "month"), all.x = TRUE))
    # Fills in missing values with 0
    switch_count[is.na(switch_count[,N]), N:=0]
    # Creates YM variable
    switch_count<-within(switch_count,YM<-sprintf("%d-%02d",year,month))
    
    # Denominator1 (Retinoid Prevalent counts )
    prevalence_count<-as.data.table(readRDS(paste0(medicines_counts_dir, "/", pop_prefix, "_Retinoid_prevalence_counts.rds")))
    # Clean up denominator 
    prevalence_count<-prevalence_count[,c("YM", "N")] 
    # Rename variables
    setnames(prevalence_count,"N", "Freq")
    
    # Merge switch count with prevalence count (denom)
    switch_count1<-merge(x=switch_count, y=prevalence_count, by=c("YM"), all.x=TRUE)
    # Calculate rates
    switch_count1<-switch_count1[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][is.nan(rates)|is.na(rates), rates:=0][,rates:=rates*1000]
    # Adjust for PHARMO
    # if(is_PHARMO){switch_count1<-switch_count1[year < 2020,]} else {switch_count1<-switch_count1[year < 2021,]}
    # Drop columns you don't need 
    switch_count1<-switch_count1[,c("YM", "N", "Freq", "rates")]
    
    # Save patient level switcher records in g_intermediate
    saveRDS(df_temp, (paste0(objective2_temp_dir,"/", pop_prefix,"_", df_switcher$ATC.altmed[i], "_switcher_counts_df.rds")))
    # Save switcher count/rates in g_output
    saveRDS(switch_count1, (paste0(objective2_dir,"/", pop_prefix,"_", df_switcher$ATC.altmed[i], "_switcher_counts_denom_prev.rds")))
    
    #######################################################
    ##### VERSION 2 ##### denom = Retinoid discontinuation  
    ######################################################
    # Denominator1 (Retinoid Discontinued counts )
    discontinued_count<-as.data.table(readRDS(paste0(medicines_counts_dir, "/", pop_prefix, "_Retinoid_discontinued_counts.rds")))
    # Clean up denominator 
    discontinued_count<-discontinued_count[,c("YM", "N")] 
    # Rename variables
    setnames(discontinued_count,"N", "Freq")
    
    # Merge switch count with prevalence count (denom)
    switch_count2<-merge(x=switch_count, y=discontinued_count, by=c("YM"), all.x=TRUE)
    # Calculate rates
    switch_count2<-switch_count2[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][is.nan(rates)|is.na(rates), rates:=0][,rates:=rates*1000]
    # Adjust for PHARMO
    # if(is_PHARMO){switch_count2<-switch_count2[year < 2020,]} else {switch_count2<-switch_count2[year < 2021,]}
    switch_count2<-switch_count2[year < 2021,]
    # Drop columns you don't need 
    switch_count2<-switch_count2[,c("YM", "N", "Freq", "rates")]
    
    # Save switcher count/rates in g_output
    saveRDS(switch_count2, (paste0(objective2_dir,"/", pop_prefix,"_", df_switcher$ATC.altmed[i], "_switcher_counts_denom_disc.rds")))
    
    # Stratified by age 
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
        temp_count_min <- switch_count1[,c("YM","N")]
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
        saveRDS(age_group_temp_count, (paste0(objective2_strat_dir,"/", pop_prefix,"_", unique(df_episodes$ATC)[i], "_switched_counts_",  unique(temp_by_age$age_group)[group],"_age_group.rds")))
      }
    }
  }  
} else {
  print("There are no switchers from Retinoids to RAM")
}

# Clean up 
rm(list = grep("^df_|^RAM_", ls(), value = TRUE))









