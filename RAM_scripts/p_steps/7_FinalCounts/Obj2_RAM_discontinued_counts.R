#############################################################################################
#############################################################################################
######################################## Objective 2 ########################################
#############################################################################################
#############################################################################################

##### Monthly discontinuation rates (IR) of RAM #####
## Conditions 
# a. Not receiving any other prescription or dispensing of the same drug within 90 days after the end date of the last treatment episode
# Numerator = number of subjects discontinuing RAM per calendar month
# Denominator = number of all RAM users during the same month (at least one day of exposure that month) (prevalence)
# Unit = person-month.  


### 1. Load needed records: 
# RAM treatment episode files 
df_episodes<-as.data.table(readRDS(paste0(g_intermediate,"treatment_episodes_alt_meds/", pop_prefix, "_AltMed_CMA_treatment_episodes.rds")))

# Denominator 
source(paste0(pre_dir,"4_Denominator/load_denominator.R"))

# RAM prevalent files 
RAM_prevalence_files<-list.files(objective1_temp_dir, pattern = "prevalence") # get list of incidence files 
RAM_prevalence_files<-RAM_prevalence_files[grepl(pop_prefix, RAM_prevalence_files)] # filter by subpop (BIFAP)
if(populations[pop] == "PC_study_population.rds"){RAM_prevalence_files<-RAM_prevalence_files[!grepl("PC_HOSP", RAM_prevalence_files)]}
RAM_prevalence_list<-lapply(paste0(objective1_temp_dir, RAM_prevalence_files), readRDS) # Read in all the files 

df_RAM_prevalence<-as.data.table(rbindlist(RAM_prevalence_list)) # Create df that binds all RAM Incidence files 


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
  
  #######################################################################################################
  ######################################## Discontinuation Calculation ##################################
  #######################################################################################################
  # Order data by person id and episode start
  df_discontinued<-df_episodes_subset[order(person_id, episode.start)]
  # Get the discontinued -> if next episode.start is more than 90 days (discontinuation window) from episode.end of previous episode.end
  df_discontinued[,RAM.discontinued:=ifelse(end.episode.gap.days>discontinuation_window, 1, 0)]
  # Get discontinuations only 
  df_discontinued<-df_discontinued[RAM.discontinued==1,]
  # If episode start falls outside of patients entry and exit into study dates, then remove it
  df_discontinued<-df_discontinued[episode.end>= entry_date & episode.end<=exit_date,]
  
  # Create column with patients age at episode.start
  df_discontinued[,current_age:= floor((episode.end - birth_date)*10/365.25)/10]
  # We change the min of age group 12-20.99 to 11 to assign a group to those with tx days in 2009 (otherwise it would fall in a group that is not defined)
  df_discontinued[current_age >= 11 & current_age < 21, age_group:= "12-20.99"]
  df_discontinued[current_age >= 21 & current_age < 31, age_group:= "21-30.99"]
  df_discontinued[current_age >= 31 & current_age < 41, age_group:= "31-40.99"]
  df_discontinued[current_age >= 41 & current_age < 56, age_group:= "41-55.99"]
  
  # Create year and month columns (for counts)
  df_discontinued[,year:=year(episode.end)][,month:=month(episode.end)]
  
  # Numerator 
  # Count discontinued by year and month
  discontinued_count<-df_discontinued[,.N, by = .(year,month)]
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  discontinued_count<-as.data.table(merge(x=empty_df, y=discontinued_count, by=c("year", "month"), all.x=TRUE))
  # Fills in missing values with 0
  discontinued_count[is.na(N), N:=0]
  # Creates YM variable
  discontinued_count<-within(discontinued_count,YM<-sprintf("%d-%02d",year,month))
  
  # Denominator
  # Count prevalence by year and month
  prevalence_count_all <-df_RAM_prevalence[,.N, by = .(year,month)]
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  prevalence_count_all<-as.data.table(merge(x=empty_df, y=prevalence_count_all, by=c("year", "month"), all.x=TRUE))
  # Fills in missing values with 0
  prevalence_count_all[is.na(N), N:=0]
  # Rename columns
  setnames(prevalence_count_all, "N", "Freq")
  
  # Merge Numerator with denominator 
  # Merge incident count with denominator file
  discontinued_count<-merge(x=discontinued_count, y=prevalence_count_all, by=c("year", "month"), all.x=TRUE)
  
  # Calculate rates
  discontinued_count<-discontinued_count[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
  # Adjust for PHARMO
  # if(is_PHARMO){discontinued_count<-discontinued_count[year < 2020,]} else {discontinued_count<-discontinued_count[year < 2021,]}
  discontinued_count<-discontinued_count[year < 2021,]
  # Drop columns you don't need 
  discontinued_count<-discontinued_count[,c("YM", "N", "Freq", "rates")]
  
  # Save patient level discontinued records in g_intermediate
  saveRDS(df_discontinued, (paste0(objective2_temp_dir,"/", pop_prefix,"_", unique(df_episodes$ATC)[i], "_discontinued_counts_df.rds")))
  # Save incident count in g_output
  saveRDS(discontinued_count, (paste0(objective2_dir,"/", pop_prefix,"_", unique(df_episodes$ATC)[i], "_discontinued_counts.rds")))
  
  ################ Stratified by Age Groups ###################
  if(nrow(df_discontinued)>0){
    # Count discontinued by age, month, year
    discontinued_by_age<-df_discontinued[,.N, by = .(year,month, age_group)]
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
      discontinued_count_min <- discontinued_count[,c("YM","N")]
      setnames(discontinued_count_min,"N","Freq")
      # Merge age-group subset count with all discontinued counts 
      age_group_discontinued_count<-merge(x=each_group,y=discontinued_count_min,by=c("YM"),all.x=TRUE)
      # Masking set at 0
      age_group_discontinued_count[,masked:=0]
      # If masking applies
      if(mask==T){age_group_discontinued_count[N>0&N<5,N:=5][Freq>0&Freq<5,Freq:=5][,masked:=1]}
      # Calculates rates
      age_group_discontinued_count<-age_group_discontinued_count[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates),rates:=0]
      # Adjust for PHARMO
      # if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
      each_group<-each_group[year<2021,]
      # Drop columns you don't need 
      age_group_discontinued_count<-age_group_discontinued_count[,c("YM","N","Freq","rates","masked")]
      
      # Save files in medicine counts folder
      saveRDS(age_group_discontinued_count, (paste0(objective2_strat_dir,"/", pop_prefix,"_", unique(df_episodes$ATC)[i], "_discontinued_counts_",  unique(discontinued_by_age$age_group)[group],"_age_group.rds")))
    }
  }
}

# Clean up 
rm(list = grep("^age_group|^df_|^discontinued|^each_group|^prevalence|^RAM", ls(), value = TRUE))


