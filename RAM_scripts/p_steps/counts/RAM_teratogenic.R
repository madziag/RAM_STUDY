#############################################################################################
################################## Teratogenic Users ########################################
#############################################################################################
### Data Loading
RAM_prevalence_data<-as.data.table(readRDS(paste0(objective1_temp_dir, pop_prefix, "_RAM_prevalence_data.rds")))
setnames(RAM_prevalence_data,old=c("episode.start"), new=("episode.start.RAM"))
RAM_teratogenic_data<-RAM_prevalence_data[ATC.RAM %in% teratogenic_codes,]
# Keep only records that occur after retinoid (1st RAM prescription of RAM)
# Keep RAM meds that occur in Retinoid users only, and only if RAM prescriptions occur after Retinoid incidence use
# Read in retinoid incidence data
retinoid_prevalence_data<-as.data.table(readRDS(paste0(retinoid_counts_dfs, pop_prefix,"_Retinoid_prevalence_data.rds")))
# Rename episode start column
setnames(retinoid_prevalence_data,"episode.start","episode.start.retinoid") 
# Get first retinoid episode - so that any RAM's occurring before first ever retinoid record is removed 
retinoid_prevalence_data_first<-retinoid_prevalence_data[order(episode.start.retinoid), .SD[1], by = person_id]
# Merge retinoid prevalence_first data with RAM meds
# Merge these incident retinoid treatment episodes with RAM episodes so that we have both Retinoid and RAM dates per row
RAM_teratogenic_data<-merge(retinoid_prevalence_data_first[,c("person_id","episode.start.retinoid","ATC.retinoid")],RAM_teratogenic_data,by="person_id")
# Keep RAM_meds that occurred after the start of a Retinoid 
RAM_teratogenic_data<-RAM_teratogenic_data[episode.day>=episode.start.retinoid,]

if(nrow(RAM_teratogenic_data)>0){
  
  # Counts 
  RAM_teratogenic_USER_counts<-RAM_teratogenic_data[,.N, by = .(year,month)]
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  RAM_teratogenic_USER_counts<-as.data.table(merge(x = empty_df, y = RAM_teratogenic_USER_counts, by = c("year", "month"), all.x = TRUE))
  # Fills in missing values with 0
  RAM_teratogenic_USER_counts[is.na(N), N:=0]
  # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
  RAM_teratogenic_USER_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
  # Masking is not applied before stratification
  RAM_teratogenic_USER_counts[,masked:=0]
  # Adjust for PHARMO
  if(is_PHARMO){RAM_teratogenic_USER_counts<-RAM_teratogenic_USER_counts[year < 2020,]} else {RAM_teratogenic_USER_counts<-RAM_teratogenic_USER_counts[year < 2021,]}
  # Create YM variable 
  RAM_teratogenic_USER_counts<-within(RAM_teratogenic_USER_counts, YM<- sprintf("%d-%02d", year, month))
  
  # Denominator : RAM Prevalence 
  RAM_prevalence_counts<-as.data.table(readRDS(paste0(objective1_dir, "/", pop_prefix, "_RAM_prevalence_counts.rds")))
  RAM_prevalence_counts<-RAM_prevalence_counts[,c("YM", "N")]
  # Rename N variable to Freq
  setnames(RAM_prevalence_counts, "N", "Freq")
  # Merge the two files 
  # Numerator=> Number of teratogenic users 
  # Denominator => RAM prevalence
  RAM_teratogenic_USER_rates<-merge(x=RAM_teratogenic_USER_counts[,c("YM","N","masked","true_value")], y = RAM_prevalence_counts[,c("YM", "Freq")], by = c("YM"), all.x = TRUE)
  # Calculates rates
  RAM_teratogenic_USER_rates<-RAM_teratogenic_USER_rates[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][is.nan(rates)|is.na(rates), rates:=0]
  # Keeps necessary columns 
  RAM_teratogenic_USER_rates<-RAM_teratogenic_USER_rates[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
  # Saves files in medicine counts folder
  saveRDS(RAM_teratogenic_USER_rates, paste0(objective4_dir,"/", pop_prefix, "_RAM_teratogenic_USERS_counts.rds")) 
  saveRDS(RAM_teratogenic_data, paste0(objective4_temp_dir, pop_prefix, "_RAM_teratogenic_USERS_data.rds"))
  } else {
  print("There is NO TERATOGENIC DATA ")
}

#############################################################################################
##### Per Records ###########################################################################
#############################################################################################
# RAM prescriptions 
RAM_meds_WOCBP<-as.data.table(do.call(rbind,lapply(paste0(medications_pop, list.files(medications_pop, pattern=paste0(pop_prefix, "_altmed"))), readRDS)))
# Get RAM meds in Retinoid population only 
# Rename ATC columns in both Retinoid and RAM population 
setnames(RAM_meds_WOCBP,"Code", "ATC.RAM", skip_absent = TRUE)

# Merge retinoid prevalence data with RAM meds
# Merge these incident retinoid treatment episodes with RAM episodes so that we have both Retinoid and RAM dates per row
RAM_meds_retinoid_subpop<-merge(retinoid_prevalence_data_first[,c("person_id","episode.start.retinoid")],RAM_meds_WOCBP,by="person_id")
# Keep RAM_meds that occurred after the start of a Retinoid 
RAM_meds_retinoid_subpop<-RAM_meds_retinoid_subpop[Date>=episode.start.retinoid,]

# Create year-months columns based on episode.day
RAM_meds_retinoid_subpop[,year:=year(Date)][,month:=month(Date)][,episode.start.retinoid:=NULL]

# Get teratogenic ATC subset
RAM_meds_teratogenic<-RAM_meds_retinoid_subpop[ATC.RAM %in% teratogenic_codes,]
RAM_flowchart_teratogenic_users<-length(unique(RAM_meds_teratogenic$person_id))
RAM_flowchart_teratogenic_records<-nrow(RAM_meds_teratogenic)

if (nrow(RAM_meds_teratogenic)>0) { 
  
  # Remove true duplicates
  RAM_meds_retinoid_subpop<-unique(RAM_meds_retinoid_subpop) # Denominator 
  RAM_meds_teratogenic_RECORDS<-unique(RAM_meds_teratogenic) # Numerator 
  
  # Perform counts of each 
  RAM_all_meds_RECORD_counts<-RAM_meds_retinoid_subpop[,.N, by = .(year,month)] # Denominator
  RAM_teratogenic_RECORD_counts<-RAM_meds_teratogenic_RECORDS[,.N, by = .(year,month)] # Numerator 
  
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  RAM_all_meds_RECORD_counts<-as.data.table(merge(x = empty_df, y = RAM_all_meds_RECORD_counts, by = c("year", "month"), all.x = TRUE))
  RAM_teratogenic_RECORD_counts<-as.data.table(merge(x = empty_df, y = RAM_teratogenic_RECORD_counts, by = c("year", "month"), all.x = TRUE))
  
  # Fills in missing values with 0
  RAM_all_meds_RECORD_counts[is.na(N), N:=0]
  RAM_teratogenic_RECORD_counts[is.na(N), N:=0]
  
  # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
  RAM_all_meds_RECORD_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
  RAM_teratogenic_RECORD_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
  
  # Masking is not applied before stratification
  RAM_all_meds_RECORD_counts[,masked:=0]
  RAM_teratogenic_RECORD_counts[,masked:=0]
  
  # Adjust for PHARMO
  if(is_PHARMO){RAM_all_meds_RECORD_counts<-RAM_all_meds_RECORD_counts[year < 2020,]} else {RAM_all_meds_RECORD_counts<-RAM_all_meds_RECORD_counts[year < 2021,]}
  if(is_PHARMO){RAM_teratogenic_RECORD_counts<-RAM_teratogenic_RECORD_counts[year < 2020,]} else {RAM_teratogenic_RECORD_counts<-RAM_teratogenic_RECORD_counts[year < 2021,]}
  
  # Create YM variable 
  RAM_all_meds_RECORD_counts<-within(RAM_all_meds_RECORD_counts, YM<- sprintf("%d-%02d", year, month))
  RAM_teratogenic_RECORD_counts<-within(RAM_teratogenic_RECORD_counts, YM<- sprintf("%d-%02d", year, month))
  
  # Rename N variable to Freq
  setnames(RAM_all_meds_RECORD_counts, "N", "Freq")
  
  # Merge the two files 
  # Numerator=> Number of teratogenic records
  # Denominator => All RAM records in retinoid subpop
  RAM_teratogenic_RECORD_rates <-merge(x = RAM_teratogenic_RECORD_counts[,c("YM","N","masked","true_value")], y = RAM_all_meds_RECORD_counts[,c("YM", "Freq")], by = c("YM"), all.x = TRUE)
  # Calculates rates
  RAM_teratogenic_RECORD_rates<-RAM_teratogenic_RECORD_rates[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][is.nan(rates)|is.na(rates), rates:=0]
  # Keeps necessary columns 
  RAM_teratogenic_RECORD_rates<-RAM_teratogenic_RECORD_rates[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
  # Saves files in medicine counts folder
  saveRDS(RAM_teratogenic_RECORD_rates, paste0(objective4_dir,"/", pop_prefix, "_RAM_teratogenic_RECORDS_counts.rds")) 
  # Save teratogenic data set 
  saveRDS(RAM_meds_teratogenic,paste0(objective4_temp_dir, pop_prefix, "_RAM_teratogenic_RECORDS_data.rds"))
  
  #############################################################################################
  ##### Per User ##############################################################################
  #############################################################################################
  
  
  # Clean up 
  rm(list = grep("age_group|each_group|RAM_concomit|RAM_meds|RAM_rates|RAM_teratogenic", ls(), value = TRUE))
  
  
} else {
  print("There are no records for retinoid-RAM concomitance")
  RAM_flowchart_teratogenic_users<-0
  RAM_flowchart_teratogenic_records<-0
}
