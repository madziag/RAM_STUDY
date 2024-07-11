#############################################################################################
################################## Data Preparation #########################################
#############################################################################################
### Data Loading
# RAM prescriptions 
RAM_meds<-as.data.table(do.call(rbind,lapply(paste0(medications_pop, list.files(medications_pop, pattern=paste0(pop_prefix, "_altmed"))), readRDS)))
# Get RAM meds in Retinoid population only 
# Rename ATC columns in both Retinoid and RAM population 
setnames(RAM_meds,"Code", "ATC.RAM", skip_absent = TRUE)

# Keep RAM meds that occur in Retinoid users only, and only if RAM prescriptions occur after Retinoid incidence use
# Read in retinoid incidence data
retinoid_prevalence_data<-as.data.table(readRDS(paste0(retinoid_counts_dfs, pop_prefix,"_Retinoid_prevalence_data.rds")))
# Rename episode start column
setnames(retinoid_prevalence_data,"episode.start","episode.start.retinoid") 
# Get first retinoid use in entry into study (could be incidence or prevalent use)
retinoid_prevalence_data<-unique(retinoid_prevalence_data, by=c("person_id"))
# Merge retinoid prevalence data with RAM meds
# Merge these incident retinoid treatment episodes with RAM episodes so that we have both Retinoid and RAM dates per row
RAM_meds<-merge(retinoid_prevalence_data[,c("person_id","episode.start.retinoid")],RAM_meds,by="person_id")
# Keep RAM_meds that occurred after the start of a Retinoid 
RAM_meds<-RAM_meds[Date>=episode.start.retinoid,]

# Create year-months columns based on episode.day
RAM_meds[,year:=year(Date)][,month:=month(Date)][,episode.start.retinoid:=NULL]
# Get contraindicated ATC subset
RAM_meds_teratogenic<-RAM_meds[ATC.RAM %in% teratogenic_codes,]

if (nrow(RAM_meds_teratogenic)>0) { 
  
  saveRDS(RAM_meds_teratogenic,paste0(objective4_temp_dir, pop_prefix, "_RAM_meds_teratogenic.rds"))
  
  #############################################################################################
  ##### Per User ##############################################################################
  #############################################################################################
  RAM_meds_per_user<-unique(RAM_meds, by=c("person_id", "year", "month"))
  RAM_meds_per_user_teratogenic<-unique(RAM_meds_teratogenic, by=c("person_id", "year", "month"))
  
  # Total concomitance counts per user (for all concomitant users and for concomitant users in contraindicated ATCs)
  RAM_meds_per_user_counts<-RAM_meds_per_user[,.N, by = .(year,month)]
  RAM_meds_per_user_teratogenic_counts<-RAM_meds_per_user_teratogenic[,.N, by = .(year,month)]
  
  # Adjust for PHARMO
  if(is_PHARMO){RAM_meds_per_user_counts<-RAM_meds_per_user_counts[year < 2020,]} else {RAM_meds_per_user_counts<-RAM_meds_per_user_counts[year < 2021,]}
  if(is_PHARMO){RAM_meds_per_user_teratogenic_counts<-RAM_meds_per_user_teratogenic_counts[year < 2020,]} else {RAM_meds_per_user_teratogenic_counts<-RAM_meds_per_user_teratogenic_counts[year < 2021,]}
  
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  RAM_meds_per_user_counts<-as.data.table(merge(x = empty_df, y = RAM_meds_per_user_counts, by = c("year", "month"), all.x = TRUE))
  RAM_meds_per_user_teratogenic_counts<-as.data.table(merge(x = empty_df, y = RAM_meds_per_user_teratogenic_counts, by = c("year", "month"), all.x = TRUE))
  
  # Fills in missing values with 0
  RAM_meds_per_user_counts[is.na(N), N:=0]
  RAM_meds_per_user_teratogenic_counts[is.na(N), N:=0]
  
  # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
  RAM_meds_per_user_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
  RAM_meds_per_user_teratogenic_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
  
  # Masking is not applied before stratification
  RAM_meds_per_user_counts[,masked:=0]
  RAM_meds_per_user_teratogenic_counts[,masked:=0]
  
  # Create YM variable 
  RAM_meds_per_user_counts<-within(RAM_meds_per_user_counts, YM<- sprintf("%d-%02d", year, month))
  RAM_meds_per_user_teratogenic_counts<-within(RAM_meds_per_user_teratogenic_counts, YM<- sprintf("%d-%02d", year, month))
  
  # Rename N variable to Freq
  setnames(RAM_meds_per_user_counts, "N", "Freq")
  
  # Merge the two files 
  # Numerator=> Number of teratogenic users 
  # Denominator => All users who have RAM in concomitance with Retinoid
  RAM_teratogenic_rates_per_user<-merge(x = RAM_meds_per_user_teratogenic_counts[,c("YM","N","masked","true_value")], y = RAM_meds_per_user_counts[,c("YM", "Freq")], by = c("YM"), all.x = TRUE)
  # Calculates rates
  RAM_teratogenic_rates_per_user<-RAM_teratogenic_rates_per_user[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][is.nan(rates)|is.na(rates), rates:=0]
  # Keeps necessary columns 
  RAM_teratogenic_rates_per_user<-RAM_teratogenic_rates_per_user[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
  # Saves files in medicine counts folder
  saveRDS(RAM_teratogenic_rates_per_user, paste0(objective4_dir,"/", pop_prefix, "_RAM_teratogenic_per_user_counts.rds")) 
  
  #############################################################################################
  ##### Per Records ##############################################################################
  #############################################################################################
  # Remove true duplicates
  RAM_meds<-unique(RAM_meds)
  RAM_meds_teratogenic<-unique(RAM_meds_teratogenic)
  
  # Total concomitance counts per record (for all concomitant records and for concomitant records in contraindicated ATCs)
  RAM_meds_per_record_counts<-RAM_meds[,.N, by = .(year,month)]
  RAM_meds_per_record_teratogenic_counts<-RAM_meds_teratogenic[,.N, by = .(year,month)]
  
  # Adjust for PHARMO
  if(is_PHARMO){RAM_meds_per_record_counts<-RAM_meds_per_record_counts[year < 2020,]} else {RAM_meds_per_record_counts<-RAM_meds_per_record_counts[year < 2021,]}
  if(is_PHARMO){RAM_meds_per_record_teratogenic_counts<-RAM_meds_per_record_teratogenic_counts[year < 2020,]} else {RAM_meds_per_record_teratogenic_counts<-RAM_meds_per_record_teratogenic_counts[year < 2021,]}
  
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  RAM_meds_per_record_counts<-as.data.table(merge(x = empty_df, y = RAM_meds_per_record_counts, by = c("year", "month"), all.x = TRUE))
  RAM_meds_per_record_teratogenic_counts<-as.data.table(merge(x = empty_df, y = RAM_meds_per_record_teratogenic_counts, by = c("year", "month"), all.x = TRUE))
  
  # Fills in missing values with 0
  RAM_meds_per_record_counts[is.na(N), N:=0]
  RAM_meds_per_record_teratogenic_counts[is.na(N), N:=0]
  
  # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
  RAM_meds_per_record_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
  RAM_meds_per_record_teratogenic_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
  
  # Masking is not applied before stratification
  RAM_meds_per_record_counts[,masked:=0]
  RAM_meds_per_record_teratogenic_counts[,masked:=0]
  
  # Create YM variable 
  RAM_meds_per_record_counts<-within(RAM_meds_per_record_counts, YM<- sprintf("%d-%02d", year, month))
  RAM_meds_per_record_teratogenic_counts<-within(RAM_meds_per_record_teratogenic_counts, YM<- sprintf("%d-%02d", year, month))
  
  # Rename N variable to Freq
  setnames(RAM_meds_per_record_counts, "N", "Freq")
  
  # Merge the two files 
  # Numerator=> Number of contraindicated RAM records in concomitance with Retinoids
  # Denominator => All records who have RAM in concomitance with Retinoid
  RAM_teratogenic_rates_per_record<-merge(x = RAM_meds_per_record_teratogenic_counts[,c("YM","N","masked","true_value")], y = RAM_meds_per_record_counts[,c("YM", "Freq")], by = c("YM"), all.x = TRUE)
  # Calculates rates
  RAM_teratogenic_rates_per_record<-RAM_teratogenic_rates_per_record[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][is.nan(rates)|is.na(rates), rates:=0]
  # Keeps necessary columns 
  RAM_teratogenic_rates_per_record<-RAM_teratogenic_rates_per_record[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
  # Saves files in medicine counts folder
  saveRDS(RAM_teratogenic_rates_per_record, paste0(objective4_dir,"/", pop_prefix, "_RAM_teratogenic_per_record_counts.rds")) 
  
  # Clean up 
  rm(list = grep("age_group|each_group|RAM_concomit|RAM_meds|RAM_rates|RAM_teratogenic", ls(), value = TRUE))
  
  
} else {
  print("There are no records for retinoid-RAM concomitance")
  RAM_flowchart_teratogenic_users<-0
  RAM_flowchart_teratogenic_records<-0
}
