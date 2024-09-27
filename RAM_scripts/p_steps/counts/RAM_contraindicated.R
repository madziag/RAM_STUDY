# Get contraindicated codes
source(paste0(pre_dir, "parameters/ATC_for_contra_and_teratogenic.R"))

if (file.exists(paste0(objective3_temp_dir, pop_prefix,"_RAM_general_concomit_RECORDS_data.rds"))) {
  
  #############################################################################################
  ################################## Data Preparation #########################################
  #############################################################################################
  ### Data Loading
  # RAM Concomitance Data 
  RAM_concomitance_data<-as.data.table(readRDS(paste0(objective3_temp_dir, pop_prefix,"_RAM_general_concomit_RECORDS_data.rds")))
  
  #############################################################################################
  ##### Per Record ############################################################################
  #############################################################################################
  # Remove true duplicates
  RAM_concomitance_RECORDS<-unique(RAM_concomitance_data) #DENOMINATOR
  # Get contraindicated ATC subset
  RAM_contra_RECORDS<-RAM_concomitance_RECORDS[ATC.RAM %in%contraindicated_codes,] #NUMERATOR
  
  # counts for both general concomit and contras (RECORDS) 
  RAM_concomitance_RECORDS_counts<-RAM_concomitance_RECORDS[,.N, by = .(year,month)]#DENOMINATOR
  RAM_contra_RECORDS_counts<-RAM_contra_RECORDS[,.N, by = .(year,month)] #NUMERATOR
  
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  RAM_concomitance_RECORDS_counts<-as.data.table(merge(x=empty_df,y=RAM_concomitance_RECORDS_counts,by=c("year","month"),all.x=TRUE)) #DENOMINATOR
  RAM_contra_RECORDS_counts<-as.data.table(merge(x=empty_df,y=RAM_contra_RECORDS_counts,by=c("year","month"),all.x=TRUE)) #NUMERATOR
  
  # Fills in missing values with 0
  RAM_concomitance_RECORDS_counts[is.na(N), N:=0] #DENOMINATOR
  RAM_contra_RECORDS_counts[is.na(N), N:=0] #NUMERATOR
  
  # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
  RAM_concomitance_RECORDS_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]#DENOMINATOR
  RAM_contra_RECORDS_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]#NUMERATOR
  
  # Masking is not applied before stratification
  RAM_concomitance_RECORDS_counts[,masked:=0]#DENOMINATOR
  RAM_contra_RECORDS_counts[,masked:=0]#NUMERATOR
  
  # Create YM variable 
  RAM_concomitance_RECORDS_counts<-within(RAM_concomitance_RECORDS_counts, YM<- sprintf("%d-%02d", year, month))#DENOMINATOR
  RAM_contra_RECORDS_counts<-within(RAM_contra_RECORDS_counts, YM<- sprintf("%d-%02d", year, month))#NUMERATOR
  
  # Adjust for PHARMO
  if(is_PHARMO){RAM_concomitance_RECORDS_counts<-RAM_concomitance_RECORDS_counts[year < 2020,]}else{RAM_concomitance_RECORDS_counts<-RAM_concomitance_RECORDS_counts[year < 2021,]}#DENOMINATOR
  if(is_PHARMO){RAM_contra_RECORDS_counts<-RAM_contra_RECORDS_counts[year<2020,]}else{RAM_contra_RECORDS_counts<-RAM_contra_RECORDS_counts[year<2021,]}
  
  # Rename N variable to Freq
  setnames(RAM_concomitance_RECORDS_counts, "N", "Freq")
  
  # Merge files
  # Numerator=> Number of contraindicated RAM users in concomitance with Retinoids
  # Denominator => All users who have RAM in concomitance with Retinoid
  RAM_contra_RECORDS_rates<-merge(x= RAM_contra_RECORDS_counts[,c("YM","N","masked","true_value")], y = RAM_concomitance_RECORDS_counts[,c("YM", "Freq")], by = c("YM"), all.x = TRUE)
  # Calculates rates
  RAM_contra_RECORDS_rates<-RAM_contra_RECORDS_rates[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][is.nan(rates)|is.na(rates), rates:=0]
  # Keeps necessary columns 
  RAM_contra_RECORDS_rates<-RAM_contra_RECORDS_rates[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
  # Saves files in medicine counts folder
  saveRDS(RAM_contra_RECORDS_rates, paste0(objective3_dir,"/", pop_prefix, "_RAM_contra_RECORDS_counts.rds")) 
  # Saves data set 
  saveRDS(RAM_contra_RECORDS, paste0(objective3_temp_dir, pop_prefix, "_RAM_contra_RECORDS_data.rds")) 
 
   #############################################################################################
  ##### Per User ##############################################################################
  #############################################################################################
  RAM_concomitance_USERS<-unique(RAM_concomitance_data, by=c("person_id", "year", "month"))
  # Get contraindicated ATC subset
  RAM_contra_USERS<-RAM_concomitance_USERS[ATC.RAM %in%contraindicated_codes,] #NUMERATOR
  
  # Total concomitance counts per user (for all concomitant users and for concomitant users in contraindicated ATCs)
  RAM_concomitance_USERS_counts<-RAM_concomitance_USERS[,.N, by = .(year,month)]
  RAM_contra_USERS_counts<-RAM_contra_USERS[,.N, by = .(year,month)]
  
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  RAM_concomitance_USERS_counts<-as.data.table(merge(x = empty_df, y = RAM_concomitance_USERS_counts, by = c("year", "month"), all.x = TRUE))
  RAM_contra_USERS_counts<-as.data.table(merge(x = empty_df, y = RAM_contra_USERS_counts, by = c("year", "month"), all.x = TRUE))
  
  # Fills in missing values with 0
  RAM_concomitance_USERS_counts[is.na(N), N:=0]
  RAM_contra_USERS_counts[is.na(N), N:=0]
  
  # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
  RAM_concomitance_USERS_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
  RAM_contra_USERS_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
  
  # Masking is not applied before stratification
  RAM_concomitance_USERS_counts[,masked:=0]
  RAM_contra_USERS_counts[,masked:=0]
  
  # Create YM variable 
  RAM_concomitance_USERS_counts<-within(RAM_concomitance_USERS_counts, YM<- sprintf("%d-%02d", year, month))
  RAM_contra_USERS_counts<-within(RAM_contra_USERS_counts, YM<- sprintf("%d-%02d", year, month))
  
  # Adjust for PHARMO
  if(is_PHARMO){RAM_concomitance_USERS_counts<-RAM_concomitance_USERS_counts[year < 2020,]} else {RAM_concomitance_USERS_counts<-RAM_concomitance_USERS_counts[year < 2021,]}
  if(is_PHARMO){RAM_contra_USERS_counts<-RAM_contra_USERS_counts[year < 2020,]} else {RAM_contra_USERS_counts<-RAM_contra_USERS_counts[year < 2021,]}
  
  # Rename N variable to Freq
  setnames(RAM_concomitance_USERS_counts, "N", "Freq")
  
  # Merge the two files 
  # Numerator=> Number of contraindicated RAM users in concomitance with Retinoids
  # Denominator => All users who have RAM in concomitance with Retinoid
  RAM_contra_USERS_rates<-merge(x=RAM_contra_USERS_counts[,c("YM","N","masked","true_value")],y =RAM_concomitance_USERS_counts[,c("YM", "Freq")], by = c("YM"), all.x = TRUE)
  # Calculates rates
  RAM_contra_USERS_rates<-RAM_contra_USERS_rates[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][is.nan(rates)|is.na(rates), rates:=0]
  # Keeps necessary columns 
  RAM_contra_USERS_rates<-RAM_contra_USERS_rates[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
  # Saves files in medicine counts folder
  saveRDS(RAM_contra_USERS_rates, paste0(objective3_dir,"/", pop_prefix, "_RAM_contra_USERS_counts.rds")) 
  saveRDS(RAM_contra_USERS, paste0(objective3_temp_dir, pop_prefix, "_RAM_contra_USERS_data.rds")) 
  
  } else {
  
  print("There are no records for retinoid-RAM concomitance")
  RAM_flowchart_concomit_users<-0
  RAM_flowchart_concomit_users_contraindicated<-0
  RAM_flowchart_concomit_records<-0
  RAM_flowchart_concomit_records_contraindicated<-0
}

# Clean up 
rm(list = grep("RAM_concomit", ls(), value = TRUE))
