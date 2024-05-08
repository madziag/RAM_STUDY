if (file.exists(paste0(objective3_temp_dir, pop_prefix,"_RAM_general_concomit_data.rds"))) {
  
  #############################################################################################
  ################################## Data Preparation #########################################
  #############################################################################################
  ### Data Loading
  # RAM Concomitance Data 
  RAM_concomitance_data<-as.data.table(readRDS(paste0(objective3_temp_dir, pop_prefix,"_RAM_general_concomit_data.rds")))
  # Get contraindicated ATC subset
  RAM_concomitance_data_contraindicated<-RAM_concomitance_data[ATC.RAM %in%contraindicated_codes,]
  
  #############################################################################################
  ##### Per User ##############################################################################
  #############################################################################################
  
  RAM_concomitance_data_per_user<-unique(RAM_concomitance_data, by=c("person_id", "year", "month"))
  RAM_concomitance_data_per_user_contraindicated<-unique(RAM_concomitance_data_contraindicated, by=c("person_id", "year", "month"))
  
  #flowchart
  if(length(unique(RAM_concomitance_data_per_user$person_id))){RAM_flowchart_concomit_users<-length(unique(RAM_concomitance_data_per_user$person_id))}else{RAM_flowchart_concomit_users<-0}
  if(length(unique(RAM_concomitance_data_per_user_contraindicated$person_id))){RAM_flowchart_concomit_users_contraindicated<-length(unique(RAM_concomitance_data_per_user_contraindicated$person_id))}else{RAM_flowchart_concomit_users_contraindicated<-0}
  
  
  # Total concomitance counts per user (for all concomitant users and for concomitant users in contraindicated ATCs)
  RAM_concomitance_data_per_user_counts<-RAM_concomitance_data_per_user[,.N, by = .(year,month)]
  RAM_concomitance_data_per_user_contraindicated_counts<-RAM_concomitance_data_per_user_contraindicated[,.N, by = .(year,month)]
  
  # Adjust for PHARMO
  if(is_PHARMO){RAM_concomitance_data_per_user_counts<-RAM_concomitance_data_per_user_counts[year < 2020,]} else {RAM_concomitance_data_per_user_counts<-RAM_concomitance_data_per_user_counts[year < 2021,]}
  if(is_PHARMO){RAM_concomitance_data_per_user_contraindicated_counts<-RAM_concomitance_data_per_user_contraindicated_counts[year < 2020,]} else {RAM_concomitance_data_per_user_contraindicated_counts<-RAM_concomitance_data_per_user_contraindicated_counts[year < 2021,]}
  
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  RAM_concomitance_data_per_user_counts<-as.data.table(merge(x = empty_df, y = RAM_concomitance_data_per_user_counts, by = c("year", "month"), all.x = TRUE))
  RAM_concomitance_data_per_user_contraindicated_counts<-as.data.table(merge(x = empty_df, y = RAM_concomitance_data_per_user_contraindicated_counts, by = c("year", "month"), all.x = TRUE))
  
  # Fills in missing values with 0
  RAM_concomitance_data_per_user_counts[is.na(N), N:=0]
  RAM_concomitance_data_per_user_contraindicated_counts[is.na(N), N:=0]
  
  # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
  RAM_concomitance_data_per_user_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
  RAM_concomitance_data_per_user_contraindicated_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
  
  # Masking is not applied before stratification
  RAM_concomitance_data_per_user_counts[,masked:=0]
  RAM_concomitance_data_per_user_contraindicated_counts[,masked:=0]
  
  # Create YM variable 
  RAM_concomitance_data_per_user_counts<-within(RAM_concomitance_data_per_user_counts, YM<- sprintf("%d-%02d", year, month))
  RAM_concomitance_data_per_user_contraindicated_counts<-within(RAM_concomitance_data_per_user_contraindicated_counts, YM<- sprintf("%d-%02d", year, month))
  
  # Rename N variable to Freq
  setnames(RAM_concomitance_data_per_user_counts, "N", "Freq")
  
  # Merge the two files 
  # Numerator=> Number of contraindicated RAM users in concomitance with Retinoids
  # Denominator => All users who have RAM in concomitance with Retinoid
  RAM_concomit_rates_contraindicated_per_user<-merge(x = RAM_concomitance_data_per_user_contraindicated_counts[,c("YM","N","masked","true_value")], y = RAM_concomitance_data_per_user_counts[,c("YM", "Freq")], by = c("YM"), all.x = TRUE)
  # Calculates rates
  RAM_concomit_rates_contraindicated_per_user<-RAM_concomit_rates_contraindicated_per_user[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][is.nan(rates)|is.na(rates), rates:=0]
  # Keeps necessary columns 
  RAM_concomit_rates_contraindicated_per_user<-RAM_concomit_rates_contraindicated_per_user[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
  # Saves files in medicine counts folder
  saveRDS(RAM_concomit_rates_contraindicated_per_user, paste0(objective3_dir,"/", pop_prefix, "_RAM_contraindicated_users_in_concomitance.rds")) 
  
  #############################################################################################
  ##### Per Record ##############################################################################
  #############################################################################################
  # Remove true duplicates
  RAM_concomitance_data<-unique(RAM_concomitance_data)
  RAM_concomitance_data_contraindicated<-unique(RAM_concomitance_data_contraindicated)
  
  # Flowchart
  RAM_flowchart_concomit_records<-nrow(RAM_concomitance_data)
  RAM_flowchart_concomit_records_contraindicated<-nrow(RAM_concomitance_data_contraindicated)
  
  # Total concomitance counts per user (for all concomitant users and for concomitant users in contraindicated ATCs)
  RAM_concomitance_data_per_record_counts<-RAM_concomitance_data[,.N, by = .(year,month)]
  RAM_concomitance_data_per_record_contraindicated<-RAM_concomitance_data_contraindicated[,.N, by = .(year,month)]
  
  # Adjust for PHARMO
  if(is_PHARMO){
    RAM_concomitance_data_per_record_counts<-RAM_concomitance_data_per_record_counts[year < 2020,]
  }else{
      RAM_concomitance_data_per_record_counts<-RAM_concomitance_data_per_record_counts[year < 2021,]
  }
  if(is_PHARMO){
    RAM_concomitance_data_per_record_contraindicated<-RAM_concomitance_data_per_record_contraindicated[year<2020,]
    }else{
    RAM_concomitance_data_per_record_contraindicated<-RAM_concomitance_data_per_record_contraindicated[year<2021,]
    }
  
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  RAM_concomitance_data_per_record_counts<-as.data.table(merge(x = empty_df, y = RAM_concomitance_data_per_record_counts, by = c("year", "month"), all.x = TRUE))
  RAM_concomitance_data_per_record_contraindicated<-as.data.table(merge(x = empty_df, y = RAM_concomitance_data_per_record_contraindicated, by = c("year", "month"), all.x = TRUE))
  
  # Fills in missing values with 0
  RAM_concomitance_data_per_record_counts[is.na(N), N:=0]
  RAM_concomitance_data_per_record_contraindicated[is.na(N), N:=0]
  
  # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
  RAM_concomitance_data_per_record_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
  RAM_concomitance_data_per_record_contraindicated[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
  
  # Masking is not applied before stratification
  RAM_concomitance_data_per_record_counts[,masked:=0]
  RAM_concomitance_data_per_record_contraindicated[,masked:=0]
  
  # Create YM variable 
  RAM_concomitance_data_per_record_counts<-within(RAM_concomitance_data_per_record_counts, YM<- sprintf("%d-%02d", year, month))
  RAM_concomitance_data_per_record_contraindicated<-within(RAM_concomitance_data_per_record_contraindicated, YM<- sprintf("%d-%02d", year, month))
  
  # Rename N variable to Freq
  setnames(RAM_concomitance_data_per_record_counts, "N", "Freq")
  
  # Merge the two files 
  # Numerator=> Number of contraindicated RAM users in concomitance with Retinoids
  # Denominator => All users who have RAM in concomitance with Retinoid
  RAM_concomit_rates_contraindicated_per_record<-merge(x = RAM_concomitance_data_per_record_contraindicated[,c("YM","N","masked","true_value")], y = RAM_concomitance_data_per_record_counts[,c("YM", "Freq")], by = c("YM"), all.x = TRUE)
  # Calculates rates
  RAM_concomit_rates_contraindicated_per_record<-RAM_concomit_rates_contraindicated_per_record[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][is.nan(rates)|is.na(rates), rates:=0]
  # Keeps necessary columns 
  RAM_concomit_rates_contraindicated_per_record<-RAM_concomit_rates_contraindicated_per_record[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
  # Saves files in medicine counts folder
  saveRDS(RAM_concomit_rates_contraindicated_per_record, paste0(objective3_dir,"/", pop_prefix, "_RAM_contraindicated_records_in_concomitance.rds")) 
  
} else {
  
  print("There are no records for retinoid-RAM concomitance")
  RAM_flowchart_concomit_users<-0
  RAM_flowchart_concomit_users_contraindicated<-0
  RAM_flowchart_concomit_records<-0
  RAM_flowchart_concomit_records_contraindicated<-0
}

# Clean up 
rm(list = grep("RAM_concomit", ls(), value = TRUE))
