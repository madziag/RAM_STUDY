

#############################################################################################
################################## Data Preparation #########################################
#############################################################################################
### Data Loading
## RAM Prevalent Data
RAM_prevalence_data<-as.data.table(readRDS(paste0(objective1_temp_dir, pop_prefix,"_RAM_prevalence_data.rds")))
# RAM contraindicated data
RAM_concomitance_data<-as.data.table(readRDS(paste0(objective3_temp_dir, pop_prefix,"_RAM_general_concomit_data.rds")))
# RAM prevalent counts
RAM_prevalence_counts<-as.data.table(readRDS(paste0(objective1_dir, "/", pop_prefix, "_RAM_prevalence_counts.rds")))  
# RAM prescriptions 
RAM_meds<-as.data.table(do.call(rbind,lapply(paste0(medications_pop, list.files(medications_pop, pattern=paste0(pop_prefix, "_altmed"))), readRDS)))
# 
### TESTING ###
RAM_concomitance_data[person_id=="ConCDM_SIM_200421_00018", ATC.RAM:="H02AB02"]
RAM_prevalence_data[person_id=="ConCDM_SIM_200421_00018", ATC:="H02AB02"]
### TESTING ###

### Data Cleaning 
## RAM Prevalent Data
# Get contraindicated ATC subset
RAM_prevalence_data_contraindicated<-RAM_prevalence_data[ATC %in% c("H02AB02","L04AX30","A11CA01","J01AA07"),]
# RAM contraindicated data
# Get contraindicated ATC subset
RAM_concomitance_data_contraindicated<-RAM_concomitance_data[ATC.RAM %in% c("H02AB02","L04AX30", "A11CA01", "J01AA07"),]
## Denominators 
## Retinoid Prevalent Counts
RAM_prevalence_counts<-RAM_prevalence_counts[,c("YM", "N")]   
# Rename variables
setnames(RAM_prevalence_counts,"N", "Freq")
# RAM meds 
# Get RAM occuring in Retinoid users only 
RAM_meds<-merge(retinoid_study_population[,"person_id"], RAM_meds, by="person_id")
# Get contraindicated ATC subset
RAM_meds_contraindicated<-RAM_meds[Code %in%c("H02AB02","L04AX30", "A11CA01", "J01AA07"),]

if(nrow(RAM_prevalence_data_contraindicated)>0){
  ### Counts # 1
  ### Number of users of RAM which are contraindicated in combination with an oral retinoid per calendar month
  # contraindicated RAM counts 
  RAM_prevalence_counts_contraindicated<-RAM_prevalence_data_contraindicated[,.N, by = .(year,month)]
  # Adjust for PHARMO
  if(is_PHARMO){RAM_prevalence_counts_contraindicated<-RAM_prevalence_counts_contraindicated[year < 2020,]} else {RAM_prevalence_counts_contraindicated<-RAM_prevalence_counts_contraindicated[year < 2021,]}
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  RAM_prevalence_counts_contraindicated<-as.data.table(merge(x = empty_df, y = RAM_prevalence_counts_contraindicated, by = c("year", "month"), all.x = TRUE))
  # Fills in missing values with 0
  RAM_prevalence_counts_contraindicated[is.na(N), N:=0]
  # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
  RAM_prevalence_counts_contraindicated[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
  # Masking is not applied before stratification
  RAM_prevalence_counts_contraindicated[,masked:=0]
  # Create YM variable 
  RAM_prevalence_counts_contraindicated<-within(RAM_prevalence_counts_contraindicated, YM<- sprintf("%d-%02d", year, month))
  # Denominator => Number of prevalent RAM users that month
  RAM_rates_contraindicated<-merge(x = RAM_prevalence_counts_contraindicated, y = RAM_prevalence_counts, by = c("YM"), all.x = TRUE)
  # Calculates rates
  RAM_rates_contraindicated<-RAM_rates_contraindicated[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
  # Keeps necessary columns 
  RAM_rates_contraindicated<-RAM_rates_contraindicated[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
  # Saves files in medicine counts folder
  saveRDS(RAM_rates_contraindicated, paste0(objective4_dir, "/", pop_prefix, "_RAM_counts_contraindicated.rds"))
  # Saves discontinued dfs
  saveRDS(RAM_prevalence_data_contraindicated, paste0(objective3_temp_dir, pop_prefix, "_RAM_prevalence_data_contraindicated.rds")) 
}else {
  print("There are no contraindicated RAM's in the data 1")
}

if(nrow(RAM_concomitance_data_contraindicated)>0){
  
  ### Counts # 2
  ### Number of users of RAM which are contraindicated in combination with an oral retinoid per calendar month
  # contraindicated RAM counts concomitant recotds 
  RAM_concomitance_counts_contraindicated<-RAM_concomitance_data_contraindicated[,.N, by = .(year,month)]
  # Adjust for PHARMO
  if(is_PHARMO){RAM_concomitance_counts_contraindicated<-RAM_concomitance_counts_contraindicated[year < 2020,]} else {RAM_concomitance_counts_contraindicated<-RAM_concomitance_counts_contraindicated[year < 2021,]}
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  RAM_concomitance_counts_contraindicated<-as.data.table(merge(x = empty_df, y = RAM_concomitance_counts_contraindicated, by = c("year", "month"), all.x = TRUE))
  # Fills in missing values with 0
  RAM_concomitance_counts_contraindicated[is.na(N), N:=0]
  # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
  RAM_concomitance_counts_contraindicated[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
  # Masking is not applied before stratification
  RAM_concomitance_counts_contraindicated[,masked:=0]
  # Create YM variable 
  RAM_concomitance_counts_contraindicated<-within(RAM_concomitance_counts_contraindicated, YM<- sprintf("%d-%02d", year, month))
  
  # Denominator
  # contraindicated RAM counts concomitant records 
  RAM_meds_counts_contraindicated<-RAM_meds_contraindicated[,.N, by = .(year(Date),month(Date))]
  # Adjust for PHARMO
  if(is_PHARMO){RAM_meds_counts_contraindicated<-RAM_meds_counts_contraindicated[year < 2020,]} else {RAM_meds_counts_contraindicated<-RAM_meds_counts_contraindicated[year < 2021,]}
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  RAM_meds_counts_contraindicated<-as.data.table(merge(x = empty_df, y = RAM_meds_counts_contraindicated, by = c("year", "month"), all.x = TRUE))
  # Fills in missing values with 0
  RAM_meds_counts_contraindicated[is.na(N), N:=0]
  # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
  RAM_meds_counts_contraindicated[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
  # Masking is not applied before stratification
  RAM_meds_counts_contraindicated[,masked:=0]
  # Create YM variable 
  RAM_meds_counts_contraindicated<-within(RAM_meds_counts_contraindicated, YM<- sprintf("%d-%02d", year, month))
  # Rename variable N to Freq
  setnames(RAM_meds_counts_contraindicated, "N", "Freq")
  # Merge the 2 
  # Denominator => Number of prevalent RAM users that month
  RAM_concomit_rates_contraindicated<-merge(x = RAM_concomitance_counts_contraindicated[,c("YM","N","masked", "true_value")], y = RAM_meds_counts_contraindicated[,c("YM", "Freq")], by = c("YM"), all.x = TRUE)
  # Calculates rates
  RAM_concomit_rates_contraindicated<-RAM_concomit_rates_contraindicated[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
  # Keeps necessary columns 
  RAM_concomit_rates_contraindicated<-RAM_concomit_rates_contraindicated[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
  # Saves files in medicine counts folder
  saveRDS(RAM_concomit_rates_contraindicated, paste0(objective3_dir, "/", pop_prefix, "_RAM_counts_contraindicated.rds"))
} else {
  print("There are no contraindicated RAM's in the data 2")
}
