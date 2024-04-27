

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


### Data Cleaning 
## RAM Prevalent Data
# Get teratogenic ATC subset
RAM_prevalence_data_teratogenic<-RAM_prevalence_data[ATC %in%c("L04AA32","L04AX01","L04AX07","D05BA02","L04AX03","A11CA01","J01AA07","J01FA01", "J01FA10","L04AD01", "L04AB01","L04AB02","L04AC05","L04AC10","L04AC13","L04AC12","L04AC16","L04AC17"),]
# RAM contraindicated data
# Get teratogenic ATC subset
RAM_concomitance_data_teratogenic<-RAM_concomitance_data[ATC.RAM %in%c("L04AA32","L04AX01","L04AX07","D05BA02","L04AX03","A11CA01","J01AA07","J01FA01", "J01FA10","L04AD01", "L04AB01","L04AB02","L04AC05","L04AC10","L04AC13","L04AC12","L04AC16","L04AC17"),]
## Denominators 
## Retinoid Prevalent Counts
RAM_prevalence_counts<-RAM_prevalence_counts[,c("YM", "N")]   
# Rename variables
setnames(RAM_prevalence_counts,"N", "Freq")
# RAM meds 
# Get RAM occuring in Retinoid users only 
RAM_meds<-merge(retinoid_study_population[,"person_id"], RAM_meds, by="person_id")
# Get teratogenic ATC subset
RAM_meds_teratogenic<-RAM_meds[Code %in%c("L04AA32","L04AX01","L04AX07","D05BA02","L04AX03","A11CA01","J01AA07","J01FA01", "J01FA10","L04AD01", "L04AB01","L04AB02","L04AC05","L04AC10","L04AC13","L04AC12","L04AC16","L04AC17"),]

if(nrow(RAM_prevalence_data_teratogenic)>0){
  ### Counts # 1
  ### Number of users of RAM which are contraindicated in combination with an oral retinoid per calendar month
  # Teratogenic RAM counts 
  RAM_prevalence_counts_teratogenic<-RAM_prevalence_data_teratogenic[,.N, by = .(year,month)]
  # Adjust for PHARMO
  if(is_PHARMO){RAM_prevalence_counts_teratogenic<-RAM_prevalence_counts_teratogenic[year < 2020,]} else {RAM_prevalence_counts_teratogenic<-RAM_prevalence_counts_teratogenic[year < 2021,]}
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  RAM_prevalence_counts_teratogenic<-as.data.table(merge(x = empty_df, y = RAM_prevalence_counts_teratogenic, by = c("year", "month"), all.x = TRUE))
  # Fills in missing values with 0
  RAM_prevalence_counts_teratogenic[is.na(N), N:=0]
  # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
  RAM_prevalence_counts_teratogenic[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
  # Masking is not applied before stratification
  RAM_prevalence_counts_teratogenic[,masked:=0]
  # Create YM variable 
  RAM_prevalence_counts_teratogenic<-within(RAM_prevalence_counts_teratogenic, YM<- sprintf("%d-%02d", year, month))
  # Denominator => Number of prevalent RAM users that month
  RAM_rates_teratogenic<-merge(x = RAM_prevalence_counts_teratogenic, y = RAM_prevalence_counts, by = c("YM"), all.x = TRUE)
  # Calculates rates
  RAM_rates_teratogenic<-RAM_rates_teratogenic[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
  # Keeps necessary columns 
  RAM_rates_teratogenic<-RAM_rates_teratogenic[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
  # Saves files in medicine counts folder
  saveRDS(RAM_rates_teratogenic, paste0(objective4_dir, "/", pop_prefix, "_RAM_counts_teratogenic.rds"))
  # Saves discontinued dfs
  saveRDS(RAM_prevalence_data_teratogenic, paste0(objective4_temp_dir, pop_prefix, "_RAM_prevalence_data_teratogenic.rds")) 
}else {
  print("There are no teratogenic RAM's in the data")
}

if(nrow(RAM_concomitance_data_teratogenic)>0){
  
  ### Counts # 2
  ### Number of users of RAM which are contraindicated in combination with an oral retinoid per calendar month
  # Teratogenic RAM counts concomitant recotds 
  RAM_concomitance_counts_teratogenic<-RAM_concomitance_data_teratogenic[,.N, by = .(year,month)]
  # Adjust for PHARMO
  if(is_PHARMO){RAM_concomitance_counts_teratogenic<-RAM_concomitance_counts_teratogenic[year < 2020,]} else {RAM_concomitance_counts_teratogenic<-RAM_concomitance_counts_teratogenic[year < 2021,]}
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  RAM_concomitance_counts_teratogenic<-as.data.table(merge(x = empty_df, y = RAM_concomitance_counts_teratogenic, by = c("year", "month"), all.x = TRUE))
  # Fills in missing values with 0
  RAM_concomitance_counts_teratogenic[is.na(N), N:=0]
  # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
  RAM_concomitance_counts_teratogenic[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
  # Masking is not applied before stratification
  RAM_concomitance_counts_teratogenic[,masked:=0]
  # Create YM variable 
  RAM_concomitance_counts_teratogenic<-within(RAM_concomitance_counts_teratogenic, YM<- sprintf("%d-%02d", year, month))
  
  # Denominator
  # Teratogenic RAM counts concomitant records 
  RAM_meds_counts_teratogenic<-RAM_meds_teratogenic[,.N, by = .(year(Date),month(Date))]
  # Adjust for PHARMO
  if(is_PHARMO){RAM_meds_counts_teratogenic<-RAM_meds_counts_teratogenic[year < 2020,]} else {RAM_meds_counts_teratogenic<-RAM_meds_counts_teratogenic[year < 2021,]}
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  RAM_meds_counts_teratogenic<-as.data.table(merge(x = empty_df, y = RAM_meds_counts_teratogenic, by = c("year", "month"), all.x = TRUE))
  # Fills in missing values with 0
  RAM_meds_counts_teratogenic[is.na(N), N:=0]
  # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
  RAM_meds_counts_teratogenic[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
  # Masking is not applied before stratification
  RAM_meds_counts_teratogenic[,masked:=0]
  # Create YM variable 
  RAM_meds_counts_teratogenic<-within(RAM_meds_counts_teratogenic, YM<- sprintf("%d-%02d", year, month))
  # Rename variable N to Freq
  setnames(RAM_meds_counts_teratogenic, "N", "Freq")
  # Merge the 2 
  # Denominator => Number of prevalent RAM users that month
  RAM_concomit_rates_teratogenic<-merge(x = RAM_concomitance_counts_teratogenic[,c("YM","N","masked", "true_value")], y = RAM_meds_counts_teratogenic[,c("YM", "Freq")], by = c("YM"), all.x = TRUE)
  # Calculates rates
  RAM_concomit_rates_teratogenic<-RAM_concomit_rates_teratogenic[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
  # Keeps necessary columns 
  RAM_concomit_rates_teratogenic<-RAM_concomit_rates_teratogenic[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
  # Saves files in medicine counts folder
  saveRDS(RAM_concomit_rates_teratogenic, paste0(objective4_dir, "/", pop_prefix, "_RAM_counts_teratogenic.rds"))
} else {
  print("There are no teratogenic RAM's in the data")
}
