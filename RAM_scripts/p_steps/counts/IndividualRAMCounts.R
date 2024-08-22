# Counts the number of users/records per period (pre/post implementation)
# If records -> counts all records per ATC per period
# If users -> A user is counted once per period per ATC 

#############################################################################################
################################## Data Preparation #########################################
#############################################################################################
# Data Loading
# Retinoid study population # Already loaded 
# RAM meds
RAM_meds<-as.data.table(do.call(rbind,lapply(paste0(medications_pop, list.files(medications_pop, pattern=paste0(pop_prefix, "_altmed"))), readRDS)))
# Keep RAM meds that occur in Retinoid users only, and only if RAM prescriptions occur after Retinoid incidence use
# Read in retinoid incidence data
retinoid_prevalence_data<-as.data.table(readRDS(paste0(retinoid_counts_dfs, pop_prefix,"_Retinoid_prevalence_data.rds")))
# Rename episode start column
setnames(retinoid_prevalence_data,"episode.start","episode.start.retinoid") 
# Get first retinoid use in entry into study (could be incidence or prevalent use)
retinoid_prevalence_data<-unique(retinoid_prevalence_data, by=c("person_id"))

# Merge with retinoid incidence data 
RAM_meds<-retinoid_prevalence_data[,c("person_id","ATC.retinoid","episode.start.retinoid")][RAM_meds,on=.(person_id)]
# Keep records where RAM date is after retinoid date and RAM date falls within entry and exit dates 
RAM_meds<-RAM_meds[Date>=episode.start.retinoid & Date>=entry_date-90 & Date<=exit_date,c("person_id","ATC.retinoid","Code","Date","episode.start.retinoid")]
# Get RAM meds in Retinoid users 
RAMs_in_studypop<-retinoid_study_population[,c("person_id","birth_date","entry_date","exit_date","fu_dur_days","age_at_entry_date","age_groups")][RAM_meds,on=.(person_id)]
# Rename columns 
setnames(RAMs_in_studypop,old=c("Code","Date"),new=c("ATC.RAM","Date.RAM"))
# Creates column with year month for prescription 
setDT(RAMs_in_studypop)[, rx_year_month:=format(as.Date(Date.RAM), "%Y-%m")]
# Create column for pre and post periods per DAP
if(is_PHARMO){RAMs_in_studypop[,period:=ifelse(rx_year_month<"2018-08","pre","post")]}
if(is_BIFAP){RAMs_in_studypop[,period:=ifelse(rx_year_month<"2018-07","pre","post")]}

#############################################################################################
################################## Counts ###################################################
#############################################################################################
##### ALL RECORDS #####
# Count per year month ATC code 
RAM_record_counts<-RAMs_in_studypop[,.N, by = .(ATC.RAM,period)]
# Change table format form long to wide
RAM_record_counts<-dcast(RAM_record_counts, ATC.RAM~period, value.var = "N")
if("pre"%!in%colnames(RAM_record_counts)) {RAM_record_counts[,pre :=0]}
if("post"%!in%colnames(RAM_record_counts)){RAM_record_counts[,post:=0]}
  
RAM_record_counts[is.na(post),post:=0][is.na(pre),pre:=0]
# Rearrange columns
setcolorder(RAM_record_counts,c("ATC.RAM","pre","post"))

##### PER USER #####
# Get unique user values i.e. user is counted once per ATC and period
RAMs_in_studypop_unique<-unique(RAMs_in_studypop,by=c("person_id","ATC.RAM","period"))
# Count per year month ATC code 
RAM_user_counts<-RAMs_in_studypop_unique[,.N, by = .(ATC.RAM,period)]
# Change table format form long to wide
RAM_user_counts<-dcast(RAM_user_counts, ATC.RAM~period, value.var = "N")
if("pre"%!in%colnames(RAM_user_counts)) {RAM_user_counts[,pre :=0]}
if("post"%!in%colnames(RAM_user_counts)){RAM_user_counts[,post:=0]}
# If value is missing, change to 0
RAM_user_counts[is.na(post),post:=0][is.na(pre),pre:=0]
# Rearrange columns
setcolorder(RAM_user_counts,c("ATC.RAM","pre","post"))


# Save the files 

saveRDS(RAM_record_counts, paste0(medicines_counts_dir,"/", pop_prefix, "_RAM_record_counts_overall.rds")) 
saveRDS(RAM_user_counts, paste0(medicines_counts_dir,"/", pop_prefix, "_RAM_user_counts_overall.rds")) 

##########################################  
##### COUNTS BY INDICATION ###############  
##########################################   
# RAMS in retinoid
# RAMs_in_studypop
# Create Indication subsets based on RAM ATC
# RAMS in studypop => all RAM records

RAMs_in_studypop<-unique(RAMs_in_studypop) # this should be records 
# Create subsets for each of the indications - based on the Retinoid not RAM
RAM_data_psoriasis<-RAMs_in_studypop[ATC.retinoid=="D05BB02",][,indication:="psoriasis"]
RAM_data_acne<-RAMs_in_studypop[ATC.retinoid=="D10BA01",][,indication:="acne"]
RAM_data_dermatitis<-RAMs_in_studypop[ATC.retinoid=="D11AH04",][,indication:="dermatitis"]

# Bind the three indications 
RAM_data_per_indication_all<-rbindlist(list(RAM_data_psoriasis,RAM_data_acne,RAM_data_dermatitis))
RAM_data_per_indication_all<-RAM_data_per_indication_all[,-c("ATC.retinoid","episode.start.retinoid")]
RAM_data_per_indication_all<-unique(RAM_data_per_indication_all)
RAM_data_per_indication_all[,year:= year(Date.RAM)][,month:=month(Date.RAM)]

# Count incidence by indication, month, year, period
##### per indication #####
# Count per year month ATC code 
RAM_record_counts_per_indication<-RAM_data_per_indication_all[,.N, by = .(ATC.RAM,indication,period)]
# Change table format form long to wide
RAM_record_counts_per_indication1<-dcast(RAM_record_counts_per_indication, ATC.RAM+indication~period, value.var = "N")

if("pre"%!in%colnames (RAM_record_counts_per_indication1)){RAM_record_counts_per_indication1[,pre :=0]}
if("post"%!in%colnames(RAM_record_counts_per_indication1)){RAM_record_counts_per_indication1[,post:=0]}

RAM_record_counts_per_indication1[is.na(post),post:=0][is.na(pre),pre:=0]
# Rearrange columns
setcolorder(RAM_record_counts_per_indication1,c("ATC.RAM","indication","pre","post"))

RAM_record_counts_per_indication_psoriasis<-RAM_record_counts_per_indication1[indication=="psoriasis",]
RAM_record_counts_per_indication_acne<-RAM_record_counts_per_indication1[indication=="acne",]
RAM_record_counts_per_indication_dermatitis<-RAM_record_counts_per_indication1[indication=="dermatitis",]

saveRDS(RAM_record_counts_per_indication_psoriasis, paste0(medicines_counts_dir,"/", pop_prefix, "_RAM_record_counts_psoriasis.rds")) 
saveRDS(RAM_record_counts_per_indication_acne, paste0(medicines_counts_dir,"/", pop_prefix, "_RAM_record_counts_acne.rds")) 
saveRDS(RAM_record_counts_per_indication_dermatitis, paste0(medicines_counts_dir,"/", pop_prefix, "_RAM_record_counts_dermatitis.rds")) 



# Count incidence by indication, month, year, period - PER USER
##### per indication #####
RAM_data_per_indication_all_unique<-unique(RAM_data_per_indication_all,by=c("person_id", "indication"))
# Count per year month ATC code 
RAM_record_counts_per_indication_unique<-RAM_data_per_indication_all_unique[,.N, by = .(ATC.RAM,indication,period)]
# Change table format form long to wide
RAM_record_counts_per_indication1_unique<-dcast(RAM_record_counts_per_indication_unique, ATC.RAM+indication~period, value.var = "N")

if("pre"%!in%colnames(RAM_record_counts_per_indication1_unique)) {RAM_record_counts_per_indication1_unique[,pre :=0]}
if("post"%!in%colnames(RAM_record_counts_per_indication1_unique)){RAM_record_counts_per_indication1_unique[,post:=0]}

RAM_record_counts_per_indication1_unique[is.na(post),post:=0][is.na(pre),pre:=0]
# Rearrange columns
setcolorder(RAM_record_counts_per_indication1_unique,c("ATC.RAM","indication","pre","post"))

RAM_user_counts_per_indication_psoriasis<-RAM_record_counts_per_indication1_unique[indication=="psoriasis",]
RAM_user_counts_per_indication_acne<-RAM_record_counts_per_indication1_unique[indication=="acne",]
RAM_user_counts_per_indication_dermatitis<-RAM_record_counts_per_indication1_unique[indication=="dermatitis",]

saveRDS(RAM_user_counts_per_indication_psoriasis, paste0(medicines_counts_dir,"/", pop_prefix, "_RAM_user_counts_psoriasis.rds")) 
saveRDS(RAM_user_counts_per_indication_acne, paste0(medicines_counts_dir,"/", pop_prefix, "_RAM_user_counts_acne.rds")) 
saveRDS(RAM_user_counts_per_indication_dermatitis, paste0(medicines_counts_dir,"/", pop_prefix, "_RAM_user_counts_dermatitis.rds")) 
