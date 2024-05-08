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

# Get RAM meds in Retinoid users 
RAMs_in_studypop<-merge(retinoid_study_population[,c("person_id")], RAM_meds[,c("person_id", "Code", "Date", "entry_date", "exit_date")], by="person_id")
# Filters out any rx dates that falls outside of entry and exit dates 
RAMs_in_studypop<-RAMs_in_studypop[Date>=entry_date & Date<=exit_date,]
# Rename columns 
setnames(RAMs_in_studypop,"Code","ATC")
setnames(RAMs_in_studypop,"Date","rx_date")
# Creates column with year month for prescription 
setDT(RAMs_in_studypop)[, rx_year_month := format(as.Date(rx_date), "%Y-%m")]
# Create column for pre and post periods per DAP
if(is_PHARMO){RAMs_in_studypop[,period:=ifelse(rx_year_month<"2018-08","pre","post")]}
if(is_BIFAP){RAMs_in_studypop[,period:=ifelse(rx_year_month<"2018-07","pre","post")]}

#############################################################################################
################################## Counts ###################################################
#############################################################################################
##### ALL RECORDS #####
# Count per year month ATC code 
RAM_record_counts<-RAMs_in_studypop[,.N, by = .(ATC,period)]
# Change table format form long to wide
RAM_record_counts<-dcast(RAM_record_counts, ATC~period, value.var = "N")
if("pre"%!in%colnames(RAM_record_counts)){RAM_record_counts[,pre:=0]}
if("post"%!in%colnames(RAM_record_counts)){RAM_record_counts[,post:=0]}
  
RAM_record_counts[is.na(post),post:=0][is.na(pre),pre:=0]
# Rearrange columns
setcolorder(RAM_record_counts,c("ATC","pre","post"))

##### PER USER #####
# Get unique user values i.e. user is counted once per ATC and period
RAMs_in_studypop_unique<-unique(RAMs_in_studypop,by=c("person_id","ATC","period"))
# Count per year month ATC code 
RAM_user_counts<-RAMs_in_studypop_unique[,.N, by = .(ATC,period)]
# Change table format form long to wide
RAM_user_counts<-dcast(RAM_user_counts, ATC~period, value.var = "N")
if("pre"%!in%colnames(RAM_user_counts)){RAM_user_counts[,pre:=0]}
if("post"%!in%colnames(RAM_user_counts)){RAM_user_counts[,post:=0]}
# If value is missing, change to 0
RAM_user_counts[is.na(post),post:=0][is.na(pre),pre:=0]
# Rearrange columns
setcolorder(RAM_user_counts,c("ATC","pre","post"))


# Save the files 

saveRDS(RAM_record_counts, paste0(medicines_counts_dir,"/", pop_prefix, "_RAM_record_counts_overall.rds")) 
saveRDS(RAM_user_counts, paste0(medicines_counts_dir,"/", pop_prefix, "_RAM_user_counts_overall.rds")) 
