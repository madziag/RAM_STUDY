#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 10/12/2021

# 1. Looks for ATC codes from the ATC codelist (concept set created via CreateConceptSets_ATC.R) in MEDICINES TABLES 
# 2. Results in list of records with matching ATC
# 3. Counts records saved in the previous step by month/year for each code group 
################################################################################################################
################################################################################################################
################################################################################################################

# Concept Sets
matches<-c() #matches all ATC codes -> Retinoid + RAM codes 
source(paste0(pre_dir,"conceptsets/create_concept_sets_ATC.R")) #load concept sets

# Empty df grid for counts
FUmonths_df<-as.data.table(FUmonths_df) #convert to data.table
min_data_available<-min(FUmonths_df$Y) #get min year from denominator file
max_data_available<-max(FUmonths_df$Y) #get max year from denominator file
FUmonths_df[, c("Y", "M") := tstrsplit(YM, "-", fixed=TRUE)] #create Y, M variables from YM 
empty_df<-expand.grid(seq(min(FUmonths_df$Y), max(FUmonths_df$Y)), seq(1, 12)) #create grid with all possible month-year combinations from min year to max year
names(empty_df)<-c("year", "month") #rename the columns 

# Lists all medicines files 
med_files<-list.files(path=path_dir, pattern = "MEDICINES", ignore.case = TRUE) 

# Checks that there is at least 1 medicine file 
if(length(med_files)>0){
  # do this for each medicine files 
  for (y in 1:length(med_files)){
    #get prefix of table - to be used in file naming 
    meds_prefix<-gsub(".csv", "", med_files[y]) 
    # Load current table 
    df<-fread(paste(path_dir, med_files[y], sep=""), stringsAsFactors = FALSE) #load table
    # Data Cleaning
    df<-df[,c("person_id", "medicinal_product_atc_code", "date_dispensing", "date_prescription", "meaning_of_drug_record", "presc_duration_days", "disp_number_medicinal_product", "presc_quantity_per_day", "medicinal_product_id")] #keep only necessary columns
    df<-df[,lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #replace any missing values with NA
    setnames(df,"meaning_of_drug_record","Meaning") #rename column 
    setnames(df,"medicinal_product_atc_code","Code") #rename column
    df[,Code:=gsub("\u00A0","",Code, fixed = TRUE)] #remove non breaking spaces 
    # Creates new column Date. 
    ## if date_dispensing is not missing, then Date = date_dispensing
    ## if date_dispensing is missing, then Date = date_prescription
    df<-df[,Date:= ifelse(!is.na(date_dispensing), date_dispensing, date_prescription)]
    
    colnames_events<-names(df)
    std_names_events<-names(study_population)
    colnames_events<-colnames_events[!colnames_events %in% "person_id"]
    
    # Merge medicine table with study population table (there is no missing data in this table)
    df[,person_id:=as.character(person_id)] #person_id converted to data type character 
    study_population[,person_id:=as.character(person_id)]#person_id converted to data type character 
    df<-df[study_population,on=.(person_id)] # Left join, keeps all people in the study population even if they didn't have an event
    df<-df[!rowSums(is.na(df[,..colnames_events]))==length(colnames_events)] #removes records with missing values in the medicine table 
    # Adjust column data types 
    df[presc_duration_days == ".", presc_duration_days := NA]
    df[,presc_duration_days:=as.numeric(presc_duration_days)]
    # df[,age_start_follow_up:=as.numeric(age_start_follow_up)]
    # df[,disp_number_medicinal_product:=as.integer(disp_number_medicinal_product)][,medicinal_product_id:=as.integer(medicinal_product_id)]
    df[,Code:=as.character(Code)]
    df[,Date:=as.IDate(Date,"%Y%m%d")][,entry_date:=as.IDate(entry_date,"%Y%m%d")] 
    
    # Create Year variable from Date
    df[,year:=year(Date)]
    
    # Filter out records that have year or ATC missing and are not within years used in study 
    df<-df[!is.na(year),]
    df<-df[!is.na(Code),]
    if(is_PHARMO){df<-df[year>2008 & year<2020]} else {df<-df[year>2008 & year<2021]} 
    # Filter out records with unspecified sex
    df<-df[sex_at_instance_creation=="M"|sex_at_instance_creation=="F"]
    # Looking for ATC matches in Current Medicines Table 
    #checks first if df has at least one row of records
    if(nrow(df)>0){
      # checks if df has each of the ATC codes in the previously created codelist
      for (i in 1:length(codelist_all)){
        df_subset<-setDT(df)[Code %chin% codelist_all[[i]][,Code]]#creates subset with current ATC code group being checked i.e. retinoids, and each RAM group 
        df_subset<-df_subset[!duplicated(df_subset),]# removes any duplicates 
        if(nrow(df_subset)<=0){print(paste("There are NO records for", names(codelist_all[i])))}
        saveRDS(data.table(df_subset), paste0(events_tmp_ATC, pop_prefix,"_", names(codelist_all[i]), "_",meds_prefix, ".rds")) #save the subset into events folder 
      }
    } 
  }
  # Counting the records 
  print("Counting records")
  # Create empty lists for use below
  comb_meds<-list()
  comb_meds1<-list()
  # For each Retinoid-RAM group
  for(j in 1:length(codelist_all)){
    files<-list.files(path=events_tmp_ATC, pattern=names(codelist_all[j]))#list of files in current code group
    #checks that there is at least on file in the list
    if (length(files)>0){
      # Load and bind all data in the same code group
      comb_meds[[j]]<-do.call(rbind,lapply(paste0(events_tmp_ATC, files), readRDS))
      comb_meds[[j]]<-comb_meds[[j]][!duplicated(comb_meds[[j]]),]
      comb_meds1[[j]]<-comb_meds[[j]][Date>=entry_date & Date<=exit_date]
      # Counts
      counts<-comb_meds1[[j]][,.N, by = .(year,month(Date))]# count by year-month
      counts<-as.data.table(merge(x=empty_df,y=counts,by=c("year","month"),all.x = TRUE))#merge with empty df
      counts[is.na(counts[,N]), N:=0]#fill in missing values with 0
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Create column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
      counts[,masked:=ifelse(N>0&N<5,1,0)]
      # Change values less than 5 and more than 0 to 5 (only if masked is 1)
      if(mask==T){counts[masked==1,N:=5]} else {counts[masked==1,N:=N]}
      # Calculate rates
      counts<-within(counts, YM<- sprintf("%d-%02d", year, month))
      counts<-merge(x = counts, y = FUmonths_df, by = c("YM"), all.x = TRUE)
      counts<-counts[,rates:=as.numeric(N)/as.numeric(Freq)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
      counts<-counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
      # Save files 
      if(comb_meds[[j]][,.N]>0){
        saveRDS(comb_meds[[j]], paste0(medications_pop, pop_prefix, "_", names(codelist_all[j]),"_MEDS.rds"))
        saveRDS(counts, paste0(monthly_counts_atc,"/", pop_prefix, "_", names(codelist_all[j]),"_MEDS_counts.rds"))
      } 
    } else {
      print(paste("There are no matching records for", names(codelist_all[j])))
    }
  }
} else {
  print("There are no MEDICINES tables to analyse!")
}

# Delete all files in events_tmp_ATC (so as not to have them merge with the second subpopulation )
for(file in list.files(events_tmp_ATC, pattern = "\\.rds$", full.names = TRUE)){unlink(file)}
# Cleanup
rm(list = grep("^codelist|^comb|^count|^df|^sub", ls(), value = TRUE))
