# Load Retinoid Study Population (already loaded)
# Correct date format
retinoid_study_population[,entry_date:=as.IDate(entry_date,"%Y%m%d")][,exit_date:=as.IDate(exit_date,"%Y%m%d")][,birth_date:=as.IDate(birth_date,"%Y%m%d")]
# Creates column in study population: fu_dur_days 
retinoid_study_population[, fu_dur_days:=exit_date-entry_date]
# Creates age variable in study population = entry_date - birth date  (# Rounds down)
retinoid_study_population[,age_at_entry_date:=floor((entry_date-birth_date)/365.25)]
retinoid_study_population[,age_groups:=ifelse(retinoid_study_population[,age_at_entry_date>= 12&age_at_entry_date<21],"12-20.99", 
                                              ifelse(retinoid_study_population[,age_at_entry_date>=21&age_at_entry_date<31],"21-30.99",
                                                     ifelse(retinoid_study_population[,age_at_entry_date>=31&age_at_entry_date<41],"31-40.99",
                                                            ifelse(retinoid_study_population[,age_at_entry_date>=41&age_at_entry_date<56],"41-55.99","Not in range"))))]
# Load RAM medications 
RAM_meds<-as.data.table(do.call(rbind,lapply(paste0(medications_pop, list.files(medications_pop, pattern=paste0(pop_prefix, "_altmed"))), readRDS)))
# Get RAM meds in Retinoid users 
RAM_meds_in_retinoid_users<-merge(retinoid_study_population[,c("person_id","birth_date","entry_date","exit_date","fu_dur_days","age_at_entry_date","age_groups")], RAM_meds[,c("person_id","Code","Date")], by=c("person_id"))
# Remove any records that fall outside entry and exit days 
RAM_meds_in_retinoid_users<-RAM_meds_in_retinoid_users[Date>=entry_date&Date<=exit_date,]
# Create subsets for each of the indications 
# RAM - psoriasis 
RAM_psoriasis<-RAM_meds_in_retinoid_users[Code%in%c("D05AC01","D05AD02","D05BA02","D05BX51","D07AB01","D07AB02","D07AB03","D07AB04","D07AB05","D07AB06","D07AB07",
                                                    "D07AB08","D07AB09","D07AB10","D07AB11","D07AB19","D07AB21","D07AB30","D07AC01","D07AC02","D07AC03","D07AC04",
                                                    "D07AC05","D07AC06","D07AC07","D07AC08","D07AC09","D07AC10","D07AC11","D07AC12","D07AC13","D07AC14","D07AC15",
                                                    "D07AC16","D07AC17","D07AC18","D07AC19","D07AC21","D07AD01","D07AD02","D11AH01","D11AH02","L04AA32","L04AB01",
                                                    "L04AB02","L04AB04","L04AB05","L04AC05","L04AC10","L04AC12", "L04AC13","L04AC16","L04AC17","L04AD01","L04AX07"),]
# RAM - acne
RAM_acne<-RAM_meds_in_retinoid_users[Code%in%c("D07AA01","D07AB19","D10AA01","D10AA02","D10AA03","D10AE01","D10AF01","D10AF02","D10AF51","D10AF52",
                                               "H02AA01","H02AA02","H02AA03","H02AB01","H02AB02","H02AB03","H02AB04","H02AB05","H02AB08","H02AB09",
                                               "H02AB10","H02AB11","H02AB13","H02AB14","H02AB15","H02AB17","J01AA08","J01FA01","J01FA10","S01AA17","S01AA26"),]
# RAM - dermatitis 
RAM_dermatitis<-RAM_meds_in_retinoid_users[Code%in%c("D07AB01","D07AB02","D07AB03","D07AB04","D07AB05","D07AB06","D07AB07","D07AB08","D07AB09","D07AB10",
                                                     "D07AB11","D07AB19","D07AB21","D07AB30","D11AH01","D11AH02","H02AB06","H02AB07","L04AD01","L04AX01","L04AX03"),]         

# Remove duplicates for each group 
# retinoid study population # alreadu deduplicated 
# All RAM's in retinoid users 
RAM_meds_in_retinoid_users_unique<-unique(RAM_meds_in_retinoid_users,by="person_id")
# Psoriasis 
RAM_psoriasis_unique<-unique(RAM_psoriasis,by="person_id")       
# Acne
RAM_acne_unique<-unique(RAM_acne,by="person_id")    
# Dermatitis
RAM_dermatitis_unique<-unique(RAM_dermatitis,by="person_id")    
# Create a list of all the data you want baseline tables for 
Pops_for_baseline_tables<-list(retinoid_study_population, RAM_meds_in_retinoid_users_unique, RAM_psoriasis_unique, RAM_acne_unique, RAM_dermatitis_unique)
names(Pops_for_baseline_tables)<-c("Retinoid_Study_Population", "All_RAMs", "RAM_Psoriasis","RAM_Acne", "RAM_Dermatitis")
# For each of the Populations 
for (i in 1:length(Pops_for_baseline_tables)){
  df<-Pops_for_baseline_tables[[i]]
  if(nrow(df>0)){
    ################## BASELINE ALL POPULATION ########################
    # Calculates median of followup in years 
    fu_median     <- median(df$fu_dur_days)/365.25
    fu_IQR        <- IQR(df$fu_dur_days)/365.25
    fu_min        <- min(df$fu_dur_days)/365.25
    fu_max        <- max(df$fu_dur_days)/365.25
    max_exit_date <- max(df$exit_date)
    # fu_SD
    age_at_ID_mean <-mean(df$age_at_entry_date)
    age_at_ID_SD   <-sd(df$age_at_entry_date)
    
    # Calculations 
    age_at_ID_12_20.99_count <- sum(df$age_groups == "12-20.99") 
    age_at_ID_21_30.99_count <- sum(df$age_groups == "21-30.99")
    age_at_ID_31_40.99_count <- sum(df$age_groups == "31-40.99") 
    age_at_ID_41_55.99_count <- sum(df$age_groups == "41-55.99")
    # Calculates percentages
    age_at_ID_12_20.99_perc  <- (age_at_ID_12_20.99_count/nrow(df)) * 100
    age_at_ID_21_30.99_perc  <- (age_at_ID_21_30.99_count/nrow(df)) * 100
    age_at_ID_31_40.99_perc  <- (age_at_ID_31_40.99_count/nrow(df)) * 100
    age_at_ID_41_55.99_perc  <- (age_at_ID_41_55.99_count/nrow(df)) * 100
    
    # Create dataframe
    names<-c("Follow-up, years - median",
             "Follow-up, years - IQR",
             "Follow-up, years - min",
             "Follow-up, years - max",
             "Max exit date",
             "Age at index date (study entry) - mean",
             "Age at index date (study entry) - sd",
             "12.0-20.99 years_count",
             "12.0-20.99 years_perc",
             "21.0-30.99 years_count",
             "21.0-30.99 years_perc",
             "31.0-40.99 years_count",
             "31.0-40.99 years_perc",
             "41.0-55.99 years_count",
             "41.0-55.99 years_perc")
    
    values<-c(as.character(round(fu_median,1)),
              as.character(round(fu_IQR,1)),
              as.character(round(fu_min,2)),
              as.character(round(fu_max,2)),
              as.character(max_exit_date),
              as.character(round(age_at_ID_mean,1)),
              as.character(round(age_at_ID_SD,1)),
              as.character(age_at_ID_12_20.99_count),
              as.character(round(age_at_ID_12_20.99_perc,1)),
              as.character(age_at_ID_21_30.99_count),
              as.character(round(age_at_ID_21_30.99_perc,1)),
              as.character(age_at_ID_31_40.99_count),
              as.character(round(age_at_ID_31_40.99_perc,1)),
              as.character(age_at_ID_41_55.99_count),
              as.character(round(age_at_ID_41_55.99_perc),1))
    
    # Creates baseline table
    baseline<-data.table(names,values)
    # Saves files
    print(paste0("Saving baseline table: ", pop_prefix, "_", names(Pops_for_baseline_tables[i])))
    saveRDS(baseline, paste0(baseline_tables_dir,"/", pop_prefix, "_", names(Pops_for_baseline_tables[i]),"_baseline.rds"))
  }
}







