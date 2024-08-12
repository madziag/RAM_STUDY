# Load study population 
study_population <- as.data.table(readRDS(paste0(populations_dir,pop_prefix,"_study_population.rds")))

# Correct date format
study_population[,entry_date:=as.IDate(entry_date,"%Y%m%d")][,exit_date:=as.IDate(exit_date,"%Y%m%d")][,birth_date:=as.IDate(birth_date,"%Y%m%d")]
# Creates column in study population: fu_dur_days 
study_population[, fu_dur_days:=exit_date-entry_date]
# Creates age variable in study population = entry_date - birth date  (# Rounds down)
study_population[,age_at_entry_date:=floor((entry_date-birth_date)/365.25)]
# Create Age Groups
study_population[,age_groups:=ifelse(study_population[,age_at_entry_date>= 12&age_at_entry_date<21],"12-20.99", 
                                              ifelse(study_population[,age_at_entry_date>=21&age_at_entry_date<31],"21-30.99",
                                                     ifelse(study_population[,age_at_entry_date>=31&age_at_entry_date<41],"31-40.99",
                                                            ifelse(study_population[,age_at_entry_date>=41&age_at_entry_date<56],"41-55.99","Not in range"))))]

# Load RAM medications 
RAM_meds<-as.data.table(do.call(rbind,lapply(paste0(medications_pop, list.files(medications_pop, pattern=paste0(pop_prefix, "_altmed"))), readRDS)))

# Keep records where RAM date is after retinoid date and RAM date falls within entry and exit dates 
RAM_meds<-RAM_meds[Date>=entry_date-90 & Date<=exit_date,]

# Get fu duration and age groups from study population
RAM_meds_in_WOCBP<-study_population[,c("person_id","birth_date","entry_date","exit_date","fu_dur_days","age_at_entry_date","age_groups")][RAM_meds,on=.(person_id)]

# Remove duplicates for each group 
# study population # 
study_population_unique <- unique(study_population, by="person_id")
# All RAM's in WOCBP
RAM_meds_in_WOCBP_unique<-unique(RAM_meds_in_WOCBP,by="person_id")

# Create a list of all the data you want baseline tables for 
Pops_for_baseline_tables<-list(study_population_unique, RAM_meds_in_WOCBP_unique)
names(Pops_for_baseline_tables)<-c("WOCBP_study_population", "RAM_WOCBP")
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



# Clean up 
rm(list= grep("Pops|RAM_a|RAM_d|RAM_p|RAM_m|retinoid_incidence_data", ls(), value = TRUE))



