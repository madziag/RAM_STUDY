#Author: Magdalena Gamba M.D.,Ema Alsina MSc.
#email: m.a.gamba@uu.nl,e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 10/01/2022

# Creates entry and exit dates for each person based on their eligibility and data availability
# Adds spell_start and spell_end

OBS_SPELLS<-as.data.table(readRDS(paste0(g_intermediate,"tmp/STUDY_SOURCE_POPULATION/", pop_prefix, "_OBS_SPELLS.rds")))
# rename columns
setnames(OBS_SPELLS,"op_start_date", "spell_start_date")
setnames(OBS_SPELLS,"op_end_date","spell_end_date")

# Merges OBS_SPELLS with study population
study_population[,person_id:=as.character(person_id)]
OBS_SPELLS[,person_id:=as.character(person_id)]
study_population<-OBS_SPELLS[study_population,on=.(person_id)] # Left join
# Creates Entry Date 
## entry date<-latest date at which ANY of the following conditions are met: age > 12, observation starts, study starts
# Looks for max date of all chosen columns
study_population[,start_date:=as.IDate(as.character(20100101), "%Y%m%d")]
study_population[,entry_date:=pmax(date_min, spell_start_date, start_date, na.rm = TRUE)]

summary(study_population[,entry_date])
print("entry date OK")
# Creates Exit date
## exit date<-earliest date at which ANY of the following conditions are met: age>55, observation ends, study ends, sterilization record date (if present)
# Checks if there are sterilization records. 
## If there are, loads the file and merges with study population, and uses this data to create exit date
## If there are no sterilization records, uses age>55, observation date end and study end date to determine exit date
if (length(list.files(paste0(g_intermediate,"tmp/sterility/"), pattern = "sterility_all_first_occurrence.rds"))>0){
  # Loads sterilization records
  # if(SUBP == TRUE){sterility<-readRDS(paste0(g_intermediate,"tmp/sterility/", populations[pop], "_sterility_all_first_occurrence.rds"))}else {sterility<-readRDS(paste0(g_intermediate,"tmp/sterility/sterility_all_first_occurrence.rds"))}
  sterility<-readRDS(paste0(sterility_pop, pop_prefix, "_sterility_all_first_occurrence.rds"))
  # Data Cleaning 
  sterility<-sterility[,c("person_id", "Date")][,person_id:=as.character(person_id)]
  setnames(sterility,"Date","Sterility_Date") # Renames column names
  # Merges sterilization file with study population
  study_population<-sterility[study_population,on=.(person_id)] # Left join
  # Looks for min date of all chosen columns
  study_population[,end_date:=ifelse(is_PHARMO, as.IDate(as.character(20191231),"%Y%m%d"),as.IDate(as.character(20201231),"%Y%m%d"))]
  study_population[,exit_date:=pmin(date_max,spell_end_date,end_date,Sterility_Date,na.rm=TRUE)]
  study_population[,exit_date_reason:=ifelse(pmin(date_max,spell_end_date,end_date,Sterility_Date,na.rm=TRUE)==date_max,"date_max",
                                             ifelse(pmin(date_max,spell_end_date,end_date,Sterility_Date,na.rm=TRUE)==spell_end_date,"spell_end",
                                                    ifelse(pmin(date_max,spell_end_date,end_date,Sterility_Date,na.rm=TRUE)==end_date,"end_date","sterility")))]
                   
  summary(study_population[,exit_date])
  print("exit date OK")
  
  sterility_before_entry_date<-nrow(unique(study_population[exit_date_reason=="sterility" & Sterility_Date<entry_date,]))

  } else {
  # Determines exit date (excluding sterility records )
  study_population[,end_date:=ifelse(is_PHARMO, as.IDate(as.character(20191231),"%Y%m%d"),as.IDate(as.character(20201231),"%Y%m%d"))]
  study_population[,exit_date:=pmin(date_max,spell_end_date,end_date,na.rm=TRUE)]
  
  summary(study_population$exit_date)
  print("exit date OK")
}

# Exclude any individuals who have exit_date before Jan 01 2010, as they will never be eligible
print(paste0(dim(study_population[exit_date<=entry_date,])[1], " individuals excluded as their exit date was before their entry date, i.e. before 01 Jan 2010"))
study_population<-study_population[exit_date>entry_date,]
print("save study_population.rds with entry, exit and spells start and end")
saveRDS(study_population, paste0(populations_dir, populations[pop]))

rm(OBS_SPELLS)


