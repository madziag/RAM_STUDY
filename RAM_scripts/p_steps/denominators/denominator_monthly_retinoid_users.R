
#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 10/12/2021

# Same code as in denominator_monthly_WOCBP only here it is run on the subset of that population (retinoid users)

# Create a subset of study population who has ever used Retinoids 
# Read in Retinoid records that were created from monthly counts ATC
retinoid_meds<-as.data.table(readRDS(paste0(medications_pop, pop_prefix, "_Retinoid_MEDS.rds")))
# all retinoid users 
#flowchart
all_retinoid_users<-length(unique(retinoid_meds$person_id))

# Retinoid use should be between study entry and exit to be counted 
retinoid_meds<-retinoid_meds[Date>=entry_date & Date<=exit_date,]
# after excluding meds that fall outside of entry and exit dates 
#flowchart
retinoid_within_entry_exit<-length(unique(retinoid_meds[,person_id]))
# remove duplicates
retinoid_users<- as.data.table(unique(retinoid_meds, by = c("person_id")))[,c("person_id")]

# Get study population subset i.e only those who have retinoid prescriptions 
retinoid_study_population<-as.data.table(merge(study_population, retinoid_users, by = "person_id")) 

# Saves file
saveRDS(data.table(retinoid_study_population), paste0(populations_dir, pop_prefix, "_retinoid_study_population.rds"))

# this script counts the number of eligible participants per month (as persons may leave and enter the database)

if(nrow(retinoid_study_population)>0){
  # Sets start and end dates 
  start.date<-as.Date(retinoid_study_population$entry_date)
  end.date<-as.Date(retinoid_study_population$exit_date)
  # Converts to yearmon
  ym1<-as.yearmon(as.character(start.date), "%Y-%m-%d") 
  ym2<-as.yearmon(as.character(end.date), "%Y-%m-%d") 
  # Gets seq of months between start dates and end dates in a list 
  FUmonths<-list()
  for (i in 1:length(ym1)){
    s<-seq(ym1[i], ym2[i], (1/12)) # creates yearmon sequence
    s <-as.numeric(format(s, "%Y%m"))
    FUmonths[[i]]<-s
  }
  FUmonths<-unlist(FUmonths)
  if(is_PHARMO) {studyFUmonths<-FUmonths[(FUmonths>=200901)&(FUmonths<=201912)]} else {studyFUmonths<-FUmonths[(FUmonths>=200901)&(FUmonths<=202012)]}
  FUmonths_df_retinoid<-as.data.frame(table(studyFUmonths))
  FUmonths_df_retinoid$YM<-as.Date(paste0(as.character(FUmonths_df_retinoid$studyFUmonths),"01"), format="%Y%m%d")
  FUmonths_df_retinoid$YM<-format(as.Date(FUmonths_df_retinoid$YM),"%Y-%m")
  
  if(is_BIFAP){
    if(nrow(FUmonths_df_retinoid)<132){
      empty_df<-expand.grid(seq(2010, 2020), seq(1, 12))
      names(empty_df)<-c("year", "month")
      empty_df<-within(empty_df, YM<- sprintf("%d-%02d", year, month))
      FUmonths_df_retinoid<-as.data.table(merge(x = empty_df, y = FUmonths_df_retinoid, by = c("YM"), all.x = TRUE))
      FUmonths_df_retinoid[is.na(Freq), Freq:=0][is.na(studyFUmonths),studyFUmonths:=gsub("-", "",YM)]
      FUmonths_df_retinoid<-FUmonths_df_retinoid[,c("studyFUmonths","YM", "Freq")]
    } else {
      FUmonths_df_retinoid<-FUmonths_df_retinoid
    }
  } else {
    FUmonths_df_retinoid<-FUmonths_df_retinoid
  }
  # Saves file
  saveRDS(data.table(FUmonths_df_retinoid), paste0(output_dir, pop_prefix, "_denominator_retinoid.rds"))
  # plots denominator 
  pdf((paste0(output_dir, "plots/", pop_prefix ,"_retinoid_denominator.pdf")), width=8, height=4)
  plot(FUmonths_df_retinoid$studyFUmonths, FUmonths_df_retinoid$Freq, ylab="Persons Observed per Month", xlab="Year and Month")
  invisible(dev.off())
}
