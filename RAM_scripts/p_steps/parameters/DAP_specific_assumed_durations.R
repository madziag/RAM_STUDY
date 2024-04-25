#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 31/01/2022


##############################################################
############### DAP SPECIFIC ASSUMED DURATIONS ###############
##############################################################
### DEFAULT ###
# cma_data$assumed_duration<-rep(30, nrow(cma_data))
cma_data[,assumed_duration:=30]
### DAP	Type algorithm	Rules ###
# 1. BIFAP: Estimated using local algorithm	Valp/Retin: Use the value in MEDICINES$presc_duration_days --> COLUMN ADDED IN MONTHLY COUNTS ATC
### ARE THERE ANY MISSING VALUES??????
if(is_BIFAP){
  cma_data[,assumed_duration:=presc_duration_days][,assumed_duration:=as.numeric(assumed_duration)]
  # If any NA's, we take the value of 30 days 
  cma_data[is.na(assumed_duration), assumed_duration:=30][assumed_duration<=0, assumed_duration:=30]
  if(sum(is.na(cma_data$presc_duration_days))>0){
    print("Missing values for duration of treatment detected.")
    print(paste0(sum(is.na(cma_data$presc_duration_days)), " missing values were imputed with a value of 30 days"))
  }
}

# 2. PHARMO Estimated using local algorithm	Valp/Retin: Use the value in MEDICINES$presc_duration_days
if(is_PHARMO){
  cma_data[,assumed_duration:=presc_duration_days][,assumed_duration:=as.numeric(assumed_duration)]
  # If any NA's, we take the value of 30 days 
  cma_data[is.na(assumed_duration), assumed_duration:=30][assumed_duration<=0, assumed_duration:=30]
  if(sum(is.na(cma_data$presc_duration_days))>0){
    print("Missing values for duration of treatment detected.")
    print(paste0(sum(is.na(cma_data$presc_duration_days)), " missing values were imputed with a value of 30 days"))
  }
}


