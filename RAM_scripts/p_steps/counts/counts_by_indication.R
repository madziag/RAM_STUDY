##################################################################
#################### PREVALENCE BY INDICATION #################### 
##################################################################
# Data Loading & Cleaning
## RAM Prevalent Data ##
if(file.exists(paste0(objective1_temp_dir, pop_prefix,"_RAM_prevalence_data.rds"))){
  
  RAM_prevalence_data<-as.data.table(readRDS(paste0(objective1_temp_dir, pop_prefix,"_RAM_prevalence_data.rds")))
  
  if(nrow(RAM_prevalence_data)>0){
    # Drop unneeded columns 
    RAM_prevalence_data<-RAM_prevalence_data[,-c("episode.ID","end.episode.gap.days","episode.duration","birth_date","entry_date","exit_date","current_age","age_group")]
    # Change date format
    RAM_prevalence_data[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)]
    # Rename columns
    setnames(RAM_prevalence_data, old = c("episode.start","episode.end"), new = c("episode.start.RAM","episode.end.RAM"))
    ## RAM prevalence counts
    RAM_prevalence_rates<-as.data.table(readRDS(paste0(objective1_dir, "/", pop_prefix, "_RAM_prevalence_counts.rds")))
    # Read in retinoid prevalence data
    retinoid_prevalence_data<-as.data.table(readRDS(paste0(retinoid_counts_dfs, pop_prefix,"_Retinoid_prevalence_data.rds")))
    # Rename episode start column
    setnames(retinoid_prevalence_data, old=c("episode.start", "episode.day"), new=c("episode.start.retinoid", "episode.day.retinoid")) 
    # Get first retinoid use in entry into study (could be incidence or prevalent use)
    retinoid_prevalence_data_first<-retinoid_prevalence_data[order(episode.start.retinoid), .SD[1], by = person_id]
    # Merge these prevalent retinoid treatment episodes with RAM episodes so that we have both Retinoid and RAM dates per row
    RAM_prevalence_data<-retinoid_prevalence_data_first[,c("person_id","ATC.retinoid","episode.day.retinoid")][RAM_prevalence_data,on=.(person_id)]
    
    # Create column for indication 
    RAM_prevalence_data[ATC.retinoid=="D05BB02",indication:="psoriasis"]
    RAM_prevalence_data[ATC.retinoid=="D10BA01",indication:="acne"]
    RAM_prevalence_data[ATC.retinoid=="D11AH04",indication:="dermatitis"]
    
    # Flowchart
    RAM_flowchart_prevalence<-nrow(RAM_prevalence_data)
    RAM_flowchart_prevalence_psoriasis<-nrow(RAM_prevalence_data[indication=="psoriasis",])
    RAM_flowchart_prevalence_acne<- nrow(RAM_prevalence_data[indication=="acne",])
    RAM_flowchart_prevalence_dermatitis<-nrow(RAM_prevalence_data[indication=="dermatitis",])
    
    # Count prevalence by indication, month, year
    prevalence_by_indication<-RAM_prevalence_data[,.N, by = .(year,month, indication)]
    
    for(group in 1:length(unique(prevalence_by_indication$indication))){
      # Create a subset of age group
      each_group<-prevalence_by_indication[indication==unique(prevalence_by_indication$indication)[group]]
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
      # Fills in missing values with 0
      each_group[is.na(N),N:=0][is.na(indication),indication:=unique(prevalence_by_indication$indication)[group]]
      # Adjust for PHARMO
      if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
      # Create YM variable
      each_group<-within(each_group,YM<-sprintf("%d-%02d",year,month))
      
      # Prepare denominator
      prevalence_count_min <- RAM_prevalence_rates[,c("YM","N")]
      setnames(prevalence_count_min,"N","Freq")
      
      # Merge age-group subset count with all prevalent counts
      indication_prevalence_count<-merge(x=each_group,y=prevalence_count_min,by=c("YM"),all.x=TRUE)
      # Masking set at 0
      indication_prevalence_count[,masked:=0]
      # Calculates rates
      indication_prevalence_count<-indication_prevalence_count[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates),rates:=0]
      # Drop columns you don't need
      indication_prevalence_count<-indication_prevalence_count[,c("YM","N","Freq","rates","masked")]
      
      # Save files in medicine counts folder
      saveRDS(indication_prevalence_count, (paste0(objective1_strat_dir,"/", pop_prefix,"_RAM_prevalence_counts_", unique(prevalence_by_indication$indication)[group],"_indication_group.rds")))
    }
  } 
} else {
  if(is_BIFAP){print(paste("There is no prevalence data for", regions[reg], pop_prefix))}
  if(is_PHARMO){print("There is no prevalence data")}
 
  # Flowchart when no records exist
  RAM_flowchart_prevalence<-0
  RAM_flowchart_prevalence_psoriasis<-0
  RAM_flowchart_prevalence_acne<- 0
  RAM_flowchart_prevalence_dermatitis<-0
}
#################################################################
#################### INCIDENCE BY INDICATION #################### 
#################################################################
# Data Loading & Cleaning
## RAM Incidence Data ##
if(file.exists(paste0(objective1_temp_dir, pop_prefix,"_RAM_incidence_data.rds"))){
  
  RAM_incidence_data<-as.data.table(readRDS(paste0(objective1_temp_dir, pop_prefix,"_RAM_incidence_data.rds")))
  
  if(nrow(RAM_incidence_data)>0){
    # Drop unneeded columns
    RAM_incidence_data<-RAM_incidence_data[,-c("episode.ID","end.episode.gap.days","episode.duration","birth_date","entry_date","exit_date","current_age","age_group")]
    # Change date format
    RAM_incidence_data[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)]
    # Rename columns
    setnames(RAM_incidence_data, old = c("episode.start","episode.end"), new = c("episode.start.RAM","episode.end.RAM"))
    ## RAM incidence counts
    RAM_incidence_rates<-as.data.table(readRDS(paste0(objective1_dir, "/", pop_prefix, "_RAM_incidence_counts.rds")))
    # Merge these incident retinoid treatment episodes with RAM episodes so that we have both Retinoid and RAM dates per row
    RAM_incidence_data<-retinoid_prevalence_data_first[,c("person_id","ATC.retinoid")][RAM_incidence_data,on=.(person_id)]
    
    # Create column for indication 
    RAM_incidence_data[ATC.retinoid=="D05BB02",indication:="psoriasis"]
    RAM_incidence_data[ATC.retinoid=="D10BA01",indication:="acne"]
    RAM_incidence_data[ATC.retinoid=="D11AH04",indication:="dermatitis"]
    # Flowchart
    RAM_flowchart_incidence<-nrow(RAM_incidence_data)
    RAM_flowchart_incidence_psoriasis<-nrow(RAM_incidence_data[indication=="psoriasis",])
    RAM_flowchart_incidence_acne<- nrow(RAM_incidence_data[indication=="acne",])
    RAM_flowchart_incidence_dermatitis<-nrow(RAM_incidence_data[indication=="dermatitis",])
    
    # Count incidence by indication, month, year
    incidence_by_indication<-RAM_incidence_data[,.N, by = .(year,month, indication)]
    
    for(group in 1:length(unique(incidence_by_indication$indication))){
      # Create a subset of age group
      each_group<-incidence_by_indication[indication==unique(incidence_by_indication$indication)[group]]
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
      # Fills in missing values with 0
      each_group[is.na(N),N:=0][is.na(indication),indication:=unique(incidence_by_indication$indication)[group]]
      # Adjust for PHARMO
      if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
      # Create YM variable
      each_group<-within(each_group,YM<-sprintf("%d-%02d",year,month))
      
      # Prepare denominator
      incidence_count_min <- RAM_incidence_rates[,c("YM","N")]
      setnames(incidence_count_min,"N","Freq")
      
      # Merge age-group subset count with all prevalent counts
      indication_incidence_count<-merge(x=each_group,y=incidence_count_min,by=c("YM"),all.x=TRUE)
      # Masking set at 0
      indication_incidence_count[,masked:=0]
      # Calculates rates
      indication_incidence_count<-indication_incidence_count[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates),rates:=0]
      # Drop columns you don't need
      indication_incidence_count<-indication_incidence_count[,c("YM","N","Freq","rates","masked")]
      
      # Save files in medicine counts folder
      saveRDS(indication_incidence_count, (paste0(objective1_strat_dir,"/", pop_prefix,"_RAM_incidence_counts_", unique(incidence_by_indication$indication)[group],"_indication_group.rds")))
      
    }
  }
} else {
  if(is_BIFAP){print(paste("There is no incidence data for", regions[reg], pop_prefix))}
  if(is_PHARMO){print("There is no incidence data")}
  # Flowchart
  RAM_flowchart_incidence<-0
  RAM_flowchart_incidence_psoriasis<-0
  RAM_flowchart_incidence_acne<- 0
  RAM_flowchart_incidence_dermatitis<-0
}

#######################################################################
#################### DISCONTINUATION BY INDICATION #################### 
#######################################################################
# Data Loading & Cleaning
## RAM Discontinued Data ##
if(file.exists(paste0(objective2_temp_dir, pop_prefix, "_RAM_discontinued_data.rds"))){
  
  RAM_discontinued_data<-as.data.table(readRDS(paste0(objective2_temp_dir, pop_prefix, "_RAM_discontinued_data.rds")))
  
  if(nrow(RAM_discontinued_data)>0){
    # Drops unneeded columns
    RAM_discontinued_data<-RAM_discontinued_data[,-c("episode.ID","end.episode.gap.days","episode.duration","birth_date","entry_date","exit_date","current_age","age_group")]
    
    # Change date format
    RAM_discontinued_data[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)]
    # Rename columns
    setnames(RAM_discontinued_data, old = c("episode.start","episode.end"), new = c("episode.start.RAM","episode.end.RAM"))
    
    ## RAM discontinued counts
    RAM_discontinued_rates<-as.data.table(readRDS(paste0(objective2_dir, "/", pop_prefix, "_RAM_discontinued_counts.rds")))
    
    # Merge these incident retinoid treatment episodes with RAM episodes so that we have both Retinoid and RAM dates per row
    RAM_discontinued_data<-retinoid_prevalence_data_first[,c("person_id","ATC.retinoid")][RAM_discontinued_data,on=.(person_id)]
    # Create column for indication 
    RAM_discontinued_data[ATC.retinoid=="D05BB02",indication:="psoriasis"]
    RAM_discontinued_data[ATC.retinoid=="D10BA01",indication:="acne"]
    RAM_discontinued_data[ATC.retinoid=="D11AH04",indication:="dermatitis"]
    
    # Flowchart
    RAM_flowchart_discontinued<-nrow(RAM_discontinued_data)
    RAM_flowchart_discontinued_psoriasis<-nrow(RAM_discontinued_data[indication=="psoriasis",])
    RAM_flowchart_discontinued_acne<- nrow(RAM_discontinued_data[indication=="acne",])
    RAM_flowchart_discontinued_dermatitis<-nrow(RAM_discontinued_data[indication=="dermatitis",])
    
    # Count incidence by indication, month, year
    discontinued_by_indication<-RAM_discontinued_data[,.N, by = .(year,month, indication)]
    
    for(group in 1:length(unique(discontinued_by_indication$indication))){
      # Create a subset of age group
      each_group<-discontinued_by_indication[indication==unique(discontinued_by_indication$indication)[group]]
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
      # Fills in missing values with 0
      each_group[is.na(N),N:=0][is.na(indication),indication:=unique(discontinued_by_indication$indication)[group]]
      # Adjust for PHARMO
      if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
      # Create YM variable
      each_group<-within(each_group,YM<-sprintf("%d-%02d",year,month))
      
      # Prepare denominator
      discontinued_count_min <- RAM_discontinued_rates[,c("YM","N")]
      setnames(discontinued_count_min,"N","Freq")
      
      # Merge age-group subset count with all prevalent counts
      indication_discontinued_count<-merge(x=each_group,y=discontinued_count_min,by=c("YM"),all.x=TRUE)
      # Masking set at 0
      indication_discontinued_count[,masked:=0]
      # Calculates rates
      indication_discontinued_count<-indication_discontinued_count[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates),rates:=0]
      # Drop columns you don't need
      indication_discontinued_count<-indication_discontinued_count[,c("YM","N","Freq","rates","masked")]
      
      # Save files in medicine counts folder
      saveRDS(indication_discontinued_count, (paste0(objective2_strat_dir,"/", pop_prefix,"_RAM_discontinued_counts_", unique(discontinued_by_indication$indication)[group],"_indication_group.rds")))
      
    }
  }
} else {
  if(is_BIFAP){print(paste("There is no discontined data for", regions[reg], pop_prefix))}
  if(is_PHARMO){print("There is no discontined data")}
  # Flowchart
  RAM_flowchart_discontinued<-0
  RAM_flowchart_discontinued_psoriasis<-0
  RAM_flowchart_discontinued_acne<- 0
  RAM_flowchart_discontinued_dermatitis<-0
}


#################### SWITCHERS BY INDICATION #################### 
#################################################################
# Data Loading & Cleaning
## RAM Switcher Data ##
if(file.exists(paste0(objective2_temp_dir, pop_prefix, "_RAM_switcher_data.rds"))){
  
  RAM_switcher_data<-as.data.table(readRDS(paste0(objective2_temp_dir, pop_prefix, "_RAM_switcher_data.rds")))
  
  if(nrow(RAM_switcher_data)>0){
    # Drop unneeded columns
    RAM_switcher_data<-RAM_switcher_data[,-c("episode.start.retinoid","episode.end.retinoid","ATC.retinoid","birth_date","entry_date","exit_date","current_age","age_group" )]
    # Change date format
    RAM_switcher_data[,episode.start.RAM:=as.IDate(episode.start.RAM)][,episode.end.RAM:=as.IDate(episode.end.RAM)]
    
    ## RAM switcher counts
    RAM_switcher_rates1<-as.data.table(readRDS(paste0(objective2_dir, "/", pop_prefix, "_RAM_switcher_1_counts.rds")))
    
    # Merge these incident retinoid treatment episodes with RAM episodes so that we have both Retinoid and RAM dates per row
    RAM_switcher_data<-retinoid_prevalence_data_first[,c("person_id","ATC.retinoid")][RAM_switcher_data,on=.(person_id)]
    # Create column for indication 
    RAM_switcher_data[ATC.retinoid=="D05BB02",indication:="psoriasis"]
    RAM_switcher_data[ATC.retinoid=="D10BA01",indication:="acne"]
    RAM_switcher_data[ATC.retinoid=="D11AH04",indication:="dermatitis"]
    
    # Flowchart
    RAM_flowchart_switcher<-nrow(RAM_switcher_data)
    RAM_flowchart_switcher_psoriasis<-nrow(RAM_switcher_data[indication=="psoriasis",])
    RAM_flowchart_switcher_acne<- nrow(RAM_switcher_data[indication=="acne",])
    RAM_flowchart_switcher_dermatitis<-nrow(RAM_switcher_data[indication=="dermatitis",])
    
    # Count incidence by indication, month, year
    switcher_by_indication<-RAM_switcher_data[,.N, by = .(year,month, indication)]
    
    for(group in 1:length(unique(switcher_by_indication$indication))){
      # Create a subset of age group
      each_group<-switcher_by_indication[indication==unique(switcher_by_indication$indication)[group]]
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
      # Fills in missing values with 0
      each_group[is.na(N),N:=0][is.na(indication),indication:=unique(switcher_by_indication$indication)[group]]
      # Adjust for PHARMO
      if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
      # Create YM variable
      each_group<-within(each_group,YM<-sprintf("%d-%02d",year,month))
      
      # Prepare denominator
      switcher_count_min <- RAM_switcher_rates1[,c("YM","N")]
      setnames(switcher_count_min,"N","Freq")
      
      # Merge age-group subset count with all prevalent counts
      indication_switcher_count<-merge(x=each_group,y=switcher_count_min,by=c("YM"),all.x=TRUE)
      # Masking set at 0
      indication_switcher_count[,masked:=0]
      # Calculates rates
      indication_switcher_count<-indication_switcher_count[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates),rates:=0]
      # Drop columns you don't need
      indication_switcher_count<-indication_switcher_count[,c("YM","N","Freq","rates","masked")]
      
      # Save files in medicine counts folder
      saveRDS(indication_switcher_count, (paste0(objective2_strat_dir,"/", pop_prefix,"_RAM_switcher_counts_", unique(switcher_by_indication$indication)[group],"_indication_group.rds")))
      
    }
  }
} else {
  if(is_BIFAP){print(paste("There are no switcher files for", regions[reg], pop_prefix))}
  if(is_PHARMO){print("There are no switcher files")}
  # Flowchart
  RAM_flowchart_switcher<-0
  RAM_flowchart_switcher_psoriasis<-0
  RAM_flowchart_switcher_acne<-0
  RAM_flowchart_switcher_dermatitis<-0
}
############################################################################
#################### GENERAL CONCOMITANCE BY INDICATION #################### 
############################################################################
# Data Loading & Cleaning
## RAM Concomitant Data ##
if (file.exists(paste0(objective3_temp_dir, pop_prefix, "_RAM_general_concomit_RECORDS_data.rds"))){
  
  RAM_concomit_data<- as.data.table(readRDS(paste0(objective3_temp_dir, pop_prefix, "_RAM_general_concomit_RECORDS_data.rds")))

  if(nrow(RAM_concomit_data)>0){
    # Get 1 per person id per month-year
    RAM_concomit_user<- unique(RAM_concomit_data, by=c("person_id", "year", "month"))
    # Drop unneeded columns
    RAM_concomit_user<-RAM_concomit_user[,-c("episode.start.retinoid","episode.end.retinoid","ATC.retinoid","birth_date","entry_date","exit_date","current_age","age_group")]
    # Change date format
    RAM_concomit_user[,episode.start.RAM:=as.IDate(episode.start.RAM)][,episode.end.RAM:=as.IDate(episode.end.RAM)]
    
    ## RAM concomit counts
    RAM_concomit_rates<-RAM_concomit_user[,.N, by = .(year,month)]
    # Adjust for PHARMO
    if(is_PHARMO){RAM_concomit_rates<-RAM_concomit_rates[year < 2020,]} else {RAM_concomit_rates<-RAM_concomit_rates[year < 2021,]}
    # Merge with empty df (for counts that do not have counts for all months and years of study)
    RAM_concomit_rates <-as.data.table(merge(x = empty_df, y = RAM_concomit_rates, by = c("year", "month"), all.x = TRUE))
    # Fills in missing values with 0
    RAM_concomit_rates[is.na(RAM_concomit_rates[,N]), N:=0]
    # Create YM variable
    RAM_concomit_rates<-within(RAM_concomit_rates, YM<- sprintf("%d-%02d", year, month))
    
    # Merge these incident retinoid treatment episodes with RAM episodes so that we have both Retinoid and RAM dates per row
    RAM_concomit_data<-retinoid_prevalence_data_first[,c("person_id","ATC.retinoid")][RAM_concomit_user,on=.(person_id)]
    # Create column for indication 
    RAM_concomit_data[ATC.retinoid=="D05BB02",indication:="psoriasis"]
    RAM_concomit_data[ATC.retinoid=="D10BA01",indication:="acne"]
    RAM_concomit_data[ATC.retinoid=="D11AH04",indication:="dermatitis"]
    
    # Flowchart
    RAM_flowchart_concomit_records<-nrow(RAM_concomit_data)
    RAM_flowchart_concomit_users<-nrow(unique(RAM_concomit_data, by="person_id"))
    RAM_flowchart_concomit_psoriasis<-nrow(unique(RAM_concomit_data[indication=="psoriasis",], by="person_id"))
    RAM_flowchart_concomit_acne<- nrow(unique(RAM_concomit_data[indication=="acne",], by="person_id"))
    RAM_flowchart_concomit_dermatitis<-nrow(unique(RAM_concomit_data[indication=="dermatitis",], by="person_id"))
    
    # Count incidence by indication, month, year
    concomit_by_indication<-RAM_concomit_data[,.N, by = .(year,month, indication)]
    
    for(group in 1:length(unique(concomit_by_indication$indication))){
      # Create a subset of age group
      each_group<-concomit_by_indication[indication==unique(concomit_by_indication$indication)[group]]
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
      # Fills in missing values with 0
      each_group[is.na(N),N:=0][is.na(indication),indication:=unique(concomit_by_indication$indication)[group]]
      # Adjust for PHARMO
      if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
      # Create YM variable
      each_group<-within(each_group,YM<-sprintf("%d-%02d",year,month))
      
      # Prepare denominator
      concomit_count_min <- RAM_concomit_rates[,c("YM","N")]
      setnames(concomit_count_min,"N","Freq")
      
      # Merge age-group subset count with all prevalent counts
      indication_concomit_count<-merge(x=each_group,y=concomit_count_min,by=c("YM"),all.x=TRUE)
      # Masking set at 0
      indication_concomit_count[,masked:=0]
      # Calculates rates
      indication_concomit_count<-indication_concomit_count[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates),rates:=0]
      # Drop columns you don't need
      indication_concomit_count<-indication_concomit_count[,c("YM","N","Freq","rates","masked")]
      
      # Save files in medicine counts folder
      saveRDS(indication_concomit_count, (paste0(objective3_strat_dir,"/", pop_prefix,"_RAM_general_concomit_USERS_counts_", unique(concomit_by_indication$indication)[group],"_indication_group.rds")))
      
    }
  }
  
  ############################################################################
  #################### CONTRAINDICATED BY INDICATION #########################
  ############################################################################
  
  # Load contra USER data
  RAM_contra_USERS_data<-as.data.table(readRDS(paste0(objective3_temp_dir, pop_prefix,"_RAM_contra_USERS_data.rds"))) 
  RAM_countra_USERS_rates<-as.data.table(readRDS(paste0(objective3_dir,"/", pop_prefix, "_RAM_contra_USERS_counts.rds"))) 
  
  if(nrow(RAM_contra_USERS_data)>0){
    # Drop unneeded columns
    RAM_contra_USERS_data<-RAM_contra_USERS_data[,-c("episode.start.retinoid","episode.end.retinoid","ATC.retinoid","birth_date","entry_date","exit_date","current_age","age_group")]
    # Merge these incident retinoid treatment episodes so that we have both Retinoid and RAM dates per row
    RAM_contra_USERS_data<-retinoid_prevalence_data_first[,c("person_id","ATC.retinoid")][RAM_contra_USERS_data,on=.(person_id)]
    # Create column for indication 
    RAM_contra_USERS_data[ATC.retinoid=="D05BB02",indication:="psoriasis"]
    RAM_contra_USERS_data[ATC.retinoid=="D10BA01",indication:="acne"]
    RAM_contra_USERS_data[ATC.retinoid=="D11AH04",indication:="dermatitis"]
    
    # Flowchart
    RAM_flowchart_concomit_contraindicated_users<-nrow(unique(RAM_contra_USERS_data, by="person_id"))
    RAM_flowchart_concomit_contraindicated_psoriasis<-nrow(unique(RAM_contra_USERS_data[indication=="psoriasis",], by="person_id"))
    RAM_flowchart_concomit_contraindicated_acne<- nrow(unique(RAM_contra_USERS_data[indication=="acne",], by="person_id"))
    RAM_flowchart_concomit_contraindicated_dermatitis<-nrow(unique(RAM_contra_USERS_data[indication=="dermatitis",], by="person_id"))
    
    # Count incidence by indication, month, year
    contra_by_indication<-RAM_contra_USERS_data[,.N, by = .(year,month, indication)]
    
    for(group in 1:length(unique(contra_by_indication$indication))){
      # Create a subset of age group
      each_group<-contra_by_indication[indication==unique(contra_by_indication$indication)[group]]
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
      # Fills in missing values with 0
      each_group[is.na(N),N:=0][is.na(indication),indication:=unique(contra_by_indication$indication)[group]]
      # Adjust for PHARMO
      if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
      # Create YM variable
      each_group<-within(each_group,YM<-sprintf("%d-%02d",year,month))
      
      # Prepare denominator
      contra_count_min <- RAM_countra_USERS_rates[,c("YM","N")]
      setnames(contra_count_min,"N","Freq")
      
      # Merge age-group subset count with all prevalent counts
      indication_contra_count<-merge(x=each_group,y=contra_count_min,by=c("YM"),all.x=TRUE)
      # Masking set at 0
      indication_contra_count[,masked:=0]
      # Calculates rates
      indication_contra_count<-indication_contra_count[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates),rates:=0]
      # Drop columns you don't need
      indication_contra_count<-indication_contra_count[,c("YM","N","Freq","rates","masked")]
      
      # Save files in medicine counts folder
      saveRDS(indication_contra_count, (paste0(objective3_strat_dir,"/", pop_prefix,"_RAM_contra_USERS_counts_", unique(contra_by_indication$indication)[group],"_indication_group.rds")))
      
    }
  } else {
    if(is_BIFAP){print(paste("There are no contra files for", regions[reg], pop_prefix))}
    if(is_PHARMO){print("There are no contra files")}
    # Flowchart
    RAM_flowchart_concomit_contraindicated_records<-0
    RAM_flowchart_concomit_contraindicated_users<-0
    RAM_flowchart_concomit_contraindicated_psoriasis<-0
    RAM_flowchart_concomit_contraindicated_acne<- 0
    RAM_flowchart_concomit_contraindicated_dermatitis<-0
    
  }
} else {
  if(is_BIFAP){print(paste("There is no concomitant data for", regions[reg], pop_prefix))}
  if(is_PHARMO){print("There is no concomitant data")}
  # Flowchart
  RAM_flowchart_concomit_contraindicated_records<-0
  RAM_flowchart_concomit_contraindicated_users<-0
  RAM_flowchart_concomit_contraindicated_psoriasis<-0
  RAM_flowchart_concomit_contraindicated_acne<- 0
  RAM_flowchart_concomit_contraindicated_dermatitis<-0
  
}

############################################################################
#################### TERATOGENIC BY INDICATION #########################
############################################################################
# teratogenic
if (file.exists(paste0(objective4_temp_dir, pop_prefix, "_RAM_teratogenic_USERS_data.rds"))){
  
  RAM_teratogenic_USERS_data<-as.data.table(readRDS(paste0(objective4_temp_dir, pop_prefix, "_RAM_teratogenic_USERS_data.rds")))
  
  if(nrow(RAM_teratogenic_USERS_data)>0){
     # Drop unneeded columns
    RAM_teratogenic_USERS_data<-RAM_teratogenic_USERS_data[,c("person_id","ATC.RAM","year","month","ATC.retinoid")]
    ## RAM user teratogenic counts # DENOMINATOR
    RAM_teratogenic_USER_counts<-as.data.table(readRDS(paste0(objective4_dir,"/", pop_prefix, "_RAM_teratogenic_USERS_counts.rds")))
    # Create column for indication 
    RAM_teratogenic_USERS_data[ATC.retinoid=="D05BB02",indication:="psoriasis"]
    RAM_teratogenic_USERS_data[ATC.retinoid=="D10BA01",indication:="acne"]
    RAM_teratogenic_USERS_data[ATC.retinoid=="D11AH04",indication:="dermatitis"]
    
    # Flowchart
    RAM_flowchart_teratogenic_users<-nrow(unique(RAM_teratogenic_USERS_data, by="person_id"))
    RAM_flowchart_teratogenic_psoriasis<-nrow(unique(RAM_teratogenic_USERS_data[indication=="psoriasis",], by="person_id"))
    RAM_flowchart_teratogenic_acne<- nrow(unique(RAM_teratogenic_USERS_data[indication=="acne",], by="person_id"))
    RAM_flowchart_teratogenic_dermatitis<-nrow(unique(RAM_teratogenic_USERS_data[indication=="dermatitis",], by="person_id"))
    
    # Count incidence by indication, month, year
    teratogenic_by_indication<-RAM_teratogenic_USERS_data[,.N, by = .(year,month, indication)]
    
    for(group in 1:length(unique(teratogenic_by_indication$indication))){
      # Create a subset of age group
      each_group<-teratogenic_by_indication[indication==unique(teratogenic_by_indication$indication)[group]]
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
      # Fills in missing values with 0
      each_group[is.na(N),N:=0][is.na(indication),indication:=unique(teratogenic_by_indication$indication)[group]]
      # Adjust for PHARMO
      if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
      # Create YM variable
      each_group<-within(each_group,YM<-sprintf("%d-%02d",year,month))
      
      # Prepare denominator
      teratogenic_count_min <- RAM_teratogenic_USER_counts[,c("YM","N")]
      setnames(teratogenic_count_min,"N","Freq")
      
      # Merge age-group subset count with all prevalent counts
      indication_teratogenic_count<-merge(x=each_group,y=teratogenic_count_min,by=c("YM"),all.x=TRUE)
      # Masking set at 0
      indication_teratogenic_count[,masked:=0]
      # Calculates rates
      indication_teratogenic_count<-indication_teratogenic_count[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates),rates:=0]
      # Drop columns you don't need
      indication_teratogenic_count<-indication_teratogenic_count[,c("YM","N","Freq","rates","masked")]
      
      # Save files in medicine counts folder
      saveRDS(indication_teratogenic_count, (paste0(objective4_strat_dir,"/", pop_prefix,"_RAM_teratogenic_USERS_counts_", unique(teratogenic_by_indication$indication)[group],"_indication_group.rds")))
      
    }
  }
} else {
  if(is_BIFAP){print(paste("There is no teratogenic data for", regions[reg], pop_prefix))}
  if(is_PHARMO){print("There is no teratogenic data")}
  # Flowchart
  RAM_flowchart_teratogenic_records<-0
  RAM_flowchart_teratogenic_users<-0
  RAM_flowchart_teratogenic_psoriasis<-0
  RAM_flowchart_teratogenic_acne<- 0
  RAM_flowchart_teratogenic_dermatitis<-0
  
}

# Clean up 
rm(list = grep("concomit_by|t_min|by_indication|each_group|indication_|RAM_c|RAM_d|RAM_i|RAM_p|RAM_s|retinoid_incidence_data|denominator|retinoid_meds", ls(), value = TRUE))




