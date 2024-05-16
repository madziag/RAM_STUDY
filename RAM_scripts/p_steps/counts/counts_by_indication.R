##################################################################
#################### PREVALENCE BY INDICATION #################### 
##################################################################
# Data Loading & Cleaning
## RAM Prevalent Data ##
RAM_prevalence_data<-as.data.table(readRDS(paste0(objective1_temp_dir, pop_prefix,"_RAM_prevalence_data.rds")))
RAM_prevalence_data<-RAM_prevalence_data[,-c("episode.ID","end.episode.gap.days","episode.duration","birth_date","entry_date","exit_date","current_age","age_group")]

# Change date format
RAM_prevalence_data[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)]
# Rename columns
setnames(RAM_prevalence_data, old = c("episode.start","episode.end"), new = c("episode.start.RAM","episode.end.RAM"))

## RAM prevalence counts
RAM_prevalence_rates<-as.data.table(readRDS(paste0(objective1_dir, "/", pop_prefix, "_RAM_prevalence_counts.rds")))
## Retinoid Prevalent Data ## 
retinoid_prevalence_data<-as.data.table(readRDS(paste0(retinoid_counts_dfs, pop_prefix,"_Retinoid_prevalence_data.rds")))
# # Remove duplicates
# retinoid_prevalence_data<-unique(retinoid_prevalence_data, by = c("person_id", "episode.start", "episode.end", "ATC.retinoid"))
# Change date format
retinoid_prevalence_data[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)]
# Rename columns
setnames(retinoid_prevalence_data, old = c("episode.start","episode.end"), new = c("episode.start.retinoid","episode.end.retinoid"))
# Keep first ever prescription per person id-retinoid atc
retinoid_prevalence_data<-retinoid_prevalence_data[order(person_id, ATC.retinoid,episode.start.retinoid)]
retinoid_prevalence_data<-retinoid_prevalence_data[, head(.SD, 1), by = c("person_id","ATC.retinoid")]

retinoid_prevalence_data_psoriasis<-retinoid_prevalence_data[ATC.retinoid=="D05BB02",]
retinoid_prevalence_data_acne<-retinoid_prevalence_data[ATC.retinoid=="D10BA01",]
retinoid_prevalence_data_dermatitis<-retinoid_prevalence_data[ATC.retinoid=="D11AH04",]

RAM_prevalence_data_psoriasis<-RAM_prevalence_data[ATC.RAM%chin%psoriasis_codes,]
RAM_prevalence_data_acne<-RAM_prevalence_data[ATC.RAM%chin%acne_codes,]
RAM_prevalence_data_dermatitis<-RAM_prevalence_data[ATC.RAM%chin%dermatitis_codes,]

# Psoriasis 
RAM_prevalence_data_psoriasis<-retinoid_prevalence_data_psoriasis[,c("person_id","ATC.retinoid","episode.start.retinoid","episode.end.retinoid")][RAM_prevalence_data_psoriasis,on=.(person_id)] 
RAM_prevalence_data_psoriasis[(episode.start.RAM>=episode.start.retinoid|episode.start.RAM>=episode.end.retinoid|episode.end.RAM>=episode.start.retinoid|episode.end.RAM>=episode.end.retinoid)&ATC.retinoid=="D05BB02",RAM.indication:="psoriasis"]
RAM_prevalence_data_psoriasis[is.na(RAM.indication),RAM.indication:="unknown"]
# Acne
RAM_prevalence_data_acne<-retinoid_prevalence_data_acne[,c("person_id","ATC.retinoid","episode.start.retinoid","episode.end.retinoid")][RAM_prevalence_data_acne,on=.(person_id)] 
RAM_prevalence_data_acne[(episode.start.RAM>=episode.start.retinoid|episode.start.RAM>=episode.end.retinoid|episode.end.RAM>=episode.start.retinoid|episode.end.RAM>=episode.end.retinoid)&ATC.retinoid=="D10BA01",RAM.indication:="acne"]
RAM_prevalence_data_acne[is.na(RAM.indication),RAM.indication:="unknown"]
# Dermatitis
RAM_prevalence_data_dermatitis<-retinoid_prevalence_data_dermatitis[,c("person_id","ATC.retinoid","episode.start.retinoid","episode.end.retinoid")][RAM_prevalence_data_dermatitis,on=.(person_id)] 
RAM_prevalence_data_dermatitis[(episode.start.RAM>=episode.start.retinoid|episode.start.RAM>=episode.end.retinoid|episode.end.RAM>=episode.start.retinoid|episode.end.RAM>=episode.end.retinoid)&ATC.retinoid=="D11AH04",RAM.indication:="dermatitis"]
RAM_prevalence_data_dermatitis[is.na(RAM.indication),RAM.indication:="unknown"]

# Bind the three indications 
RAM_prevalence_per_indication_all<-rbindlist(list(RAM_prevalence_data_psoriasis,RAM_prevalence_data_acne,RAM_prevalence_data_dermatitis))
RAM_prevalence_per_indication_all<-RAM_prevalence_per_indication_all[,-c("ATC.retinoid","episode.start.retinoid","episode.end.retinoid")]
RAM_prevalence_per_indication_all<-unique(RAM_prevalence_per_indication_all)

# Count prevalence by indication, month, year
prevalence_by_indication<-RAM_prevalence_per_indication_all[,.N, by = .(year,month, RAM.indication)]

for(group in 1:length(unique(prevalence_by_indication$RAM.indication))){
  # Create a subset of age group
  each_group<-prevalence_by_indication[RAM.indication==unique(prevalence_by_indication$RAM.indication)[group]]
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
  # Fills in missing values with 0
  each_group[is.na(N),N:=0][is.na(RAM.indication),RAM.indication:=unique(prevalence_by_indication$RAM.indication)[group]]
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
  saveRDS(indication_prevalence_count, (paste0(objective1_strat_dir,"/", pop_prefix,"_RAM_prevalence_counts_", unique(prevalence_by_indication$RAM.indication)[group],"_indication_group.rds")))

}

#################################################################
#################### INCIDENCE BY INDICATION #################### 
#################################################################
# Data Loading & Cleaning
## RAM Incidence Data ##
RAM_incidence_data<-as.data.table(readRDS(paste0(objective1_temp_dir, pop_prefix,"_RAM_incidence_data.rds")))
RAM_incidence_data<-RAM_incidence_data[,-c("episode.ID","end.episode.gap.days","episode.duration","birth_date","entry_date","exit_date","current_age","age_group")]

# Change date format
RAM_incidence_data[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)]
# Rename columns
setnames(RAM_incidence_data, old = c("episode.start","episode.end"), new = c("episode.start.RAM","episode.end.RAM"))

## RAM incidence counts
RAM_incidence_rates<-as.data.table(readRDS(paste0(objective1_dir, "/", pop_prefix, "_RAM_incidence_counts.rds")))

# Create Indication subsets based on RAM ATC
RAM_incidence_data_psoriasis<-RAM_incidence_data[ATC.RAM%chin%psoriasis_codes,]
RAM_incidence_data_acne<-RAM_incidence_data[ATC.RAM%chin%acne_codes,]
RAM_incidence_data_dermatitis<-RAM_incidence_data[ATC.RAM%chin%dermatitis_codes,]

# Merge with corresponding Retinoid subsets 
# Psoriasis 
RAM_incidence_data_psoriasis<-retinoid_prevalence_data_psoriasis[,c("person_id","ATC.retinoid","episode.start.retinoid","episode.end.retinoid")][RAM_incidence_data_psoriasis,on=.(person_id)] 
RAM_incidence_data_psoriasis[(episode.start.RAM>=episode.start.retinoid|episode.start.RAM>=episode.end.retinoid|episode.end.RAM>=episode.start.retinoid|episode.end.RAM>=episode.end.retinoid)&ATC.retinoid=="D05BB02",RAM.indication:="psoriasis"]
RAM_incidence_data_psoriasis[is.na(RAM.indication),RAM.indication:="unknown"]
# Acne
RAM_incidence_data_acne<-retinoid_prevalence_data_acne[,c("person_id","ATC.retinoid","episode.start.retinoid","episode.end.retinoid")][RAM_incidence_data_acne,on=.(person_id)] 
RAM_incidence_data_acne[(episode.start.RAM>=episode.start.retinoid|episode.start.RAM>=episode.end.retinoid|episode.end.RAM>=episode.start.retinoid|episode.end.RAM>=episode.end.retinoid)&ATC.retinoid=="D10BA01",RAM.indication:="acne"]
RAM_incidence_data_acne[is.na(RAM.indication),RAM.indication:="unknown"]
# Dermatitis
RAM_incidence_data_dermatitis<-retinoid_prevalence_data_dermatitis[,c("person_id","ATC.retinoid","episode.start.retinoid","episode.end.retinoid")][RAM_incidence_data_dermatitis,on=.(person_id)] 
RAM_incidence_data_dermatitis[(episode.start.RAM>=episode.start.retinoid|episode.start.RAM>=episode.end.retinoid|episode.end.RAM>=episode.start.retinoid|episode.end.RAM>=episode.end.retinoid)&ATC.retinoid=="D11AH04",RAM.indication:="dermatitis"]
RAM_incidence_data_dermatitis[is.na(RAM.indication),RAM.indication:="unknown"]

# Bind the three indications 
RAM_incidence_per_indication_all<-rbindlist(list(RAM_incidence_data_psoriasis,RAM_incidence_data_acne,RAM_incidence_data_dermatitis))
RAM_incidence_per_indication_all<-RAM_incidence_per_indication_all[,-c("ATC.retinoid","episode.start.retinoid","episode.end.retinoid")]
RAM_incidence_per_indication_all<-unique(RAM_incidence_per_indication_all)

# Count incidence by indication, month, year
incidence_by_indication<-RAM_incidence_per_indication_all[,.N, by = .(year,month, RAM.indication)]

for(group in 1:length(unique(incidence_by_indication$RAM.indication))){
  # Create a subset of age group
  each_group<-incidence_by_indication[RAM.indication==unique(incidence_by_indication$RAM.indication)[group]]
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
  # Fills in missing values with 0
  each_group[is.na(N),N:=0][is.na(RAM.indication),RAM.indication:=unique(incidence_by_indication$RAM.indication)[group]]
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
  saveRDS(indication_incidence_count, (paste0(objective1_strat_dir,"/", pop_prefix,"_RAM_incidence_counts_", unique(incidence_by_indication$RAM.indication)[group],"_indication_group.rds")))

}

#######################################################################
#################### DISCONTINUATION BY INDICATION #################### 
#######################################################################
# Data Loading & Cleaning
## RAM Discontinued Data ##
RAM_discontinued_data<-as.data.table(readRDS(paste0(objective2_temp_dir, pop_prefix, "_RAM_discontinued_data.rds")))
RAM_discontinued_data<-RAM_discontinued_data[,-c("episode.ID","end.episode.gap.days","episode.duration","birth_date","entry_date","exit_date","current_age","age_group")]

# Change date format
RAM_discontinued_data[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)]
# Rename columns
setnames(RAM_discontinued_data, old = c("episode.start","episode.end"), new = c("episode.start.RAM","episode.end.RAM"))

## RAM discontinued counts
RAM_discontinued_rates<-as.data.table(readRDS(paste0(objective2_dir, "/", pop_prefix, "_RAM_discontinued_counts.rds")))

# Create Indication subsets based on RAM ATC
RAM_discontinued_data_psoriasis<-RAM_discontinued_data[ATC.RAM%chin%psoriasis_codes,]
RAM_discontinued_data_acne<-RAM_discontinued_data[ATC.RAM%chin%acne_codes,]
RAM_discontinued_data_dermatitis<-RAM_discontinued_data[ATC.RAM%chin%dermatitis_codes,]

# Merge with corresponding Retinoid subsets 
# Psoriasis 
RAM_discontinued_data_psoriasis<-retinoid_prevalence_data_psoriasis[,c("person_id","ATC.retinoid","episode.start.retinoid","episode.end.retinoid")][RAM_discontinued_data_psoriasis,on=.(person_id)] 
RAM_discontinued_data_psoriasis[(episode.start.RAM>=episode.start.retinoid|episode.start.RAM>=episode.end.retinoid|episode.end.RAM>=episode.start.retinoid|episode.end.RAM>=episode.end.retinoid)&ATC.retinoid=="D05BB02",RAM.indication:="psoriasis"]
RAM_discontinued_data_psoriasis[is.na(RAM.indication),RAM.indication:="unknown"]
# Acne
RAM_discontinued_data_acne<-retinoid_prevalence_data_acne[,c("person_id","ATC.retinoid","episode.start.retinoid","episode.end.retinoid")][RAM_discontinued_data_acne,on=.(person_id)] 
RAM_discontinued_data_acne[(episode.start.RAM>=episode.start.retinoid|episode.start.RAM>=episode.end.retinoid|episode.end.RAM>=episode.start.retinoid|episode.end.RAM>=episode.end.retinoid)&ATC.retinoid=="D10BA01",RAM.indication:="acne"]
RAM_discontinued_data_acne[is.na(RAM.indication),RAM.indication:="unknown"]
# Dermatitis
RAM_discontinued_data_dermatitis<-retinoid_prevalence_data_dermatitis[,c("person_id","ATC.retinoid","episode.start.retinoid","episode.end.retinoid")][RAM_discontinued_data_dermatitis,on=.(person_id)] 
RAM_discontinued_data_dermatitis[(episode.start.RAM>=episode.start.retinoid|episode.start.RAM>=episode.end.retinoid|episode.end.RAM>=episode.start.retinoid|episode.end.RAM>=episode.end.retinoid)&ATC.retinoid=="D11AH04",RAM.indication:="dermatitis"]
RAM_discontinued_data_dermatitis[is.na(RAM.indication),RAM.indication:="unknown"]

# Bind the three indications 
RAM_discontinued_per_indication_all<-rbindlist(list(RAM_discontinued_data_psoriasis,RAM_discontinued_data_acne,RAM_discontinued_data_dermatitis))
RAM_discontinued_per_indication_all<-RAM_discontinued_per_indication_all[,-c("ATC.retinoid","episode.start.retinoid","episode.end.retinoid")]
RAM_discontinued_per_indication_all<-unique(RAM_discontinued_per_indication_all)

# Count incidence by indication, month, year
discontinued_by_indication<-RAM_discontinued_per_indication_all[,.N, by = .(year,month, RAM.indication)]

for(group in 1:length(unique(discontinued_by_indication$RAM.indication))){
  # Create a subset of age group
  each_group<-discontinued_by_indication[RAM.indication==unique(discontinued_by_indication$RAM.indication)[group]]
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
  # Fills in missing values with 0
  each_group[is.na(N),N:=0][is.na(RAM.indication),RAM.indication:=unique(discontinued_by_indication$RAM.indication)[group]]
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
  saveRDS(indication_discontinued_count, (paste0(objective2_strat_dir,"/", pop_prefix,"_RAM_discontinued_counts_", unique(discontinued_by_indication$RAM.indication)[group],"_indication_group.rds")))
  
}


#################################################################
#################### SWITCHERS BY INDICATION #################### 
#################################################################
# Data Loading & Cleaning
## RAM Switcher Data ##
RAM_switcher_data<-as.data.table(readRDS(paste0(objective2_temp_dir, pop_prefix, "_RAM_switcher_data.rds")))
RAM_switcher_data<-RAM_switcher_data[,-c("episode.start.retinoid","episode.end.retinoid","ATC.retinoid","birth_date","entry_date","exit_date","current_age","age_group" )]
# Change date format
RAM_switcher_data[,episode.start.RAM:=as.IDate(episode.start.RAM)][,episode.end.RAM:=as.IDate(episode.end.RAM)]

## RAM switcher counts
RAM_switcher_rates1<-as.data.table(readRDS(paste0(objective2_dir, "/", pop_prefix, "_RAM_switcher_1_counts.rds")))

# Create Indication subsets based on RAM ATC
RAM_switcher_data_psoriasis<-RAM_switcher_data[ATC.RAM%chin%psoriasis_codes,]
RAM_switcher_data_acne<-RAM_switcher_data[ATC.RAM%chin%acne_codes,]
RAM_switcher_data_dermatitis<-RAM_switcher_data[ATC.RAM%chin%dermatitis_codes,]

# Merge with corresponding Retinoid subsets 
# Psoriasis 
RAM_switcher_data_psoriasis<-retinoid_prevalence_data_psoriasis[,c("person_id","ATC.retinoid","episode.start.retinoid","episode.end.retinoid")][RAM_switcher_data_psoriasis,on=.(person_id)] 
RAM_switcher_data_psoriasis[(episode.start.RAM>=episode.start.retinoid|episode.start.RAM>=episode.end.retinoid|episode.end.RAM>=episode.start.retinoid|episode.end.RAM>=episode.end.retinoid)&ATC.retinoid=="D05BB02",RAM.indication:="psoriasis"]
RAM_switcher_data_psoriasis[is.na(RAM.indication),RAM.indication:="unknown"]
# Acne
RAM_switcher_data_acne<-retinoid_prevalence_data_acne[,c("person_id","ATC.retinoid","episode.start.retinoid","episode.end.retinoid")][RAM_switcher_data_acne,on=.(person_id)] 
RAM_switcher_data_acne[(episode.start.RAM>=episode.start.retinoid|episode.start.RAM>=episode.end.retinoid|episode.end.RAM>=episode.start.retinoid|episode.end.RAM>=episode.end.retinoid)&ATC.retinoid=="D10BA01",RAM.indication:="acne"]
RAM_switcher_data_acne[is.na(RAM.indication),RAM.indication:="unknown"]
# Dermatitis
RAM_switcher_data_dermatitis<-retinoid_prevalence_data_dermatitis[,c("person_id","ATC.retinoid","episode.start.retinoid","episode.end.retinoid")][RAM_switcher_data_dermatitis,on=.(person_id)] 
RAM_switcher_data_dermatitis[(episode.start.RAM>=episode.start.retinoid|episode.start.RAM>=episode.end.retinoid|episode.end.RAM>=episode.start.retinoid|episode.end.RAM>=episode.end.retinoid)&ATC.retinoid=="D11AH04",RAM.indication:="dermatitis"]
RAM_switcher_data_dermatitis[is.na(RAM.indication),RAM.indication:="unknown"]

# Bind the three indications 
RAM_switcher_per_indication_all<-rbindlist(list(RAM_switcher_data_psoriasis,RAM_switcher_data_acne,RAM_switcher_data_dermatitis))
RAM_switcher_per_indication_all<-RAM_switcher_per_indication_all[,-c("ATC.retinoid","episode.start.retinoid","episode.end.retinoid")]
RAM_switcher_per_indication_all<-unique(RAM_switcher_per_indication_all)

# Count incidence by indication, month, year
switcher_by_indication<-RAM_switcher_per_indication_all[,.N, by = .(year,month, RAM.indication)]

for(group in 1:length(unique(switcher_by_indication$RAM.indication))){
  # Create a subset of age group
  each_group<-switcher_by_indication[RAM.indication==unique(switcher_by_indication$RAM.indication)[group]]
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
  # Fills in missing values with 0
  each_group[is.na(N),N:=0][is.na(RAM.indication),RAM.indication:=unique(switcher_by_indication$RAM.indication)[group]]
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
  saveRDS(indication_switcher_count, (paste0(objective2_strat_dir,"/", pop_prefix,"_RAM_switcher_counts_", unique(switcher_by_indication$RAM.indication)[group],"_indication_group.rds")))
  
}

############################################################################
#################### GENERAL CONCOMITANCE BY INDICATION #################### 
############################################################################
# Data Loading & Cleaning
## RAM Concomitant Data ##
RAM_concomit_data<- as.data.table(readRDS(paste0(objective3_temp_dir, pop_prefix, "_RAM_general_concomit_data.rds")))
RAM_concomit_data<-RAM_concomit_data[,-c("episode.start.retinoid","episode.end.retinoid","ATC.retinoid","birth_date","entry_date","exit_date","current_age","age_group")]
RAM_concomit_data<-unique(RAM_concomit_data)
# Change date format
RAM_concomit_data[,episode.start.RAM:=as.IDate(episode.start.RAM)][,episode.end.RAM:=as.IDate(episode.end.RAM)]

## RAM concomit counts
RAM_concomit_rates<-RAM_concomit_data[,.N, by = .(year,month)]
# Adjust for PHARMO
if(is_PHARMO){RAM_concomit_rates<-RAM_concomit_rates[year < 2020,]} else {RAM_concomit_rates<-RAM_concomit_rates[year < 2021,]}
# Merge with empty df (for counts that do not have counts for all months and years of study)
RAM_concomit_rates <-as.data.table(merge(x = empty_df, y = RAM_concomit_rates, by = c("year", "month"), all.x = TRUE))
# Fills in missing values with 0
RAM_concomit_rates[is.na(RAM_concomit_rates[,N]), N:=0]
# Create YM variable
RAM_concomit_rates<-within(RAM_concomit_rates, YM<- sprintf("%d-%02d", year, month))

# Create Indication subsets based on RAM ATC
RAM_concomit_data_psoriasis<-RAM_concomit_data[ATC.RAM%chin%psoriasis_codes,]
RAM_concomit_data_acne<-RAM_concomit_data[ATC.RAM%chin%acne_codes,]
RAM_concomit_data_dermatitis<-RAM_concomit_data[ATC.RAM%chin%dermatitis_codes,]

# Merge with corresponding Retinoid subsets 
# Psoriasis 
RAM_concomit_data_psoriasis<-retinoid_prevalence_data_psoriasis[,c("person_id","ATC.retinoid","episode.start.retinoid","episode.end.retinoid")][RAM_concomit_data_psoriasis,on=.(person_id)] 
RAM_concomit_data_psoriasis[(episode.start.RAM>=episode.start.retinoid|episode.start.RAM>=episode.end.retinoid|episode.end.RAM>=episode.start.retinoid|episode.end.RAM>=episode.end.retinoid)&ATC.retinoid=="D05BB02",RAM.indication:="psoriasis"]
RAM_concomit_data_psoriasis[is.na(RAM.indication),RAM.indication:="unknown"]
# Acne
RAM_concomit_data_acne<-retinoid_prevalence_data_acne[,c("person_id","ATC.retinoid","episode.start.retinoid","episode.end.retinoid")][RAM_concomit_data_acne,on=.(person_id)] 
RAM_concomit_data_acne[(episode.start.RAM>=episode.start.retinoid|episode.start.RAM>=episode.end.retinoid|episode.end.RAM>=episode.start.retinoid|episode.end.RAM>=episode.end.retinoid)&ATC.retinoid=="D10BA01",RAM.indication:="acne"]
RAM_concomit_data_acne[is.na(RAM.indication),RAM.indication:="unknown"]
# Dermatitis
RAM_concomit_data_dermatitis<-retinoid_prevalence_data_dermatitis[,c("person_id","ATC.retinoid","episode.start.retinoid","episode.end.retinoid")][RAM_concomit_data_dermatitis,on=.(person_id)] 
RAM_concomit_data_dermatitis[(episode.start.RAM>=episode.start.retinoid|episode.start.RAM>=episode.end.retinoid|episode.end.RAM>=episode.start.retinoid|episode.end.RAM>=episode.end.retinoid)&ATC.retinoid=="D11AH04",RAM.indication:="dermatitis"]
RAM_concomit_data_dermatitis[is.na(RAM.indication),RAM.indication:="unknown"]

# Bind the three indications 
RAM_concomit_per_indication_all<-rbindlist(list(RAM_concomit_data_psoriasis,RAM_concomit_data_acne,RAM_concomit_data_dermatitis))
RAM_concomit_per_indication_all<-RAM_concomit_per_indication_all[,-c("ATC.retinoid","episode.start.retinoid","episode.end.retinoid")]
RAM_concomit_per_indication_all<-unique(RAM_concomit_per_indication_all)
# Count incidence by indication, month, year
concomit_by_indication<-RAM_concomit_per_indication_all[,.N, by = .(year,month, RAM.indication)]

for(group in 1:length(unique(concomit_by_indication$RAM.indication))){
  # Create a subset of age group
  each_group<-concomit_by_indication[RAM.indication==unique(concomit_by_indication$RAM.indication)[group]]
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
  # Fills in missing values with 0
  each_group[is.na(N),N:=0][is.na(RAM.indication),RAM.indication:=unique(concomit_by_indication$RAM.indication)[group]]
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
  saveRDS(indication_concomit_count, (paste0(objective3_strat_dir,"/", pop_prefix,"_RAM_general_concomit_counts_", unique(concomit_by_indication$RAM.indication)[group],"_indication_group.rds")))
  
}


############################################################################
#################### CONTRAINDICATED BY INDICATION #########################
############################################################################
# contraindicated 
RAM_contra_data<-RAM_concomit_data[ATC.RAM %in%contraindicated_codes,]

## RAM contra counts
RAM_contra_rates<-RAM_contra_data[,.N, by = .(year,month)]
# Adjust for PHARMO
if(is_PHARMO){RAM_contra_rates<-RAM_contra_rates[year < 2020,]} else {RAM_contra_rates<-RAM_contra_rates[year < 2021,]}
# Merge with empty df (for counts that do not have counts for all months and years of study)
RAM_contra_rates <-as.data.table(merge(x = empty_df, y = RAM_contra_rates, by = c("year", "month"), all.x = TRUE))
# Fills in missing values with 0
RAM_contra_rates[is.na(RAM_contra_rates[,N]), N:=0]
# Create YM variable
RAM_contra_rates<-within(RAM_contra_rates, YM<- sprintf("%d-%02d", year, month))

# Create Indication subsets based on RAM ATC
RAM_contra_data_psoriasis<-RAM_contra_data[ATC.RAM%chin%psoriasis_codes,]
RAM_contra_data_acne<-RAM_contra_data[ATC.RAM%chin%acne_codes,]
RAM_contra_data_dermatitis<-RAM_contra_data[ATC.RAM%chin%dermatitis_codes,]

# Merge with corresponding Retinoid subsets 
# Psoriasis 
RAM_contra_data_psoriasis<-retinoid_prevalence_data_psoriasis[,c("person_id","ATC.retinoid","episode.start.retinoid","episode.end.retinoid")][RAM_contra_data_psoriasis,on=.(person_id)] 
RAM_contra_data_psoriasis[(episode.start.RAM>=episode.start.retinoid|episode.start.RAM>=episode.end.retinoid|episode.end.RAM>=episode.start.retinoid|episode.end.RAM>=episode.end.retinoid)&ATC.retinoid=="D05BB02",RAM.indication:="psoriasis"]
RAM_contra_data_psoriasis[is.na(RAM.indication),RAM.indication:="unknown"]
# Acne
RAM_contra_data_acne<-retinoid_prevalence_data_acne[,c("person_id","ATC.retinoid","episode.start.retinoid","episode.end.retinoid")][RAM_contra_data_acne,on=.(person_id)] 
RAM_contra_data_acne[(episode.start.RAM>=episode.start.retinoid|episode.start.RAM>=episode.end.retinoid|episode.end.RAM>=episode.start.retinoid|episode.end.RAM>=episode.end.retinoid)&ATC.retinoid=="D10BA01",RAM.indication:="acne"]
RAM_contra_data_acne[is.na(RAM.indication),RAM.indication:="unknown"]
# Dermatitis
RAM_contra_data_dermatitis<-retinoid_prevalence_data_dermatitis[,c("person_id","ATC.retinoid","episode.start.retinoid","episode.end.retinoid")][RAM_contra_data_dermatitis,on=.(person_id)] 
RAM_contra_data_dermatitis[(episode.start.RAM>=episode.start.retinoid|episode.start.RAM>=episode.end.retinoid|episode.end.RAM>=episode.start.retinoid|episode.end.RAM>=episode.end.retinoid)&ATC.retinoid=="D11AH04",RAM.indication:="dermatitis"]
RAM_contra_data_dermatitis[is.na(RAM.indication),RAM.indication:="unknown"]

# Bind the three indications 
RAM_contra_per_indication_all<-rbindlist(list(RAM_contra_data_psoriasis,RAM_contra_data_acne,RAM_contra_data_dermatitis))
RAM_contra_per_indication_all<-RAM_contra_per_indication_all[,-c("ATC.retinoid","episode.start.retinoid","episode.end.retinoid")]
RAM_contra_per_indication_all<-unique(RAM_contra_per_indication_all)
# Count incidence by indication, month, year
contra_by_indication<-RAM_contra_per_indication_all[,.N, by = .(year,month, RAM.indication)]

for(group in 1:length(unique(contra_by_indication$RAM.indication))){
  # Create a subset of age group
  each_group<-contra_by_indication[RAM.indication==unique(contra_by_indication$RAM.indication)[group]]
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
  # Fills in missing values with 0
  each_group[is.na(N),N:=0][is.na(RAM.indication),RAM.indication:=unique(contra_by_indication$RAM.indication)[group]]
  # Adjust for PHARMO
  if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
  # Create YM variable
  each_group<-within(each_group,YM<-sprintf("%d-%02d",year,month))
  
  # Prepare denominator
  contra_count_min <- RAM_contra_rates[,c("YM","N")]
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
  saveRDS(indication_contra_count, (paste0(objective3_strat_dir,"/", pop_prefix,"_RAM_general_contra_counts_", unique(contra_by_indication$RAM.indication)[group],"_indication_group.rds")))
  
}


############################################################################
#################### TERATOGENIC BY INDICATION #########################
############################################################################
# teratogenic
RAM_teratogenic_data<-as.data.table(readRDS(paste0(objective4_temp_dir, pop_prefix, "_RAM_meds_teratogenic.rds")))
RAM_teratogenic_data<-RAM_teratogenic_data[,c("person_id","ATC.RAM","Date")]
setnames(RAM_teratogenic_data,"Date","Date.RAM")
RAM_teratogenic_data<-unique(RAM_teratogenic_data)
## RAM teratogenic counts
RAM_teratogenic_counts<-as.data.table(readRDS(paste0(objective4_dir,"/", pop_prefix, "_RAM_teratogenic_per_record.rds")))

# Create Indication subsets based on RAM ATC
RAM_teratogenic_data_psoriasis<-RAM_teratogenic_data[ATC.RAM%chin%psoriasis_codes,]
RAM_teratogenic_data_acne<-RAM_teratogenic_data[ATC.RAM%chin%acne_codes,]
RAM_teratogenic_data_dermatitis<-RAM_teratogenic_data[ATC.RAM%chin%dermatitis_codes,]

# Merge with corresponding Retinoid subsets 
# Psoriasis 
RAM_teratogenic_data_psoriasis<-retinoid_prevalence_data_psoriasis[,c("person_id","ATC.retinoid","episode.start.retinoid","episode.end.retinoid")][RAM_teratogenic_data_psoriasis,on=.(person_id)] 
RAM_teratogenic_data_psoriasis[(Date.RAM>=episode.start.retinoid|Date.RAM>=episode.end.retinoid)&ATC.retinoid=="D05BB02",RAM.indication:="psoriasis"]
RAM_teratogenic_data_psoriasis[is.na(RAM.indication),RAM.indication:="unknown"]
# Acne
RAM_teratogenic_data_acne<-retinoid_prevalence_data_acne[,c("person_id","ATC.retinoid","episode.start.retinoid","episode.end.retinoid")][RAM_teratogenic_data_acne,on=.(person_id)] 
RAM_teratogenic_data_acne[(Date.RAM>=episode.start.retinoid|Date.RAM>=episode.end.retinoid)&ATC.retinoid=="D10BA01",RAM.indication:="acne"]
RAM_teratogenic_data_acne[is.na(RAM.indication),RAM.indication:="unknown"]
# Dermatitis
RAM_teratogenic_data_dermatitis<-retinoid_prevalence_data_dermatitis[,c("person_id","ATC.retinoid","episode.start.retinoid","episode.end.retinoid")][RAM_teratogenic_data_dermatitis,on=.(person_id)] 
RAM_teratogenic_data_dermatitis[(Date.RAM>=episode.start.retinoid|Date.RAM>=episode.end.retinoid)&ATC.retinoid=="D11AH04",RAM.indication:="dermatitis"]
RAM_teratogenic_data_dermatitis[is.na(RAM.indication),RAM.indication:="unknown"]

# Bind the three indications 
RAM_teratogenic_per_indication_all<-rbindlist(list(RAM_teratogenic_data_psoriasis,RAM_teratogenic_data_acne,RAM_teratogenic_data_dermatitis))
RAM_teratogenic_per_indication_all<-RAM_teratogenic_per_indication_all[,-c("ATC.retinoid","episode.start.retinoid","episode.end.retinoid")]
RAM_teratogenic_per_indication_all<-unique(RAM_teratogenic_per_indication_all)
RAM_teratogenic_per_indication_all[,year:= year(Date.RAM)][,month:=month(Date.RAM)]
# Count incidence by indication, month, year
teratogenic_by_indication<-RAM_teratogenic_per_indication_all[,.N, by = .(year,month, RAM.indication)]

for(group in 1:length(unique(teratogenic_by_indication$RAM.indication))){
  # Create a subset of age group
  each_group<-teratogenic_by_indication[RAM.indication==unique(teratogenic_by_indication$RAM.indication)[group]]
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
  # Fills in missing values with 0
  each_group[is.na(N),N:=0][is.na(RAM.indication),RAM.indication:=unique(teratogenic_by_indication$RAM.indication)[group]]
  # Adjust for PHARMO
  if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
  # Create YM variable
  each_group<-within(each_group,YM<-sprintf("%d-%02d",year,month))
  
  # Prepare denominator
  teratogenic_count_min <- RAM_teratogenic_counts[,c("YM","N")]
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
  saveRDS(indication_teratogenic_count, (paste0(objective4_strat_dir,"/", pop_prefix,"_RAM_teratogenic_counts_", unique(teratogenic_by_indication$RAM.indication)[group],"_indication_group.rds")))
  
}





