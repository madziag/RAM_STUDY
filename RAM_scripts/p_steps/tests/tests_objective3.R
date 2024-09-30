###############################################################################
################################# OBJECTIVE 3 #################################
###############################################################################

################################## GENERAL CONCOMITANCE ############################
# Numerator = number of users of RAM + Retinoid per calendar month 
# Denominator = number of retinoid users the same month - retinoid prevalence 
# Stratified by age group and indication 

################################## CONTRAINDICATED #################################
# Numerator = contraindicated RAM codes within general concomitance 
# Denominator = general concomitance 
# Stratified by indication 
# Records vs Users

### TESTS ###
# 1. General concomitance numerator <= retinoid prevalence numerator
# 2. General concomitance numerator <= RAM prevalence numerator
# 3. General concomitance denominator == retinoid prevalence numerator 
# 4. General concomitance numerator users <= General concomitance numerator records 
# 5. Sum of all general concomitance age group numerators == General concomitance age group denominator == RAM General concomitance Numerator 
# 6. Sum of all general concomitance indication numerators == General concomitance indication denominator == RAM General concomitance Numerator 
# 7. Contraindicated user counts < contraindicated record counts
# 8. Contraindicated numerator <= general concomitance numerator (USERS)
# 9. Contraindicated denominator == general concomitance numerator (USERS)
# 10. Contraindicated numerator <= general concomitance numerator (RECORDS)
# 11. Contraindicated denominator == general concomitance numerator (RECORDS)
# 12. Sum of all Contraindicated indication numerators == Contraindicated indication denominator == Contraindicated counts Numerator 

###############################################################################
###############################################################################
###############################################################################
if (exists("genconcomit_all_users") && is.data.table(genconcomit_all_users)){
# 1. General concomitance numerator <= retinoid prevalence numerator
genconcomit_retinoid_prevalence_merged<-merge(genconcomit_all_users[,.(YM,N,Freq)],retinoid_prevalence[,.(YM,N)],by="YM",suffixes=c("_genconcomit","_retinoidprev"))
#check
genconcomit.num_morethan_retinoid.prevalence.num<-nrow(genconcomit_retinoid_prevalence_merged[N_genconcomit>N_retinoidprev,])

# 2. General concomitance numerator <= RAM prevalence numerator
genconcomit_RAM_prevalence_merged<-merge(genconcomit_all_users[,.(YM,N,Freq)],prevalence_all[,.(YM,N)],by="YM",suffixes=c("_genconcomit","_RAMprevalence"))
#check
genconcomit.num_morethan_RAM.prevalence.num<-nrow(genconcomit_RAM_prevalence_merged[N_genconcomit>N_RAMprevalence,])

# 3. General concomitance denominator == retinoid prevalence numerator 
#check
genconcomit.denom_notequal_retinoid.prevalence.num<-nrow(genconcomit_retinoid_prevalence_merged[Freq!=N_retinoidprev,])

# 4. General concomitance numerator users <= General concomitance numerator records 
genconcomit.users_vs_genconcomit.records<-merge(genconcomit_all_users[,.(YM,N)],genconcomit_all_records[,.(YM,N)],by="YM",suffixes=c("_users","_records"))
#check
genconcomit.users_morethan_genconcomit.records<-nrow(genconcomit.users_vs_genconcomit.records[N_users>N_records,])

# 5. Sum of all general concomitance age group numerators == General concomitance age group denominator == RAM General concomitance Numerator 
# combine age groups files into a single table 
datasets <- list(
  if (exists("genconcomit_age_group_2099") && is.data.table(genconcomit_age_group_2099)) genconcomit_age_group_2099[,.(YM,N,Freq)],
  if (exists("genconcomit_age_group_3099") && is.data.table(genconcomit_age_group_3099)) genconcomit_age_group_3099[,.(YM,N,Freq)],
  if (exists("genconcomit_age_group_4099") && is.data.table(genconcomit_age_group_4099)) genconcomit_age_group_4099[,.(YM,N,Freq)],
  if (exists("genconcomit_age_group_5599") && is.data.table(genconcomit_age_group_5599)) genconcomit_age_group_5599[,.(YM,N,Freq)]
)

# Filter out NULL entries (non-existent datasets)
datasets <- Filter(Negate(is.null), datasets)

# Combine the datasets using rbindlist
if (length(datasets) > 0) {
  genconcomit_age_groups_combined <- rbindlist(datasets)
} 

# sum 'N' by 'YM' across age groups
genconcomit_age_group_sums<-genconcomit_age_groups_combined[,.(age_group_N_sum=sum(N,na.rm=TRUE),Freq=max(Freq)), by = YM]

# merge the summed age group data with genconcomit_all by 'YM'
genconcomit_age_group_merged<-merge(genconcomit_all_users[,.(YM,all_N=N)],genconcomit_age_group_sums,by="YM",all.x=TRUE)
#check
genconcomit.agegroup.num.total_vs_genconcomit.num<-nrow(genconcomit_age_group_merged[age_group_N_sum!=all_N,])
genconcomit.agegroup.num.total_vs_genconcomit.agegroup.denom<-nrow(genconcomit_age_group_merged[age_group_N_sum!=Freq,])
genconcomit.num_vs_genconcomit.agegroup.denom<-nrow(genconcomit_age_group_merged[all_N!=Freq,])

# 6. Sum of all general concomitance indication numerators == General concomitance indication denominator == RAM General concomitance Numerator 
# combine indication files into a single table 
datasets <- list(
  if (exists("genconcomit_indication_acne") && is.data.table(genconcomit_indication_acne)) genconcomit_indication_acne[,.(YM,N,Freq)],
  if (exists("genconcomit_indication_derm") && is.data.table(genconcomit_indication_derm)) genconcomit_indication_derm[,.(YM,N,Freq)],
  if (exists("genconcomit_indication_psor") && is.data.table(genconcomit_indication_psor)) genconcomit_indication_psor[,.(YM,N,Freq)]
)

# Filter out NULL entries (non-existent datasets)
datasets <- Filter(Negate(is.null), datasets)

# Combine the datasets using rbindlist
if (length(datasets) > 0) {
  genconcomit_indication_combined <- rbindlist(datasets)
} 

# sum 'N' by 'YM' across age groups
genconcomit_indication_sums<-genconcomit_indication_combined[,.(indication_N_sum=sum(N,na.rm=TRUE),Freq=unique(Freq)),by=YM]
# merge the summed age group data with genconcomit_all by 'YM'
genconcomit_indication_merged<-merge(genconcomit_all_users[,.(YM,all_N=N)],genconcomit_indication_sums,by="YM",all.x=TRUE)
#check
genconcomit.indication.num.total_vs_genconcomit.num<-nrow(genconcomit_indication_merged[indication_N_sum!=all_N,])
genconcomit.indication.num.total_vs_genconcomit.indication.denom<-nrow(genconcomit_indication_merged[indication_N_sum!=Freq,])
genconcomit.num_vs_genconcomit.indication.denom<-nrow(genconcomit_indication_merged[all_N!=Freq,])

# 7. contraindicated user counts <= contraindicated record counts
contra_users_records_merged<-merge(contra_all_records[,.(YM,N,Freq)],contra_all_users[,.(YM,N,Freq)],by="YM",suffixes=c("_records","_users"))
#check
contra.users.num_lessthan_contra.records.num<-nrow(contra_users_records_merged[N_records<N_users,])
contra.users.denom_lessthan_contra.records.denom<-nrow(contra_users_records_merged[Freq_records<Freq_users,])#general concomitance

# 8. Contraindicated numerator <= general concomitance numerator (USERS)
contra_users_genconcomit_users_merged<-merge(contra_all_users[,.(YM,N,Freq)],genconcomit_all_users[,.(YM,N,Freq)],by="YM",suffixes=c("_contra","_genconcomit"))
#check
contra.users.num_morethan_genconcomitusers.num<-nrow(contra_users_genconcomit_users_merged[N_contra>N_genconcomit,])

# 9. Contraindicated denominator == general concomitance numerator (USERS)
#check
contra.users.denom_notequal_genconcomit.users.num<-nrow(contra_users_genconcomit_users_merged[Freq_contra!=N_genconcomit,])

# 10. Contraindicated numerator <= general concomitance numerator (RECORDS)
contra_records_genconcomit_records_merged<-merge(contra_all_records[,.(YM,N,Freq)],genconcomit_all_records[,.(YM,N)],by="YM",suffixes=c("_contra","_genconcomit"))
#check
contra.records.num_morethan_genconcomitrecords.num<-nrow(contra_records_genconcomit_records_merged[N_contra>N_genconcomit,])

# 11. Contraindicated denominator == general concomitance numerator (RECORDS)
#check
contra.records.denom_notequal_genconcomit.records.num<-nrow(contra_records_genconcomit_records_merged[Freq!=N_genconcomit,])

# 12. Sum of all Contraindicated indication numerators == Contraindicated indication denominator == Contraindicated counts Numerator 
# combine indication files into a single table 
datasets <- list(
  if (exists("contra_indication_acne") && is.data.table(contra_indication_acne)) contra_indication_acne[,.(YM,N,Freq)],
  if (exists("contra_indication_derm") && is.data.table(contra_indication_derm)) contra_indication_derm[,.(YM,N,Freq)],
  if (exists("contra_indication_psor") && is.data.table(contra_indication_psor)) contra_indication_psor[,.(YM,N,Freq)]
)

# Filter out NULL entries (non-existent datasets)
datasets <- Filter(Negate(is.null), datasets)

# Combine the datasets using rbindlist
if (length(datasets) > 0) {
  contra_indication_combined <- rbindlist(datasets)
} 

# sum 'N' by 'YM' across indications
contra_indication_sums<-contra_indication_combined[,.(indication_N_sum=sum(N,na.rm=TRUE),Freq=unique(Freq)),by=YM]
# merge the summed indications data with contra_all by 'YM'
contra_indication_merged<-merge(contra_all_users[,.(YM,all_N=N)],contra_indication_sums,by="YM",all.x=TRUE)
#check
contra.indication.num.total_vs_contra.num<-nrow(contra_indication_merged[indication_N_sum!=all_N,])
contra.indication.num.total_vs_contra.indication.denom<-nrow(contra_indication_merged[indication_N_sum!=Freq,])
contra.num_vs_contra.indication.denom<-nrow(contra_indication_merged[all_N!=Freq,])

### FLOWCHART ###

names<-c(
  # 1. General concomitance numerator <= retinoid prevalence numerator
  "genconcomit.num_morethan_retinoid.prevalence.num",
  # 2. General concomitance numerator <= RAM prevalence numerator
  "genconcomit.num_morethan_RAM.prevalence.num",
  # 3. General concomitance denominator == retinoid prevalence numerator 
  "genconcomit.denom_notequal_retinoid.prevalence.num",
  # 4. General concomitance numerator users <= General concomitance numerator records 
  "genconcomit.users_morethan_genconcomit.records",
  # 5. Sum of all general concomitance age group numerators == General concomitance age group denominator == RAM General concomitance Numerator 
  "genconcomit.agegroup.num.total_vs_genconcomit.num",
  "genconcomit.agegroup.num.total_vs_genconcomit.agegroup.denom",
  "genconcomit.num_vs_genconcomit.agegroup.denom",
  # 6. Sum of all general concomitance indication numerators == General concomitance indication denominator == RAM General concomitance Numerator 
  "genconcomit.indication.num.total_vs_genconcomit.num",
  "genconcomit.indication.num.total_vs_genconcomit.indication.denom",
  "genconcomit.num_vs_genconcomit.indication.denom",
  # 7. Contraindicated user counts < contraindicated record counts
  "contra.users.num_lessthan_contra.records.num",
  "contra.users.denom_lessthan_contra.records.denom",
  # 8. Contraindicated numerator <= general concomitance numerator (USERS)
  "contra.users.num_morethan_genconcomitusers.num",
  # 9. Contraindicated denominator == general concomitance numerator (USERS)
  "contra.users.denom_notequal_genconcomit.users.num",
  # 10. Contraindicated numerator <= general concomitance numerator (RECORDS)
  "contra_records_genconcomit_records_merged",
  # 11. Contraindicated denominator == general concomitance numerator (RECORDS)
  "contra.records.denom_notequal_genconcomit.records.num",
  # 12. Sum of all Contraindicated indication numerators == Contraindicated indication denominator == Contraindicated counts Numerator 
  "contra.indication.num.total_vs_contra.num",
  "contra.indication.num.total_vs_contra.indication.denom",
  "contra.num_vs_contra.indication.denom"
)

values<-c(
  # 1. General concomitance numerator <= retinoid prevalence numerator
  genconcomit.num_morethan_retinoid.prevalence.num,
  # 2. General concomitance numerator <= RAM prevalence numerator
  genconcomit.num_morethan_RAM.prevalence.num,
  # 3. General concomitance denominator == retinoid prevalence numerator 
  genconcomit.denom_notequal_retinoid.prevalence.num,
  # 4. General concomitance numerator users <= General concomitance numerator records 
  genconcomit.users_morethan_genconcomit.records,
  # 5. Sum of all general concomitance age group numerators == General concomitance age group denominator == RAM General concomitance Numerator 
  genconcomit.agegroup.num.total_vs_genconcomit.num,
  genconcomit.agegroup.num.total_vs_genconcomit.agegroup.denom,
  genconcomit.num_vs_genconcomit.agegroup.denom,
  # 6. Sum of all general concomitance indication numerators == General concomitance indication denominator == RAM General concomitance Numerator 
  genconcomit.indication.num.total_vs_genconcomit.num,
  genconcomit.indication.num.total_vs_genconcomit.indication.denom,
  genconcomit.num_vs_genconcomit.indication.denom,
  # 7. Contraindicated user counts < contraindicated record counts
  contra.users.num_lessthan_contra.records.num,
  contra.users.denom_lessthan_contra.records.denom,
  # 8. Contraindicated numerator <= general concomitance numerator (USERS)
  contra.users.num_morethan_genconcomitusers.num,
  # 9. Contraindicated denominator == general concomitance numerator (USERS)
  contra.users.denom_notequal_genconcomit.users.num,
  # 10. Contraindicated numerator <= general concomitance numerator (RECORDS)
  contra.records.num_morethan_genconcomitrecords.num,
  # 11. Contraindicated denominator == general concomitance numerator (RECORDS)
  contra.records.denom_notequal_genconcomit.records.num,
  # 12. Sum of all Contraindicated indication numerators == Contraindicated indication denominator == Contraindicated counts Numerator 
  contra.indication.num.total_vs_contra.num,
  contra.indication.num.total_vs_contra.indication.denom,
  contra.num_vs_contra.indication.denom
)

flowchart_objective3<-data.table(names, values)
View(flowchart_objective3)
# saveRDS(flowchart_objective3, paste0(g_output_dir,"/medicines_counts/flowchart_objective3.rds")) 

} else {
  print("There is no General Concomitance Data")
}