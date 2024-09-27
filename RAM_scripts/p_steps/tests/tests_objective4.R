###############################################################################
################################# OBJECTIVE 4 #################################
###############################################################################

################################## TERATOGENIC USERS ##################################
# Numerator   = Number of teratogenic ATC users per month-year 
# Denominator = RAM prevalence 
# Stratified by indication 

################################## TERATOGENIC RECORDS  #################################
# Numerator   = Number of teratogenic ATC records per month-year 
# Denominator = All RAM prescriptions in the retinoid subpopulation

### TESTS ###
# 1. Teratogenic user denominator == RAM prevalence numerator 
# 2. Sum of all teratogenic indication numerators == Teratogenic indication denominator == RAM Teratogenic Numerator 
# 3. Teratogenic user numerator <= Teratogenic record numerator 

###############################################################################
###############################################################################
###############################################################################
if (exists("teratogenic_all_users") && is.data.table(teratogenic_all_users)){
# 1. Teratogenic user denominator == RAM prevalence numerator 
teratogenic_RAM_prevalence_merged<-merge(teratogenic_all_users[,.(YM,Freq)],prevalence_all[,.(YM,N)])
#check
teratogenic.user.denom_notequal_RAM_prevalence.num<-nrow(teratogenic_RAM_prevalence_merged[N!=Freq,])

# 2. Sum of all teratogenic indication numerators == Teratogenic indication denominator == RAM Teratogenic Numerator 
# combine indication files into a single table 
datasets <- list(
  if (exists("teratogenic_indication_acne") && is.data.table(teratogenic_indication_acne)) teratogenic_indication_acne[,.(YM,N,Freq)],
  if (exists("teratogenic_indication_derm") && is.data.table(teratogenic_indication_derm)) teratogenic_indication_derm[,.(YM,N,Freq)],
  if (exists("teratogenic_indication_psor") && is.data.table(teratogenic_indication_psor)) teratogenic_indication_psor[,.(YM,N,Freq)]
)

# Filter out NULL entries (non-existent datasets)
datasets <- Filter(Negate(is.null), datasets)

# Combine the datasets using rbindlist
if (length(datasets) > 0) {
  teratogenic_indication_combined <- rbindlist(datasets)
} 

# sum 'N' by 'YM' across indications
teratogenic_indication_sums<-teratogenic_indication_combined[,.(indication_N_sum=sum(N,na.rm=TRUE),Freq=unique(Freq)),by=YM]
# merge the summed age group data with teratogenic_all by 'YM'
teratogenic_indication_merged<-merge(teratogenic_all_users[,.(YM,all_N=N)],teratogenic_indication_sums,by="YM",all.x=TRUE)
#check
teratogenic.indication.num.total_vs_teratogenic.num<-nrow(teratogenic_indication_merged[indication_N_sum!=all_N,])
teratogenic.indication.num.total_vs_teratogenic.indication.denom<-nrow(teratogenic_indication_merged[indication_N_sum!=Freq,])
teratogenic.num_vs_teratogenic.indication.denom<-nrow(teratogenic_indication_merged[all_N!=Freq,])

# 3. Teratogenic user numerator <= Teratogenic record numerator 
teratogenic_users_records_merged<-merge(teratogenic_all_records[,.(YM,N,Freq)],teratogenic_all_users[,.(YM,N,Freq)],by="YM",suffixes=c("_records","_users"))
#check
teratogenic.users.num_lessthan_teratogenic.records.num<-nrow(teratogenic_users_records_merged[N_records<N_users,])

### FLOWCHART ###
names<-c(
  # 1. Teratogenic user denominator == RAM prevalence numerator 
  "teratogenic.user.denom_notequal_RAM_prevalence.num",
  # 2. Sum of all teratogenic indication numerators == Teratogenic indication denominator == RAM Teratogenic Numerator 
  "teratogenic.indication.num.total_vs_teratogenic.num",
  "teratogenic.indication.num.total_vs_teratogenic.indication.denom",
  "teratogenic.num_vs_teratogenic.indication.denom",
  # 3. Teratogenic user numerator <= Teratogenic record numerator 
  "teratogenic.users.num_lessthan_teratogenic.records.num"
)

values<-c(
  # 1. Teratogenic user denominator == RAM prevalence numerator 
  teratogenic.user.denom_notequal_RAM_prevalence.num,
  # 2. Sum of all teratogenic indication numerators == Teratogenic indication denominator == RAM Teratogenic Numerator 
  teratogenic.indication.num.total_vs_teratogenic.num,
  teratogenic.indication.num.total_vs_teratogenic.indication.denom,
  teratogenic.num_vs_teratogenic.indication.denom,
  # 3. Teratogenic user numerator <= Teratogenic record numerator 
  teratogenic.users.num_lessthan_teratogenic.records.num
)

flowchart_objective4<-data.table(names, values)
View(flowchart_objective4)
# saveRDS(flowchart_objective4, paste0(g_output_dir,"/medicines_counts/flowchart_objective1.rds")) 
} else {
  print("There is no teratogenic data")
}