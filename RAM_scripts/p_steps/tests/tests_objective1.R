###############################################################################
################################# OBJECTIVE 1 #################################
###############################################################################

################################## INCIDENCE ##################################
# Numerator   = number of new RAM users (no use the year prior) per calendar month 
# Denominator = subset of WOCBP with at least one retinoid prescription within study period 
# Stratified by age group and indication 

################################## PREVALENCE #################################
# Numerator   = number of RAM users per calendar month with at least one day of exposure that month 
# Denominator = subset of WOCBP with at least one retinoid prescription within study period 
# Stratified by age group and indication 

### TESTS ###
# 1. Incidence Numerator <= Prevalence Denominator 
# 2. Incidence Denominator == Prevalence Denominator 
# 3. Sum of all incidence age group numerators == Incidence age group denominator == RAM Incidence Numerator 
# 4. Sum of all incidence indication numerators == Incidence indication denominator == RAM Incidence Numerator 
# 5. Sum of all prevalence age group numerators == Prevalence age group denominator == RAM Prevalence Numerator 
# 6. Sum of all prevalence indication numerators == Prevalence indication denominator == RAM Prevalence Numerator 

###############################################################################
###############################################################################
###############################################################################

# FOR PC/PC_HOSP
# 1. Incidence Numerator <= Prevalence Denominator 
incidence_prevalence_merged<-merge(incidence_all[,.(YM,N,Freq)],prevalence_all[,.(YM,N,Freq)],by="YM",suffixes=c("_incidence","_prevalence"))
#check
incidence.num_morethan_prevalence.num<-nrow(incidence_prevalence_merged[N_incidence>N_prevalence,])

# 2. Incidence Denominator == Prevalence Denominator 
#check
incidence.denom_notequal_prevalence.denom<-nrow(incidence_prevalence_merged[Freq_incidence!=Freq_prevalence,])

# 3. Sum of all incidence age group numerators == Incidence age group denominator == RAM Incidence Numerator 
# combine age group data into a single table 

# Create a list of the datasets
datasets <- list(
  if (exists("incidence_age_group_2099") && is.data.table(incidence_age_group_2099)) incidence_age_group_2099[,.(YM,N,Freq)],
  if (exists("incidence_age_group_3099") && is.data.table(incidence_age_group_3099)) incidence_age_group_3099[,.(YM,N,Freq)],
  if (exists("incidence_age_group_4099") && is.data.table(incidence_age_group_4099)) incidence_age_group_4099[,.(YM,N,Freq)],
  if (exists("incidence_age_group_5599") && is.data.table(incidence_age_group_5599)) incidence_age_group_5599[,.(YM,N,Freq)]
)

# Filter out NULL entries (non-existent datasets)
datasets <- Filter(Negate(is.null), datasets)

# Combine the datasets using rbindlist
if (length(datasets) > 0) {
  incidence_age_groups_combined <- rbindlist(datasets)
} 

# sum 'N' by 'YM' across age groups
incidence_age_group_sums<-incidence_age_groups_combined[,.(age_group_N_sum=sum(N,na.rm=TRUE),Freq=unique(Freq)), by = YM]
# merge the summed age group data with incidence_all by 'YM'
incidence_age_group_merged<-merge(incidence_all[,.(YM,all_N=N)],incidence_age_group_sums,by="YM",all.x=TRUE)
#check
incidence.agegroup.num.total_vs_incidence.num<-nrow(incidence_age_group_merged[age_group_N_sum!=all_N,])
incidence.agegroup.num.total_vs_incidence.agegroup.denom<-nrow(incidence_age_group_merged[age_group_N_sum!=Freq,])
incidence.num_vs_incidence.agegroup.denom<-nrow(incidence_age_group_merged[all_N!=Freq,])

# 4. Sum of all incidence indication numerators == Incidence indication denominator == RAM Incidence Numerator 
# combine indication data into a single table
datasets <- list(
  if (exists("incidence_indication_acne") && is.data.table(incidence_indication_acne)) incidence_indication_acne[,.(YM,N,Freq)],
  if (exists("incidence_indication_derm") && is.data.table(incidence_indication_derm)) incidence_indication_derm[,.(YM,N,Freq)],
  if (exists("incidence_indication_psor") && is.data.table(incidence_indication_psor)) incidence_indication_psor[,.(YM,N,Freq)]
  )

# Filter out NULL entries (non-existent datasets)
datasets <- Filter(Negate(is.null), datasets)

# Combine the datasets using rbindlist
if (length(datasets) > 0) {
  incidence_indication_combined <- rbindlist(datasets)
} 

# sum 'N' by 'YM' across indications
incidence_indication_sums<-incidence_indication_combined[,.(indication_N_sum=sum(N,na.rm=TRUE),Freq=unique(Freq)),by=YM]
# merge the summed indication data with incidence_all by 'YM'
incidence_indication_merged<-merge(incidence_all[,.(YM,all_N=N)],incidence_indication_sums,by="YM",all.x=TRUE)
#check
incidence.indication.num.total_vs_incidence.num<-nrow(incidence_indication_merged[indication_N_sum!=all_N,])
incidence.indication.num.total_vs_incidence.indication.denom<-nrow(incidence_indication_merged[indication_N_sum!=Freq,])
incidence.num_vs_incidence.indication.denom<-nrow(incidence_indication_merged[all_N!=Freq,])

# 5. Sum of all prevalence age group numerators == Prevalence age group denominator == RAM Prevalence Numerator 
# Combine age groups data into a single table 
datasets <- list(
  if (exists("prevalence_age_group_2099") && is.data.table(prevalence_age_group_2099)) prevalence_age_group_2099[,.(YM,N,Freq)],
  if (exists("prevalence_age_group_3099") && is.data.table(prevalence_age_group_3099)) prevalence_age_group_3099[,.(YM,N,Freq)],
  if (exists("prevalence_age_group_4099") && is.data.table(prevalence_age_group_4099)) prevalence_age_group_4099[,.(YM,N,Freq)],
  if (exists("prevalence_age_group_5599") && is.data.table(prevalence_age_group_5599)) prevalence_age_group_5599[,.(YM,N,Freq)]
)

# Filter out NULL entries (non-existent datasets)
datasets <- Filter(Negate(is.null), datasets)

# Combine the datasets using rbindlist
if (length(datasets) > 0) {
  prevalence_age_groups_combined <- rbindlist(datasets)
} 

# sum 'N' by 'YM' across age groups
prevalence_age_group_sums<-prevalence_age_groups_combined[,.(age_group_N_sum=sum(N,na.rm=TRUE),Freq=unique(Freq)), by = YM]
# merge the summed age group data with prevalence_all by 'YM'
prevalence_age_group_merged<-merge(prevalence_all[,.(YM,all_N=N)],prevalence_age_group_sums,by="YM",all.x=TRUE)
#check
prevalence.agegroup.num.total_notequal_prevalence.num<-nrow(prevalence_age_group_merged[age_group_N_sum!=all_N,])
prevalence.agegroup.num.total_notequal_prevalence.agegroup.denom<-nrow(prevalence_age_group_merged[age_group_N_sum!=Freq,])
prevalence.num_notequal_prevalence.agegroup.denom<-nrow(prevalence_age_group_merged[all_N!=Freq,])

# 6. Sum of all prevalence indication numerators == Prevalence indication denominator == RAM Prevalence Numerator 
# combine indication  data into a single table 
datasets <- list(
  if (exists("prevalence_indication_acne") && is.data.table(prevalence_indication_acne)) prevalence_indication_acne[,.(YM,N,Freq)],
  if (exists("prevalence_indication_derm") && is.data.table(prevalence_indication_derm)) prevalence_indication_derm[,.(YM,N,Freq)],
  if (exists("prevalence_indication_psor") && is.data.table(prevalence_indication_psor)) prevalence_indication_psor[,.(YM,N,Freq)]
)

# Filter out NULL entries (non-existent datasets)
datasets <- Filter(Negate(is.null), datasets)

# Combine the datasets using rbindlist
if (length(datasets) > 0) {
  prevalence_indication_combined <- rbindlist(datasets)
} 

# sum 'N' by 'YM' across indications
prevalence_indication_sums<-prevalence_indication_combined[,.(indication_N_sum=sum(N,na.rm=TRUE),Freq=unique(Freq)),by=YM]
# merge the summed indication data with prevalence_all by 'YM'
prevalence_indication_merged<-merge(prevalence_all[,.(YM,all_N=N)],prevalence_indication_sums,by="YM",all.x=TRUE)
#check
prevalence.indication.num.total_notequal_prevalence.num<-nrow(prevalence_indication_merged[indication_N_sum!=all_N,])
prevalence.indication.num.total_notequal_prevalence.indication.denom<-nrow(prevalence_indication_merged[indication_N_sum!=Freq,])
prevalence.num_notequal_prevalence.indication.denom<-nrow(prevalence_indication_merged[all_N!=Freq,])

### FLOWCHART ###
names<-c(
  # 1. Incidence Numerator <= Prevalence Denominator 
  "incidence.num_morethan_prevalence.num",
  # 2. Incidence Denominator == Prevalence Denominator 
  "incidence.denom_notequal_prevalence.denom",
  # 3. Sum of all incidence age group numerators == Incidence age group denominator == RAM Incidence Numerator 
  "incidence.agegroup.num.total_vs_incidence.num",
  "incidence.agegroup.num.total_vs_incidence.agegroup.denom",
  "incidence.num_vs_incidence.agegroup.denom",
  # 4. Sum of all incidence indication numerators == Incidence indication denominator == RAM Incidence Numerator 
  "incidence.indication.num.total_vs_incidence.num",
  "incidence.indication.num.total_vs_incidence.indication.denom",
  "incidence.num_vs_incidence.indication.denom",
  # 5. Sum of all prevalence age group numerators == Prevalence age group denominator == RAM Prevalence Numerator 
  "prevalence.agegroup.num.total_notequal_prevalence.num",
  "prevalence.agegroup.num.total_notequal_prevalence.agegroup.denom",
  "prevalence.num_notequal_prevalence.agegroup.denom",
  # 6. Sum of all prevalence indication numerators == Prevalence indication denominator == RAM Prevalence Numerator 
  "prevalence.indication.num.total_notequal_prevalence.num",
  "prevalence.indication.num.total_notequal_prevalence.indication.denom",
  "prevalence.num_notequal_prevalence.indication.denom"
)

values<-c(
  # 1. Incidence Numerator <= Prevalence Denominator 
  incidence.num_morethan_prevalence.num,
  # 2. Incidence Denominator == Prevalence Denominator 
  incidence.denom_notequal_prevalence.denom,
  # 3. Sum of all incidence age group numerators == Incidence age group denominator == RAM Incidence Numerator 
  incidence.agegroup.num.total_vs_incidence.num,
  incidence.agegroup.num.total_vs_incidence.agegroup.denom,
  incidence.num_vs_incidence.agegroup.denom,
  # 4. Sum of all incidence indication numerators == Incidence indication denominator == RAM Incidence Numerator 
  incidence.indication.num.total_vs_incidence.num,
  incidence.indication.num.total_vs_incidence.indication.denom,
  incidence.num_vs_incidence.indication.denom,
  # 5. Sum of all prevalence age group numerators == Prevalence age group denominator == RAM Prevalence Numerator 
  prevalence.agegroup.num.total_notequal_prevalence.num,
  prevalence.agegroup.num.total_notequal_prevalence.agegroup.denom,
  prevalence.num_notequal_prevalence.agegroup.denom,
  # 6. Sum of all prevalence indication numerators == Prevalence indication denominator == RAM Prevalence Numerator 
  prevalence.indication.num.total_notequal_prevalence.num,
  prevalence.indication.num.total_notequal_prevalence.indication.denom,
  prevalence.num_notequal_prevalence.indication.denom
)

flowchart_objective1<-data.table(names, values)
View(flowchart_objective1)
# saveRDS(flowchart_objective1, paste0(g_output_dir,"/medicines_counts/flowchart_objective1.rds")) 

