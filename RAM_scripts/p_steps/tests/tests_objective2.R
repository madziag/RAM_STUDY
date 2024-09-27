###############################################################################
################################# OBJECTIVE 2 #################################
###############################################################################

################################## DISCONTINUED ##################################
# Numerator   = number of subjects discontinuing RAM per calendar month
# Denominator = number of all RAM users during the same month - RAM prevalence 
# Stratified by age group and indication 

################################## SWITCHED  #################################
# Numerator = number of RAM incident users who switched from an oral retinoid 
# Denominator1 = number of all RAM users during the same month - retinoid prevalence 
# Denominator2 = number of WOCBP who ever discontinued a retinoid the actual month - retinoid discontinued
# Stratified by age group and indication 

### TESTS ###
# 1. Discontinued denominator == RAM prevalence numerator
# 2. Sum of all discontinued age group numerators == Discontinued age group denominator == RAM discontinued Numerator 
# 3. Sum of all discontinued indication numerators == Discontinued indication denominator == RAM discontinued Numerator 
# 4. Switcher 1 numerator == Switcher 2 numerator 
# 5. Switcher 1 denominator == retinoid prevalence numerator
# 6. Switcher 2 denominator == retinoid discontinued numerator
# 7. Sum of all switcher age group numerators == Switcher age group denominator == RAM switcher Numerator 
# 8. Sum of all switcher indication numerators == Switcher indication denominator == RAM switchter Numerator 

###############################################################################
###############################################################################
###############################################################################
# 1. Discontinued denominator == RAM prevalence numerator
discontinued_prevalence_merged<-merge(discontinued_all[,.(YM,N,Freq)],prevalence_all[,.(YM,N,Freq)],by="YM",suffixes=c("_discontinued","_prevalence"))
#check
discontinued.denom_notequal_prevalence.num<-nrow(discontinued_prevalence_merged[Freq_discontinued!=N_prevalence,])

# 2. Sum of all discontinued age group numerators == Discontinued age group denominator == RAM discontinued Numerator 
# combine age groups files into a single table 
datasets <- list(
  if (exists("discontinued_age_group_2099") && is.data.table(discontinued_age_group_2099)) discontinued_age_group_2099[,.(YM,N,Freq)],
  if (exists("discontinued_age_group_3099") && is.data.table(discontinued_age_group_3099)) discontinued_age_group_3099[,.(YM,N,Freq)],
  if (exists("discontinued_age_group_4099") && is.data.table(discontinued_age_group_4099)) discontinued_age_group_4099[,.(YM,N,Freq)],
  if (exists("discontinued_age_group_5599") && is.data.table(discontinued_age_group_5599)) discontinued_age_group_5599[,.(YM,N,Freq)]
)

# Filter out NULL entries (non-existent datasets)
datasets <- Filter(Negate(is.null), datasets)

# Combine the datasets using rbindlist
if (length(datasets) > 0) {
  discontinued_age_groups_combined <- rbindlist(datasets)
} 

# sum 'N' by 'YM' across age groups
discontinued_age_group_sums<-discontinued_age_groups_combined[,.(age_group_N_sum=sum(N,na.rm=TRUE),Freq=unique(Freq)), by = YM]
# merge the summed age group data with discontinued_all by 'YM'
discontinued_age_group_merged<-merge(discontinued_all[,.(YM,all_N=N)],discontinued_age_group_sums,by="YM",all.x=TRUE)
#check
discontinued.agegroup.num.total_vs_discontinued.num<-nrow(discontinued_age_group_merged[age_group_N_sum!=all_N,])
discontinued.agegroup.num.total_vs_discontinued.agegroup.denom<-nrow(discontinued_age_group_merged[age_group_N_sum!=Freq,])
discontinued.num_vs_discontinued.agegroup.denom<-nrow(discontinued_age_group_merged[all_N!=Freq,])

# 3. Sum of all discontinued indication numerators == Discontinued indication denominator == RAM discontinued Numerator 
# combine indication files into a single table 
datasets <- list(
  if (exists("discontinued_indication_acne") && is.data.table(discontinued_indication_acne)) discontinued_indication_acne[,.(YM,N,Freq)],
  if (exists("discontinued_indication_derm") && is.data.table(discontinued_indication_derm)) discontinued_indication_derm[,.(YM,N,Freq)],
  if (exists("discontinued_indication_psor") && is.data.table(discontinued_indication_psor)) discontinued_indication_psor[,.(YM,N,Freq)]
)

# Filter out NULL entries (non-existent datasets)
datasets <- Filter(Negate(is.null), datasets)

# Combine the datasets using rbindlist
if (length(datasets) > 0) {
  discontinued_indication_combined <- rbindlist(datasets)
} 

# sum 'N' by 'YM' across indications
discontinued_indication_sums<-discontinued_indication_combined[,.(indication_N_sum=sum(N,na.rm=TRUE),Freq=unique(Freq)),by=YM]
# merge the summed indication data with discontinued_all by 'YM'
discontinued_indication_merged<-merge(discontinued_all[,.(YM,all_N=N)],discontinued_indication_sums,by="YM",all.x=TRUE)
#check
discontinued.indication.num.total_vs_discontinued.num<-nrow(discontinued_indication_merged[indication_N_sum!=all_N,])
discontinued.indication.num.total_vs_discontinued.indication.denom<-nrow(discontinued_indication_merged[indication_N_sum!=Freq,])
discontinued.num_vs_discontinued.indication.denom<-nrow(discontinued_indication_merged[all_N!=Freq,])

# 4. Switcher 1 numerator == Switcher 2 numerator 
switcher1_switcher2_merged<-merge(switcher_1[,.(YM,N,Freq)],switcher_2[,.(YM,N,Freq)],by="YM",suffixes=c("_switcher1","_switcher2"))
#check
switcher1.num_notequal_switcher2.num<-nrow(switcher1_switcher2_merged[N_switcher1!=N_switcher2,])

# 5. Switcher 1 denominator == retinoid prevalence numerator
switcher1_retinoid_prevalent_merged<-merge(switcher_1[,.(YM,Freq)],retinoid_prevalence[,.(YM,N)],by="YM")
#check
switcher1.denom_notequal_retinoid.prevalence.num<-nrow(switcher1_retinoid_prevalent_merged[Freq!=N,])

# 6. Switcher 2 denominator == retinoid discontinued numerator
switcher2_retinoid_discontined_merged<-merge(switcher_2[,.(YM,Freq)],retinoid_discontinued[,.(YM,N)],by="YM")
#check
switcher2.denom_notequal_retinoid.discontinued.num<-nrow(switcher1_retinoid_prevalent_merged[Freq!=N,])

# 7. Sum of all switcher age group numerators == Switcher age group denominator == RAM switcher Numerator 
# combine age groups files into a single table 
datasets <- list(
  if (exists("switcher_age_group_2099") && is.data.table(switcher_age_group_2099)) switcher_age_group_2099[,.(YM,N,Freq)],
  if (exists("switcher_age_group_3099") && is.data.table(switcher_age_group_3099)) switcher_age_group_3099[,.(YM,N,Freq)],
  if (exists("switcher_age_group_4099") && is.data.table(switcher_age_group_4099)) switcher_age_group_4099[,.(YM,N,Freq)],
  if (exists("switcher_age_group_5599") && is.data.table(switcher_age_group_5599)) switcher_age_group_5599[,.(YM,N,Freq)]
)

# Filter out NULL entries (non-existent datasets)
datasets <- Filter(Negate(is.null), datasets)

# Combine the datasets using rbindlist
if (length(datasets) > 0) {
  switcher_age_groups_combined <- rbindlist(datasets)
} 
# sum 'N' by 'YM' across age groups
switcher_age_group_sums<-switcher_age_groups_combined[,.(age_group_N_sum=sum(N,na.rm=TRUE),Freq=unique(Freq)), by = YM]
# merge the summed age group data with switcher_all by 'YM'
switcher1_age_group_merged<-merge(switcher_1[,.(YM,all_N=N)],switcher_age_group_sums,by="YM",all.x=TRUE)
switcher2_age_group_merged<-merge(switcher_2[,.(YM,all_N=N)],switcher_age_group_sums,by="YM",all.x=TRUE)
#check
switcher1.agegroup.num.total_vs_switcher1.num<-nrow(switcher1_age_group_merged[age_group_N_sum!=all_N,])
switcher1.agegroup.num.total_vs_switcher1.agegroup.denom<-nrow(switcher1_age_group_merged[age_group_N_sum!=Freq,])
switcher1.num_vs_switcher1.agegroup.denom<-nrow(switcher1_age_group_merged[all_N!=Freq,])
switcher2.agegroup.num.total_vs_switcher2.num<-nrow(switcher2_age_group_merged[age_group_N_sum!=all_N,])
switcher2.agegroup.num.total_vs_switcher2.agegroup.denom<-nrow(switcher2_age_group_merged[age_group_N_sum!=Freq,])
switcher2.num_vs_switcher2.agegroup.denom<-nrow(switcher2_age_group_merged[all_N!=Freq,])

# 8. Sum of all switcher indication numerators == Switcher indication denominator == RAM switchter Numerator 
# combine indication files into a single table and sum 'N' by 'YM'
datasets <- list(
  if (exists("switcher_indication_acne") && is.data.table(switcher_indication_acne)) switcher_indication_acne[,.(YM,N,Freq)],
  if (exists("switcher_indication_derm") && is.data.table(switcher_indication_derm)) switcher_indication_derm[,.(YM,N,Freq)],
  if (exists("switcher_indication_psor") && is.data.table(switcher_indication_psor)) switcher_indication_psor[,.(YM,N,Freq)]
)

# Filter out NULL entries (non-existent datasets)
datasets <- Filter(Negate(is.null), datasets)

# Combine the datasets using rbindlist
if (length(datasets) > 0) {
  switcher_indication_combined <- rbindlist(datasets)
}
# sum 'N' by 'YM' across age groups
switcher_indication_sums<-switcher_indication_combined[,.(indication_N_sum=sum(N,na.rm=TRUE),Freq=unique(Freq)),by=YM]
# merge the summed age group data with switcher_all by 'YM'
switcher1_indication_merged<-merge(switcher_1[,.(YM,all_N=N)],switcher_indication_sums,by="YM",all.x=TRUE)
switcher2_indication_merged<-merge(switcher_2[,.(YM,all_N=N)],switcher_indication_sums,by="YM",all.x=TRUE)
#check
switcher1.indication.num.total_vs_switcher1.num<-nrow(switcher1_indication_merged[indication_N_sum!=all_N,])
switcher1.indication.num.total_vs_switcher1.indication.denom<-nrow(switcher1_indication_merged[indication_N_sum!=Freq,])
switcher1.num_vs_switcher1.indication.denom<-nrow(switcher1_indication_merged[all_N!=Freq,])
switcher2.indication.num.total_vs_switcher2.num<-nrow(switcher2_indication_merged[indication_N_sum!=all_N,])
switcher2.indication.num.total_vs_switcher2.indication.denom<-nrow(switcher2_indication_merged[indication_N_sum!=Freq,])
switcher2.num_vs_switcher2.indication.denom<-nrow(switcher2_indication_merged[all_N!=Freq,])

### FLOWCHART ###
names<-c(
  # 1. Discontinued denominator == RAM prevalence numerator
  "discontinued.denom_notequal_prevalence.num",
  # 2. Sum of all discontinued age group numerators == Discontinued age group denominator == RAM discontinued Numerator 
  "discontinued.agegroup.num.total_vs_discontinued.num",
  "discontinued.agegroup.num.total_vs_discontinued.agegroup.denom",
  "discontinued.num_vs_discontinued.agegroup.denom",
  # 3. Sum of all discontinued indication numerators == Discontinued indication denominator == RAM discontinued Numerator 
  "discontinued.indication.num.total_vs_discontinued.num",
  "discontinued.indication.num.total_vs_discontinued.indication.denom",
  "discontinued.num_vs_discontinued.indication.denom",
  # 4. Switcher 1 numerator == Switcher 2 numerator 
  "switcher1.num_notequal_switcher2.num",
  # 5. Switcher 1 denominator == retinoid prevalence numerator
  "switcher1.denom_notequal_retinoid.prevalence.num",
  # 6. Switcher 2 denominator == retinoid discontinued numerator
  "switcher2.denom_notequal_retinoid.discontinued.num",
  # 7. Sum of all switcher age group numerators == Switcher age group denominator == RAM switcher Numerator 
  "switcher1.agegroup.num.total_vs_switcher1.num",
  "switcher1.agegroup.num.total_vs_switcher1.agegroup.denom",
  "switcher1.num_vs_switcher1.agegroup.denom",
  "switcher2.agegroup.num.total_vs_switcher2.num",
  "switcher2.agegroup.num.total_vs_switcher2.agegroup.denom",
  "switcher2.num_vs_switcher2.agegroup.denom",
  # 8. Sum of all switcher indication numerators == Switcher indication denominator == RAM switchter Numerator 
  "switcher1.indication.num.total_vs_switcher1.num",
  "switcher1.indication.num.total_vs_switcher1.indication.denom",
  "switcher1.num_vs_switcher1.indication.denom",
  "switcher2.indication.num.total_vs_switcher2.num",
  "switcher2.indication.num.total_vs_switcher2.indication.denom",
  "switcher2.num_vs_switcher2.indication.denom"
)

values<-c(
  # 1. Discontinued denominator == RAM prevalence numerator
  discontinued.denom_notequal_prevalence.num,
  # 2. Sum of all discontinued age group numerators == Discontinued age group denominator == RAM discontinued Numerator 
  discontinued.agegroup.num.total_vs_discontinued.num,
  discontinued.agegroup.num.total_vs_discontinued.agegroup.denom,
  discontinued.num_vs_discontinued.agegroup.denom,
  # 3. Sum of all discontinued indication numerators == Discontinued indication denominator == RAM discontinued Numerator 
  discontinued.indication.num.total_vs_discontinued.num,
  discontinued.indication.num.total_vs_discontinued.indication.denom,
  discontinued.num_vs_discontinued.indication.denom,
  # 4. Switcher 1 numerator == Switcher 2 numerator 
  switcher1.num_notequal_switcher2.num,
  # 5. Switcher 1 denominator == retinoid prevalence numerator
  switcher1.denom_notequal_retinoid.prevalence.num,
  # 6. Switcher 2 denominator == retinoid discontinued numerator
  switcher2.denom_notequal_retinoid.discontinued.num,
  # 7. Sum of all switcher age group numerators == Switcher age group denominator == RAM switcher Numerator 
  switcher1.agegroup.num.total_vs_switcher1.num,
  switcher1.agegroup.num.total_vs_switcher1.agegroup.denom,
  switcher1.num_vs_switcher1.agegroup.denom,
  switcher2.agegroup.num.total_vs_switcher2.num,
  switcher2.agegroup.num.total_vs_switcher2.agegroup.denom,
  switcher2.num_vs_switcher2.agegroup.denom,
  # 8. Sum of all switcher indication numerators == Switcher indication denominator == RAM switchter Numerator 
  switcher1.indication.num.total_vs_switcher1.num,
  switcher1.indication.num.total_vs_switcher1.indication.denom,
  switcher1.num_vs_switcher1.indication.denom,
  switcher2.indication.num.total_vs_switcher2.num,
  switcher2.indication.num.total_vs_switcher2.indication.denom,
  switcher2.num_vs_switcher2.indication.denom
)

flowchart_objective2<-data.table(names, values)
View(flowchart_objective2)
# saveRDS(flowchart_objective2, paste0(g_output_dir,"/medicines_counts/flowchart_objective2.rds")) 

