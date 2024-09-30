###############################################################################
################################ RAW RAM COUNTS ###############################
###############################################################################
### TESTS ###
# 1. Record counts > User Counts for each ATC (PRE & POST)
# 2. Number of unique ATC codes in records counts == Number of unique ATC codes in user counts (OVERALL,for each indication)
# 3. Acne + Dermatitis + Psoriasis record counts == Overall record counts (PRE & POST)
# 4. Acne + Dermatitis + Psoriasis user counts (per ATC) == Overall user counts (PRE & POST)

###############################################################################
###############################################################################
###############################################################################
# 1. Record counts >= User Counts for each ATC (PRE & POST)
RAM_records_users_merged<-merge(RAM_records_all[,.(ATC.RAM,pre,post)],RAM_users_all[,.(ATC.RAM,pre,post)],by="ATC.RAM",suffixes=c("_records","_users"))
#check
RAM.records.pre_lessthan_RAMusers.pre<-nrow(RAM_records_users_merged[pre_records<pre_users,])
RAM.records.post_lessthan_RAMusers.post<-nrow(RAM_records_users_merged[post_records<post_users,])

# 2. Number of unique ATC codes in records counts == Number of unique ATC codes in user counts (OVERALL,for each indication)
#check
nr.unique.ATC.records_notequal_nr.unique.ATC.users<-length(unique(RAM_records_all$ATC.RAM))-length(unique(RAM_users_all$ATC.RAM))
nr.unique.ATC.records_notequal_nr.unique.ATC.users.ACNE<-length(unique(RAM_records_acne$ATC.RAM))-length(unique(RAM_users_acne$ATC.RAM))
nr.unique.ATC.records_notequal_nr.unique.ATC.users.DERMATITIS<-length(unique(RAM_records_derm$ATC.RAM))-length(unique(RAM_users_derm$ATC.RAM))
nr.unique.ATC.records_notequal_nr.unique.ATC.users.PSORIASIS<-length(unique(RAM_records_psor$ATC.RAM))-length(unique(RAM_users_psor$ATC.RAM))

# 3. Acne + Dermatitis + Psoriasis record counts == Overall record counts (PRE & POST)
# combine record files
RAM_records_combined<-rbindlist(list(
  RAM_records_acne[,.(ATC.RAM,pre,post)],
  RAM_records_derm[,.(ATC.RAM,pre,post)],
  RAM_records_psor[,.(ATC.RAM,pre,post)]
))
# sum 'N' by 'YM' across age groups
RAM_records_sums<-RAM_records_combined[,.(pre_sum_ind=sum(pre,na.rm=TRUE),post_sum_ind=sum(post,na.rm=TRUE)), by = ATC.RAM]
# merge the summed age group data with incidence_all by 'YM'
RAM_records_merged<-merge(RAM_records_all[,.(ATC.RAM,pre_from_all=pre,post_from_all=post)],RAM_records_sums,by="ATC.RAM",all.x=TRUE)
#check
RAM.records.pre.from.all_notequal_RAM.records.pre.from.indication.sum<-nrow(RAM_records_merged[pre_from_all!=pre_sum_ind,])
RAM.records.post.from.all_notequal_RAM.records.post.from.indication.sum<-nrow(RAM_records_merged[post_from_all!=post_sum_ind,])

# 4. Acne + Dermatitis + Psoriasis user counts (per ATC) == Overall user counts (PRE & POST)
# combine user files
RAM_users_combined<-rbindlist(list(
  RAM_users_acne[,.(ATC.RAM,pre,post)],
  RAM_users_derm[,.(ATC.RAM,pre,post)],
  RAM_users_psor[,.(ATC.RAM,pre,post)]
))
# sum 'N' by 'YM' across age groups
RAM_users_sums<-RAM_users_combined[,.(pre_sum_ind=sum(pre,na.rm=TRUE),post_sum_ind=sum(post,na.rm=TRUE)), by = ATC.RAM]
# merge the summed age group data with incidence_all by 'YM'
RAM_users_merged<-merge(RAM_users_all[,.(ATC.RAM,pre_from_all=pre,post_from_all=post)],RAM_users_sums,by="ATC.RAM",all.x=TRUE)
#check
RAM.users.pre.from.all_notequal_RAM.users.pre.from.indication.sum<-nrow(RAM_users_merged[pre_from_all!=pre_sum_ind,])
RAM.users.post.from.all_notequal_RAM.users.post.from.indication.sum<-nrow(RAM_users_merged[post_from_all!=post_sum_ind,])

### FLOWCHART ###
names<-c(
  # 1. Record counts > User Counts for each ATC (PRE & POST)
  "RAM.records.pre_lessthan_RAMusers.pre",
  "RAM.records.post_lessthan_RAMusers.post",
  # 2. Number of unique ATC codes in records counts == Number of unique ATC codes in user counts (OVERALL,for each indication)
  "nr.unique.ATC.records_notequal_nr.unique.ATC.users",
  "nr.unique.ATC.records_notequal_nr.unique.ATC.users.ACNE",
  "nr.unique.ATC.records_notequal_nr.unique.ATC.users.DERMATITIS",
  "nr.unique.ATC.records_notequal_nr.unique.ATC.users.PSORIASIS",
  # 3. Acne + Dermatitis + Psoriasis record counts == Overall record counts (PRE & POST)
  "RAM.records.pre.from.all_notequal_RAM.records.pre.from.indication.sum",
  "RAM.records.post.from.all_notequal_RAM.records.post.from.indication.sum",
  # 4. Acne + Dermatitis + Psoriasis user counts (per ATC) == Overall user counts (PRE & POST)
  "RAM.users.pre.from.all_notequal_RAM.users.pre.from.indication.sum",
  "RAM.users.post.from.all_notequal_RAM.users.post.from.indication.sum"
)

values<-c(
  # 1. Record counts > User Counts for each ATC (PRE & POST)
  RAM.records.pre_lessthan_RAMusers.pre,
  RAM.records.post_lessthan_RAMusers.post,
  # 2. Number of unique ATC codes in records counts == Number of unique ATC codes in user counts (OVERALL,for each indication)
  nr.unique.ATC.records_notequal_nr.unique.ATC.users,
  nr.unique.ATC.records_notequal_nr.unique.ATC.users.ACNE,
  nr.unique.ATC.records_notequal_nr.unique.ATC.users.DERMATITIS,
  nr.unique.ATC.records_notequal_nr.unique.ATC.users.PSORIASIS,
  # 3. Acne + Dermatitis + Psoriasis record counts == Overall record counts (PRE & POST)
  RAM.records.pre.from.all_notequal_RAM.records.pre.from.indication.sum,
  RAM.records.post.from.all_notequal_RAM.records.post.from.indication.sum,
  # 4. Acne + Dermatitis + Psoriasis user counts (per ATC) == Overall user counts (PRE & POST)
  RAM.users.pre.from.all_notequal_RAM.users.pre.from.indication.sum,
  RAM.users.post.from.all_notequal_RAM.users.post.from.indication.sum 
)

flowchart_rawRAMcounts<-data.table(names, values)
View(flowchart_rawRAMcounts)
# saveRDS(flowchart_rawRAMcounts, paste0(g_output_dir,"/medicines_counts/flowchart_rawRAMcounts.rds")) 

