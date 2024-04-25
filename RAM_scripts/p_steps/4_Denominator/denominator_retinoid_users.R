# Create denominator counts of WOCBP who used retinoids
# A person is counted in a month-year if that month-year falls within the retinoid treatment episode
# A person can only appear once in a month-year

# Load retinoid prevalent counts data frame 
retinoid_prev<-as.data.table(readRDS(paste0(objective1_dir, pop_prefix, "_Retinoid_prevalence_counts_df.rds")))
retinoid_prev<-unique(retinoid_prev, by = c("person_id", "year", "month"))

# Count Number of users per month-year 
denom_retinoid_users<-retinoid_prev[,.N, by = .(year,month)]
# Merge with empty df (for counts that do not have counts for all months and years of study)
denom_retinoid_users<-as.data.table(merge(x = empty_df, y = denom_retinoid_users, by = c("year", "month"), all.x = TRUE))
# Fills in missing values with 0
denom_retinoid_users[is.na(N), N:=0]

