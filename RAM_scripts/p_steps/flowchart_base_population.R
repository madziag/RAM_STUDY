
#Flowchart
Total_WOCBP<-nrow(study_population)+sterility_before_entry_date
Total_WOCBP_after_sterility_exclusion<-nrow(study_population)
WOCBP_excluded_due_to_sterility<-sterility_before_entry_date

Total_Retinoid_users<-all_retinoid_users
Retinoid_users_within_entry_exit<-retinoid_within_entry_exit
Retinoid_users_outside_entry_exit<-all_retinoid_users-retinoid_within_entry_exit

# Creates dataframe
names <- c("Total WOCBP",
           "Total WOCBP after sterility exclusion",
           "WOCBP excluded due to sterility",
           
           "Total Retinoid Users",
           "Retinoid users - within (Entry-90days and Exit)",
           "Retinoid users - outside of Entry-Exit"
)

values <- c(nrow(study_population)+sterility_before_entry_date,
            nrow(study_population),
            sterility_before_entry_date,
            
            all_retinoid_users,
            retinoid_within_entry_exit,
            all_retinoid_users-retinoid_within_entry_exit
)


flowchart<-data.table(names, values)

saveRDS(flowchart, paste0(output_dir,"/", pop_prefix, "_base_population_flowchart.rds")) 
