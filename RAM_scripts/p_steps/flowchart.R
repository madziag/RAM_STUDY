# Creates dataframe
names <- c("RAM incident users",
           "RAM prevalent users",
           
           "RAM discontinued users",
           "RAM switchers",
           
           "RAM general concomitant users",
           "RAM general concomitant records",

           "RAM contraindicated users (concomitance not taken into consideration)",
           "RAM contraindicated records (concomitance not taken into consideration)",
           "RAM contraindicated users who were concomitant with Retinoids",

           "RAM teratogenic users (concomitance not taken into consideration)",
           "RAM teratogenic records (concomitance not taken into consideration)",
           "RAM teratogenic users who were concomitant with Retinoids"
           )

values <- c(RAM_flowchart_incidence,
            RAM_flowchart_prevalence,
            
            RAM_flowchart_discontinued,
            RAM_flowchart_switcher,
            
            RAM_flowchart_concomit_users,
            RAM_flowchart_concomit_records,

            RAM_flowchart_contraindicated_users,
            RAM_flowchart_contraindicated_records,
            RAM_flowchart_contraindicated_concomitant,

            RAM_flowchart_teratogenic_users,
            RAM_flowchart_teratogenic_records,
            RAM_flowchart_teratogenic_concomitant
            )


flowchart<-data.table(names, values)
