# Creates dataframe
names <- c("RAM Incident Users",
           "RAM Incident Users - Acne",
           "RAM Incident Users - Dermatitis",
           "RAM Incident Users - Psoriasis",
           
           "RAM Prevalent users",
           "RAM Prevalent users - Acne",
           "RAM Prevalent users - Dermatitis",
           "RAM Prevalent users - Psoriasis",
           
           "RAM Discontinued users",
           "RAM Discontinued users - Acne",
           "RAM Discontinued users - Dermatitis",
           "RAM Discontinued users - Psoriasis",
           
           "RAM Switchers",
           "RAM Switchers users - Acne",
           "RAM Switchers users - Dermatitis",
           "RAM Switchers users - Psoriasis",
           
           "RAM general concomitant users",
           "RAM general concomitant users - Acne",
           "RAM general concomitant users - Dermatitis",
           "RAM general concomitant users - Psoriasis",
           "RAM general concomitant records",

           "RAM contraindicated ATC's in general concomitance - users",
           "RAM contraindicated ATC's in general concomitance - records",
           
           "All RAMs in Retinoid Users - users",
           "All RAMs in Retinoid Users - records",
           
           "All Teratogenic RAM in Retinoid Users - users",
           "All Teratogenic RAM in Retinoid Users - records",
           
           "Unrelated - users",
           "Unrelated - records"

           )

values <- c(RAM_flowchart_incidence,
            RAM_flowchart_incidence_acne,
            RAM_flowchart_incidence_dermatitis,
            RAM_flowchart_incidence_psoriasis,
            
            RAM_flowchart_prevalence,
            RAM_flowchart_prevalence_acne,
            RAM_flowchart_prevalence_dermatitis,
            RAM_flowchart_concomit_psoriasis,
            
            RAM_flowchart_discontinued,
            RAM_flowchart_discontinued_acne,
            RAM_flowchart_discontinued_dermatitis,
            RAM_flowchart_discontinued_psoriasis,
            
            RAM_flowchart_switcher,
            RAM_flowchart_switcher_acne,
            RAM_flowchart_switcher_dermatitis,
            RAM_flowchart_switcher_psoriasis,
            
            RAM_flowchart_concomit_users,
            RAM_flowchart_concomit_acne,
            RAM_flowchart_concomit_dermatitis,
            RAM_flowchart_concomit_psoriasis,
            RAM_flowchart_concomit_records,
            
            RAM_flowchart_concomit_users_contraindicated,
            RAM_flowchart_concomit_records_contraindicated,
            
            RAM_flowchart_allRAM_users,
            RAM_flowchart_allRAM_records,
            
            RAM_flowchart_teratogenic_users,
            RAM_flowchart_teratogenic_records,
            
            RAM_flowchart_unrelated_users,
            RAM_flowchart_unrelated_records

            )


flowchart<-data.table(names, values)

saveRDS(flowchart, paste0(medicines_counts_dir,"/", pop_prefix, "_flowchart.rds")) 

