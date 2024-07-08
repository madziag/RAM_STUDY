# Creates dataframe
names <- c("RAM Incident Records",
           "RAM Incident Records- Acne",
           "RAM Incident Records - Dermatitis",
           "RAM Incident Records - Psoriasis",
           
           "RAM Prevalent Records",
           "RAM Prevalent Records - Acne",
           "RAM Prevalent Records - Dermatitis",
           "RAM Prevalent Records - Psoriasis",
           
           "RAM Discontinued Records",
           "RAM Discontinued Records - Acne",
           "RAM Discontinued Records - Dermatitis",
           "RAM Discontinued Records - Psoriasis",
           
           "RAM Switchers",
           "RAM Switchers Records - Acne",
           "RAM Switchers Records - Dermatitis",
           "RAM Switchers Records - Psoriasis",
           
           "RAM general concomitant records",
           "RAM general concomitant users",
           "RAM general concomitant users - Acne",
           "RAM general concomitant users - Dermatitis",
           "RAM general concomitant users - Psoriasis",

           "RAM general concomitant contraindicated records",
           "RAM general concomitant contraindicated users",
           "RAM general concomitant contraindicated users - Acne",
           "RAM general concomitant contraindicated users - Dermatitis",
           "RAM general concomitant contraindicated users - Psoriasis",
           
           "RAM general teratogenic records",
           "RAM general teratogenic users",
           "RAM general teratogenic users - Acne",
           "RAM general teratogenic users - Dermatitis",
           "RAM general teratogenic users - Psoriasis",
           
           "All RAMs in Retinoid Users - users",
           "All RAMs in Retinoid Users - records",

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
            RAM_flowchart_prevalence_psoriasis,
            
            RAM_flowchart_discontinued,
            RAM_flowchart_discontinued_acne,
            RAM_flowchart_discontinued_dermatitis,
            RAM_flowchart_discontinued_psoriasis,
            
            RAM_flowchart_switcher,
            RAM_flowchart_switcher_acne,
            RAM_flowchart_switcher_dermatitis,
            RAM_flowchart_switcher_psoriasis,
            
            RAM_flowchart_concomit_records,
            RAM_flowchart_concomit_users,
            RAM_flowchart_concomit_acne,
            RAM_flowchart_concomit_dermatitis,
            RAM_flowchart_concomit_psoriasis,

            RAM_flowchart_concomit_contraindicated_records,
            RAM_flowchart_concomit_contraindicated_users,
            RAM_flowchart_concomit_contraindicated_acne,
            RAM_flowchart_concomit_contraindicated_dermatitis,
            RAM_flowchart_concomit_contraindicated_psoriasis,
            
            RAM_flowchart_teratogenic_records,
            RAM_flowchart_teratogenic_users,
            RAM_flowchart_teratogenic_acne,
            RAM_flowchart_teratogenic_dermatitis,
            RAM_flowchart_teratogenic_psoriasis,
            
            RAM_flowchart_allRAM_users,
            RAM_flowchart_allRAM_records,
            
            RAM_flowchart_unrelated_users,
            RAM_flowchart_unrelated_records

            )


flowchart<-data.table(names, values)

saveRDS(flowchart, paste0(medicines_counts_dir,"/", pop_prefix, "_flowchart.rds")) 

