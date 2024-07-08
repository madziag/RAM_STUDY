###########################################################################################################################
################################################## Pooling BIFAP results ##################################################
###########################################################################################################################
# Create Vector with Regions
regions<-c("AR","AS","CA","CL","CM","CN","MA","MU","NA")

# ###########################################################################################################################
# ###########################################################################################################################
# ###########################################################################################################################
# ########## Objective 1 ##########
# ###########################################################################################################################
# ###########################################################################################################################
# ###########################################################################################################################
# 
# 
# ###########################################################################################################################
# ### Incidence - PC ###
# ###########################################################################################################################
# 
# # Create an empty data table
# total_incidence_PC<-data.table()
# # For each region:
# for(reg in 1:length(regions)){
#   # Check if file exists before reading it in 
#   if (file.exists(paste0("C:/Users/mgamb/Desktop/BIFAP_results/", regions[reg], "/g_output/medicines_counts/RAM_Objective_1/PC_RAM_incidence_counts.rds"))){
#     # Read in PC incidence data
#     incidence_PC<-as.data.table(readRDS(paste0("C:/Users/mgamb/Desktop/BIFAP_results/", regions[reg], "/g_output/medicines_counts/RAM_Objective_1/PC_RAM_incidence_counts.rds")))
#     # Keep only columns YM,N,Freq
#     incidence_PC<-incidence_PC[,c("YM","N","Freq")]
#     # Add to previously created data.table => incidence data from all regions will be read in one by one and bound into one data table
#     total_incidence_PC<-rbind(total_incidence_PC,incidence_PC)
#   } else {
#     print(paste("no incidence data for PC - region",regions[reg]))
#   }
# }
# 
# # Sum up N and Freq by Year 
# total_incidence_PC[,N_all:=sum(N),by=list(YM)][,Freq_all:=sum(Freq),by=list(YM)]
# # Drop Columns Y and Freq
# total_incidence_PC<-total_incidence_PC[,-c("N","Freq")]
# # Keep 1 row per YM
# total_incidence_PC<-unique(total_incidence_PC,by=c("YM"))
# # Calculate Rates
# total_incidence_PC[,rates:=round(as.numeric(N_all)/as.numeric(Freq_all),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates),rates:=0]
# # Save File 
# saveRDS(total_incidence_PC, paste0("C:/Users/mgamb/Desktop/BIFAP_results/Pooled/Pooled_PC_RAM_incidence_data.rds"))
# # Clean Up
# rm(incidence_PC)
# 
# ###########################################################################################################################
# ### Incidence - PC_HOSP ###
# ###########################################################################################################################
# 
# # Create an empty data table
# total_incidence_PC_HOSP<-data.table()
# # For each region:
# for(reg in 1:length(regions)){
#   # Check if file exists before reading it in 
#   if (file.exists(paste0("C:/Users/mgamb/Desktop/BIFAP_results/", regions[reg], "/g_output/medicines_counts/RAM_Objective_1/PC_HOSP_RAM_incidence_counts.rds"))){
#     # Read in PC_HOSP incidence data
#     incidence_PC_HOSP<-as.data.table(readRDS(paste0("C:/Users/mgamb/Desktop/BIFAP_results/", regions[reg], "/g_output/medicines_counts/RAM_Objective_1/PC_HOSP_RAM_incidence_counts.rds")))
#     # Keep only columns YM,N,Freq
#     incidence_PC_HOSP<-incidence_PC_HOSP[,c("YM","N","Freq")]
#     # Add to previously created data.table => incidence data from all regions will be read in one by one and bound into one data table
#     total_incidence_PC_HOSP<-rbind(total_incidence_PC_HOSP,incidence_PC_HOSP)
#   } else {
#     print(paste("no incidence data for PC_HOSP - region",regions[reg]))
#   }
# }
# 
# # Sum up N and Freq by Year 
# total_incidence_PC_HOSP[,N_all:=sum(N),by=list(YM)][,Freq_all:=sum(Freq),by=list(YM)]
# # Drop Columns Y and Freq
# total_incidence_PC_HOSP<-total_incidence_PC_HOSP[,-c("N","Freq")]
# # Keep 1 row per YM
# total_incidence_PC_HOSP<-unique(total_incidence_PC_HOSP,by=c("YM"))
# # Calculate Rates
# total_incidence_PC_HOSP[,rates:=round(as.numeric(N_all)/as.numeric(Freq_all),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates),rates:=0]
# # Save File 
# saveRDS(total_incidence_PC_HOSP, paste0("C:/Users/mgamb/Desktop/BIFAP_results/Pooled/Pooled_PC_HOSP_RAM_incidence_data.rds"))
# # Clean Up
# rm(incidence_PC_HOSP)
# 
# 
# ###########################################################################################################################
# ### Prevalence - PC ###
# ###########################################################################################################################
# 
# # Create an empty data table
# total_prevalence_PC<-data.table()
# # For each region:
# for(reg in 1:length(regions)){
#   # Check if file exists before reading it in 
#   if (file.exists(paste0("C:/Users/mgamb/Desktop/BIFAP_results/", regions[reg], "/g_output/medicines_counts/RAM_Objective_1/PC_RAM_prevalence_counts.rds"))){
#     # Read in PC prevalence data
#     prevalence_PC<-as.data.table(readRDS(paste0("C:/Users/mgamb/Desktop/BIFAP_results/", regions[reg], "/g_output/medicines_counts/RAM_Objective_1/PC_RAM_prevalence_counts.rds")))
#     # Keep only columns YM,N,Freq
#     prevalence_PC<-prevalence_PC[,c("YM","N","Freq")]
#     # Add to previously created data.table => prevalence data from all regions will be read in one by one and bound into one data table
#     total_prevalence_PC<-rbind(total_prevalence_PC,prevalence_PC)
#   } else {
#     print(paste("no prevalence data for PC - region",regions[reg]))
#   }
# }
# 
# # Sum up N and Freq by Year 
# total_prevalence_PC[,N_all:=sum(N),by=list(YM)][,Freq_all:=sum(Freq),by=list(YM)]
# # Drop Columns Y and Freq
# total_prevalence_PC<-total_prevalence_PC[,-c("N","Freq")]
# # Keep 1 row per YM
# total_prevalence_PC<-unique(total_prevalence_PC,by=c("YM"))
# # Calculate Rates
# total_prevalence_PC[,rates:=round(as.numeric(N_all)/as.numeric(Freq_all),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates),rates:=0]
# # Save File 
# saveRDS(total_prevalence_PC, paste0("C:/Users/mgamb/Desktop/BIFAP_results/Pooled/Pooled_PC_RAM_prevalence_data.rds"))
# # Clean Up
# rm(prevalence_PC)
# 
# 
# ###########################################################################################################################
# ### Prevalence - PC_HOSP ###
# ###########################################################################################################################
# 
# # Create an empty data table
# total_prevalence_PC_HOSP<-data.table()
# # For each region:
# for(reg in 1:length(regions)){
#   # Check if file exists before reading it in 
#   if (file.exists(paste0("C:/Users/mgamb/Desktop/BIFAP_results/", regions[reg], "/g_output/medicines_counts/RAM_Objective_1/PC_HOSP_RAM_prevalence_counts.rds"))){
#     # Read in PC_HOSP prevalence data
#     prevalence_PC_HOSP<-as.data.table(readRDS(paste0("C:/Users/mgamb/Desktop/BIFAP_results/", regions[reg], "/g_output/medicines_counts/RAM_Objective_1/PC_HOSP_RAM_prevalence_counts.rds")))
#     # Keep only columns YM,N,Freq
#     prevalence_PC_HOSP<-prevalence_PC_HOSP[,c("YM","N","Freq")]
#     # Add to previously created data.table => prevalence data from all regions will be read in one by one and bound into one data table
#     total_prevalence_PC_HOSP<-rbind(total_prevalence_PC_HOSP,prevalence_PC_HOSP)
#   } else {
#     print(paste("no prevalence data for PC_HOSP - region",regions[reg]))
#   }
# }
# 
# # Sum up N and Freq by Year 
# total_prevalence_PC_HOSP[,N_all:=sum(N),by=list(YM)][,Freq_all:=sum(Freq),by=list(YM)]
# # Drop Columns Y and Freq
# total_prevalence_PC_HOSP<-total_prevalence_PC_HOSP[,-c("N","Freq")]
# # Keep 1 row per YM
# total_prevalence_PC_HOSP<-unique(total_prevalence_PC_HOSP,by=c("YM"))
# # Calculate Rates
# total_prevalence_PC_HOSP[,rates:=round(as.numeric(N_all)/as.numeric(Freq_all),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates),rates:=0]
# # Save File 
# saveRDS(total_prevalence_PC_HOSP, paste0("C:/Users/mgamb/Desktop/BIFAP_results/Pooled/Pooled_PC_HOSP_RAM_prevalence_data.rds"))
# # Clean Up
# rm(prevalence_PC_HOSP)

###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
########## Objective 2 ##########
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################


###########################################################################################################################
### Discontinued - PC ###
###########################################################################################################################

# Create an empty data table
total_discontinued_PC<-data.table()
# For each region:
for(reg in 1:length(regions)){
  # Check if file exists before reading it in 
  if (file.exists(paste0("C:/Users/mgamb/Desktop/BIFAP_results/", regions[reg], "/g_output/medicines_counts/RAM_Objective_2/PC_RAM_discontinued_counts.rds"))){
    # Read in PC discontinued data
    discontinued_PC<-as.data.table(readRDS(paste0("C:/Users/mgamb/Desktop/BIFAP_results/", regions[reg], "/g_output/medicines_counts/RAM_Objective_2/PC_RAM_discontinued_counts.rds")))
    # Keep only columns YM,N,Freq
    discontinued_PC<-discontinued_PC[,c("YM","N","Freq")]
    # Add to previously created data.table => discontinued data from all regions will be read in one by one and bound into one data table
    total_discontinued_PC<-rbind(total_discontinued_PC,discontinued_PC)
  } else {
    print(paste("no discontinued data for PC - region",regions[reg]))
  }
}

# Sum up N and Freq by Year 
total_discontinued_PC[,N_all:=sum(N),by=list(YM)][,Freq_all:=sum(Freq),by=list(YM)]
# Drop Columns Y and Freq
total_discontinued_PC<-total_discontinued_PC[,-c("N","Freq")]
# Keep 1 row per YM
total_discontinued_PC<-unique(total_discontinued_PC,by=c("YM"))
# Calculate Rates
total_discontinued_PC[,rates:=round(as.numeric(N_all)/as.numeric(Freq_all),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates),rates:=0]
# Save File 
saveRDS(total_discontinued_PC, paste0("C:/Users/mgamb/Desktop/BIFAP_results/Pooled/Pooled_PC_RAM_discontinued_data.rds"))
# Clean Up
rm(discontinued_PC)



###########################################################################################################################
### Discontinued - PC_HOSP ###
###########################################################################################################################

# Create an empty data table
total_discontinued_PC_HOSP<-data.table()
# For each region:
for(reg in 1:length(regions)){
  # Check if file exists before reading it in 
  if (file.exists(paste0("C:/Users/mgamb/Desktop/BIFAP_results/", regions[reg], "/g_output/medicines_counts/RAM_Objective_2/PC_HOSP_RAM_discontinued_counts.rds"))){
    # Read in PC_HOSP discontinued data
    discontinued_PC_HOSP<-as.data.table(readRDS(paste0("C:/Users/mgamb/Desktop/BIFAP_results/", regions[reg], "/g_output/medicines_counts/RAM_Objective_2/PC_HOSP_RAM_discontinued_counts.rds")))
    # Keep only columns YM,N,Freq
    discontinued_PC_HOSP<-discontinued_PC_HOSP[,c("YM","N","Freq")]
    # Add to previously created data.table => discontinued data from all regions will be read in one by one and bound into one data table
    total_discontinued_PC_HOSP<-rbind(total_discontinued_PC_HOSP,discontinued_PC_HOSP)
  } else {
    print(paste("no discontinued data for PC_HOSP - region",regions[reg]))
  }
}

# Sum up N and Freq by Year 
total_discontinued_PC_HOSP[,N_all:=sum(N),by=list(YM)][,Freq_all:=sum(Freq),by=list(YM)]
# Drop Columns Y and Freq
total_discontinued_PC_HOSP<-total_discontinued_PC_HOSP[,-c("N","Freq")]
# Keep 1 row per YM
total_discontinued_PC_HOSP<-unique(total_discontinued_PC_HOSP,by=c("YM"))
# Calculate Rates
total_discontinued_PC_HOSP[,rates:=round(as.numeric(N_all)/as.numeric(Freq_all),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates),rates:=0]
# Save File 
saveRDS(total_discontinued_PC_HOSP, paste0("C:/Users/mgamb/Desktop/BIFAP_results/Pooled/Pooled_PC_HOSP_RAM_discontinued_data.rds"))
# Clean Up
rm(discontinued_PC_HOSP)





### Switcher ###

########## Objective 3 ##########
### Concomitant ###
### Contraindicated ###

########## Objective 4 ##########
### Teratogenic ###

########## ATC Counts ##########
### Records ###
### Users ###
