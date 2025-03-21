#Author: Magdalena Gamba 
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 29/04/2024

rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

###############################################
### NOTE FOR DAPS: If you have run the preselection script and would like to use the subsetted data sets that it produces instead of your full ETL'd data files, you need to go to the "99_path.R" file and choose the second path option, by adding a "#" symbol at the start of line 7, and removing the "#" symbol at the start of line 8. If the preselection files have been stored elsewhere, then the path will need to be set manually.

## Please choose DAP NAME 
# DAP_name<-"BIFAP"
DAP_name<-"PHARMO"
#user input parameter
  
## Below you must set

source("99_path.R")
source(paste0(pre_dir,"packages.R"))
source(paste0(pre_dir,"parameters/set_DAP_params.R"))

## Please choose study type
study_type<-"Retinoid"
#user input parameter

## Turn statement to T if multiple regions #BIFAP
# multiple_regions<-F
multiple_regions<-T # BIFAP
multiple_regions_dir<-paste0(path_dir, "BIFAP/")
#user input parameter

## MASKING 
# mask<-T
mask<-F
#user input parameter

## Chose format to save files 
my_format<-"csv"
# my_format<-"xlsx"
#user input parameter

################################################
#Study_source_population + counts + plots
#################################################
source(paste0(pre_dir,"intermediate/run_counts_prelim.R"))

###########################################
# clear g_intermediate 
#set to TRUE to clear out intermediate files PLEASE REPLACE T WITH F IF YOU WANT TO SAVE INTERMEDIATE DATA SETS, I.E. TO REDUCE AMOUNT OF STORED DATA"
clear_int_files<-F
#user input parameter
#set to TRUE to clear out intermediate files PLEASE REPLACE T WITH F IF YOU WANT TO SAVE INTERMEDIATE DATA SETS, I.E. TO REDUCE AMOUNT OF STORED DATA"
if(clear_int_files==T){
  unlink(paste0(g_intermediate, "/tmp"), recursive = TRUE)
  unlink(paste0(g_intermediate, "/populations"), recursive = TRUE)
}




