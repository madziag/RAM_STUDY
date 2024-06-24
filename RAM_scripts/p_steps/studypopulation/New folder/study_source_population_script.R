#Author: Vjola Hoxhaj Drs./Roel Elbers MSc.
#email: v.hoxhaj@umcutrecht.nl/r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021




#################################################################
#Study_population and Source_population
################################################################

########################################################
#Create output folders
########################################################
if (subpopulations_present=="No"){
  #output folder for STUDY_SOURCE_POPULATION report in g_output
  if ("STUDY_SOURCE_POPULATION" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"STUDY_SOURCE_POPULATION"), recursive = T)#delete folder
    dir.create(paste(output_dir, "STUDY_SOURCE_POPULATION", sep=""))
    std_source_pop_dir<-paste(output_dir, "STUDY_SOURCE_POPULATION/", sep="")
    dir.create(paste(std_source_pop_dir,"Masked", sep=""))
  } else {
    #Create the  folder in the output dir
    dir.create(paste(output_dir, "STUDY_SOURCE_POPULATION", sep=""))
    std_source_pop_dir<-paste(output_dir, "STUDY_SOURCE_POPULATION/", sep="")
    dir.create(paste(std_source_pop_dir,"Masked", sep=""))
  }
  

  #STUDY_SOURCE_POPULATION_tmp/STUDY_SOURCE_POPULATION folder where all intermediary files are saved
  if ("STUDY_SOURCE_POPULATION" %in% list.files(tmp)){
    unlink(paste0(tmp,"STUDY_SOURCE_POPULATION"), recursive = T)#delete folder
    dir.create(paste(tmp, "STUDY_SOURCE_POPULATION", sep=""))
    std_pop_tmp<-paste(tmp, "STUDY_SOURCE_POPULATION/", sep="")
  }else{
    #Create the STUDY_SOURCE_POPULATION folder in the output dir
    dir.create(paste(tmp, "STUDY_SOURCE_POPULATION", sep=""))
    std_pop_tmp<-paste(tmp, "STUDY_SOURCE_POPULATION/", sep="")
  }
} else {
  #output folder for MEDICINES report in g_output
  if ("STUDY_SOURCE_POPULATION" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"STUDY_SOURCE_POPULATION"), recursive = T)#delete folder
    dir.create(paste(output_dir, "STUDY_SOURCE_POPULATION", sep=""))
    std_source_pop_dir<-paste(output_dir, "STUDY_SOURCE_POPULATION/", sep="")


    do.call(file.remove, list(list.files(std_source_pop_dir, full.names = T)))
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(std_source_pop_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(std_source_pop_dir, subpopulations_names[i],"/Masked"))
    }

  } else {
    #Create the STUDY_SOURCE_POPULATION folder in the output dir
    dir.create(paste(output_dir, "STUDY_SOURCE_POPULATION", sep=""))
    std_source_pop_dir<-paste(output_dir, "STUDY_SOURCE_POPULATION/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(std_source_pop_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(std_source_pop_dir, subpopulations_names[i],"/Masked"))
    }
  }
  

  #STUDY_SOURCE_POPULATION_tmp/STUDY_SOURCE_POPULATION folder where all intermediary files are saved
  if ("STUDY_SOURCE_POPULATION" %in% list.files(tmp)){
    unlink(paste0(tmp,"STUDY_SOURCE_POPULATION"), recursive = T)#delete folder
    dir.create(paste(tmp, "STUDY_SOURCE_POPULATION", sep=""))
    std_pop_tmp<-paste(tmp, "STUDY_SOURCE_POPULATION/", sep="")
  }else{
    #Create the STUDY_SOURCE_POPULATION folder in the output dir
    dir.create(paste(tmp, "STUDY_SOURCE_POPULATION", sep=""))
    std_pop_tmp<-paste(tmp, "STUDY_SOURCE_POPULATION/", sep="")
  }

}
#######################################################
#std_source_pop_dir output folder for study_source population
#std_pop_tmp output folder for temporary files
#############################################################

#Load functions
source(paste0(pre_dir,"functions/", "CreateSpells_v15.R"))
source(paste0(pre_dir,"functions/", "CountPersonTimeV13.6.R"))
source(paste0(pre_dir,"functions/", "FUNCTIONS.R"))

#Set parameters
source(paste0(pre_dir,"studypopulation/Step_00_SetParameters.R"))

#Preparation of analyses input tables
source(paste0(pre_dir,"studypopulation/Step_01_CreateSpells.R"))
source(paste0(pre_dir,"studypopulation/Step_02_PreparePersonsTable.R"))
source(paste0(pre_dir,"studypopulation/Step_03_CreateSourceTable.R"))
source(paste0(pre_dir,"studypopulation/Step_04_CreateStudyPopulation.R"))

source(paste0(pre_dir,"studypopulation/Step_05_AddVariablesSourcePopulation.R"))
source(paste0(pre_dir,"studypopulation/Step_06_AddVariablesStudyPopulation.R"))
source(paste0(pre_dir,"studypopulation/Step_07_RunCountPersonTime.R"))

# Clean up
rm(list = grep("^actual|^FlowChart|METADATA_subp|^OBSERVATION|^op_meaning|SOURCE|SelectionCriteria|subpopulation_meanings", ls(), value = TRUE))
