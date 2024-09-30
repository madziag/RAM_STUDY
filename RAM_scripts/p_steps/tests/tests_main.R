# Load libraries
library(data.table)

# Path to Scripts 
pre_dir<-"C:/Users/mgamb/Documents/GitHub/RAM_STUDY/RAM_scripts/p_steps/"

# Load datasets
# PHARMO
source(paste0(pre_dir,"tests/tests_loaddata.R"))
# source(paste0(pre_dir,"tests/tests_loaddata_PC.R"))
# source(paste0(pre_dir,"tests/tests_loaddata_PC_notpooled.R"))

# Objective 1
source(paste0(pre_dir,"tests/tests_objective1.R"))
# Objective 2
source(paste0(pre_dir,"tests/tests_objective2.R"))
# Objective 3
source(paste0(pre_dir,"tests/tests_objective3.R"))
# Objective 4
source(paste0(pre_dir,"tests/tests_objective4.R"))
# RAW RAM Counts
source(paste0(pre_dir,"tests/tests_rawRAMcounts.R"))
# Population Counts
source(paste0(pre_dir,"tests/tests_populations.R"))






