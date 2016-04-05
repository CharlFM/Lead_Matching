#################################################################################################################################
################################################### AssertLife Lapse Analysis ###################################################
#################################################################################################################################

#####################################################################################################
# Initialize #
##############

# Clears Memory
rm(list = ls())
gc()

Path <- getwd()

source(paste(Path, "/R_Code/Initialize.R", sep = ""))

#####################################################################################################
# Load Data #
#############

source(paste(Path, "/R_Code/OpenDB.R", sep = ""))
source(paste(Path, "/R_Code/Load_Data.R", sep = ""))

dbDisconnectAll()

#####################################################################################################
# Cleans Data #
###############

# If offline data is needed
load(paste(getwd(), "/Offline_WS.RData", sep = ""))

# Cleans data used for modelling
source(paste(Path, "/R_Code/Clean_Model_Data.R", sep = ""))

# Cleans data used for prediction
source(paste(Path, "/R_Code/Append_Sort.R", sep = "")) # To be adjusted - loads of changes

#####################################################################################################
# Allocation #
##############

# Load Current model and start allocation script
load(paste(getwd(), "/Active_Model.RData", sep = ""))

repeat {
  
  source(paste(Path, "/R_Code/Allocation.R", sep = ""))
  
}

#####################################################################################################
# Prepare Model Data #
######################

source(paste(Path, "/R_Code/Prep_Model_Data.R", sep = ""))

#####################################################################################################
# Build Model #
###############

source(paste(Path, "/R_Code/Build_Model.R", sep = ""))

# Updates Model

source(paste(Path, "/R_Code/Model_Updater.R", sep = ""))

#####################################################################################################
# Test Model #
##############

source(paste(Path, "/R_Code/Testing.R", sep = ""))

#####################################################################################################
# Analyse Model #
#################

source(paste(Path, "/R_Code/Model_Analysis.R", sep = ""))











