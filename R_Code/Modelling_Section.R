#####################################################################################################
# Load Data #
#############

# Opens DB connection
source(paste(Path, "/R_Code/OpenDB.R", sep = ""))

if (Load_Offline_Data == "YES") {
  # If offline data is needed
  load(paste(getwd(), "/Offline_WS.RData", sep = ""))
} else {
  # Loads DB Data - For Model Building
  source(paste(Path, "/R_Code/Load_DB_Data.R", sep = ""))
}

# Cleans data used for modelling
source(paste(Path, "/R_Code/Clean_Model_Data.R", sep = ""))

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
