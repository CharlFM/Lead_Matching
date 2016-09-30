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
# Modelling #
#############

# Enter "YES" or "NO", this will determine if offline data should be used (faster but outdated data)
Load_Offline_Data <- "NO"

# Important to step through the insides of this script when updating the model.
source(paste(Path, "/R_Code/Modelling_Section.R", sep = ""))  #     Need to take out parts that is done in the data import step - 
                                                              #     such as appending Culture and Race info where it already 
                                                              #     exists (if in new db, old db - update)

#####################################################################################################
# Allocation #
##############

# Manual Allocation of new leads
source(paste(Path, "/R_Code/Allocation_Manual_New.R", sep = ""))

# Check Affinity names
source(paste(Path, "/R_Code/Affinity_Names.R", sep=""))

# Run from here (This should be run continuously)
source(paste(Path, "/R_Code/DataUpdater.R", sep = ""))






