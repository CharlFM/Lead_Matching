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

# Enter "YES" or "NO", this will determine if offline data should be used (faster but outdated data)
Load_Offline_Data <- "YES"

#####################################################################################################
# Modelling #
#############

# Important to step through the insides of this script when updating the model.
source(paste(Path, "/R_Code/Modelling_Section.R", sep = "")) 

#####################################################################################################
# Allocation #
##############

# Loads New lead data - For Allocation
source(paste(Path, "/R_Code/Load_Lead_Data.R", sep = "")) # TO BE MOVED TO INSIDE THE LOOP !!!! Will be a seperate script that will run from VB.NET

# Load Current model and start allocation script
load(paste(getwd(), "/Active_Model.RData", sep = ""))

repeat {
  
  source(paste(Path, "/R_Code/Allocation.R", sep = ""))
  Sys.sleep(120) # allows to refresh each 2 minutes
  
}

# Manual Allocation 
source(paste(Path, "/R_Code/Manual_Allocation.R", sep = ""))





