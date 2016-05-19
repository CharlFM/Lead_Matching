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
Load_Offline_Data <- "YES"

# Important to step through the insides of this script when updating the model.
source(paste(Path, "/R_Code/Modelling_Section.R", sep = ""))  # Need to take out parts that is done in the data import step - such as appending Culture and Race info where it already exists (if in new db, old db - update)

#####################################################################################################
# Allocation #
##############

# Loads New lead data to DB - For Allocation
source(paste(Path, "/R_Code/Load_Lead_Data.R", sep = "")) # TO BE MOVED TO INSIDE THE LOOP (continuously read new lead data) !!!! 
                                                          # Will be a seperate script that will run from VB.NET
repeat {
  
  source(paste(Path, "/R_Code/Allocation.R", sep = ""))
  
  dbDisconnectAll()
  # Allows to refresh each 2 minutes
  Sys.sleep(120) 
  
}



# Manual Allocation 
source(paste(Path, "/R_Code/Manual_Allocation.R", sep = ""))





