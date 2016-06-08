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

repeat {
  
  # Disable after 10PM and before 5AM
  curtime <- as.numeric(format(Sys.time(), "%H")) + as.numeric(format(Sys.time(), "%M"))/60
  
  if (curtime > 5 & curtime < 10) {
    
    rm(list = ls())
    gc()
    
    Path <- getwd()
    
    source(paste(Path, "/R_Code/Initialize.R", sep = ""))
    
    curday  <- weekdays(Sys.Date())
    curtime <- as.numeric(format(Sys.time(), "%H")) + as.numeric(format(Sys.time(), "%M"))/60
    
    # Do recycling every Wednesday between 13:00 and 14:00 (thus curtime between 13.00 and 14.00)
    # Also do recycling every Thursday and Friday between 5AM and 6AM
    if ((curday == "Wednesday" & curtime > 13 & curtime < 14) |
        (curday == "Thursday" & curtime > 5 | curtime < 6) |
        (curday == "Friday" & curtime > 5 | curtime < 6)) {
      
      source(paste(Path, "/R_Code/Allocation_Manual_Recycled.R", sep = ""))
      
    } else if ((curtime - floor(curtime))  > 0.4 | (curtime - floor(curtime)) < 0.6) { # Recalc the probabilities in the DB each half an hour
      
      source(paste(Path, "/R_Code/Recalc_DB_Probs.R", sep = ""))
      
    }
    
    dbDisconnectAll()
    
  }
  
}



# Manual Allocation of new leads
source(paste(Path, "/R_Code/Allocation_Manual_New.R", sep = ""))





