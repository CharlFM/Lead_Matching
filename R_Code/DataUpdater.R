done_today <- 0
repeat {
  
  # Disable after 10PM and before 5AM
  curtime <- as.numeric(format(Sys.time(), "%H")) + as.numeric(format(Sys.time(), "%M"))/60
  
  if (curtime < 5) {
    done_today <- 0
    cat("\014")
  }
  
  if (curtime > 5 & curtime < 22) {
    
    rm(list = ls()[ls() != "done_today"])
    gc()
    
    Path <- getwd()
    
    source(paste(Path, "/R_Code/Initialize.R", sep = ""))
    
    curday  <- weekdays(Sys.Date())
    curtime <- as.numeric(format(Sys.time(), "%H")) + as.numeric(format(Sys.time(), "%M"))/60
    
    # Do recycling every Wednesday, Thursday and Friday between 5AM and 6AM
    if (done_today == 0 & ((curday == "Wednesday" & curtime > 5 & curtime < 6) |
                           (curday == "Thursday" & curtime > 5 | curtime < 6) |
                           (curday == "Friday" & curtime > 5 | curtime < 6))) {
      
      source(paste(Path, "/R_Code/Allocation_Manual_Recycled.R", sep = ""))

      done_today <- 1
      
      print(paste("Recycle at", Sys.time()))
      
    } else if ((curtime - floor(curtime))  > 0 & (curtime - floor(curtime)) < 0.025) { 
      
      source(paste(Path, "/R_Code/Recalc_DB_Probs.R", sep = ""))
      print(paste("Recalc at", Sys.time()))
      
    } else if ((curtime - floor(curtime))  > 0.5 & (curtime - floor(curtime)) < 0.525) { 
      
      source(paste(Path, "/R_Code/Recalc_DB_Probs.R", sep = ""))
      print(paste("Recalc at", Sys.time()))
      
    }
    
  }
  
  rm(list = ls()[ls() != "done_today"])
  
}










