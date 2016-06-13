# Clean Data --------------------------------------------------------------
Allocation_Dat$ZWINGMASTER <- paste(Allocation_Dat$AgentName, Allocation_Dat$AgentSurname, sep = " ")
Allocation_Dat$ZLAGENT     <- gsub(" ", "", gsub("[^[:alpha:] ]", "", toupper(Allocation_Dat$ZWINGMASTER)))

OwnedAffs <- Allocation_Dat$Owner[Allocation_Dat$Owner != "NONE"]
OwnedAffs <- gsub(" ", "", gsub("[^[:alpha:] ]", "", toupper(OwnedAffs)))

LeadOut <- data.frame(ZLAGENT = as.character(),
                      Pred    = as.numeric(),
                      ID      = as.integer())

# Allocate/Rank Owned Affinities ------------------------------------------
if (length(OwnedAffs) > 0) {

  for (aff in OwnedAffs) {
    
    agent   <- Allocation_Dat$ZLAGENT[gsub(" ", "", gsub("[^[:alpha:] ]", "", toupper(Allocation_Dat$Owner))) == aff]
    agentid <- Allocation_Dat$AgentID[gsub(" ", "", gsub("[^[:alpha:] ]", "", toupper(Allocation_Dat$Owner))) == aff]
    
    SubsetData <- ManLead_Dat[grepl(aff, ManLead_Dat$AFFINITY), ]
    SubsetData <- SubsetData[, c(Model$var.names, "ID")]
    
    if (nrow(SubsetData) > 0) {
      
      SubsetData$ZLAGENT <- agent
      SubsetData$ZLAGENT <- as.factor(SubsetData$ZLAGENT)
      
      Preds <- predict(object = Model,
                       newdata = SubsetData,
                       n.trees = ntrees,
                       type = "response")
      SubsetData$Pred <- Preds
      
      UpdateR <- data.frame(ZLAGENT = Allocation_Dat$ZWINGMASTER[Allocation_Dat$AgentID == agentid],
                            Pred    = SubsetData$Pred, 
                            ID      = SubsetData$ID) 
      
      LeadOut <- rbind(LeadOut, UpdateR)
      
      ManLead_Dat     <- ManLead_Dat[!(ManLead_Dat$ID %in% SubsetData$ID), ]
      
    }
    
  }
  
  source(paste(Path, "/R_Code/Insert.R", sep = ""))
  
  ManLead_DatOrig <- ManLead_DatOrig[!(ManLead_DatOrig$ID %in% SubsetData$ID), ]
  
}


# Allocate all other leads ------------------------------------------------
# Determine allocation numbers
InSplit  <- sum(Allocation_Dat$Owner == "NONE")
ToSplit  <- length(ManLead_Dat$ID[!(ManLead_Dat$AFFINITY %in% OwnedAffs)])
perAgent <- ceiling(ToSplit / InSplit)

# Find "other" agents
Allocation_Dat <- Allocation_Dat[Allocation_Dat$Owner == "NONE", ]
Allocation_Dat$PerAgent <- perAgent

# Fins "other" affinities
Affinities <- unique(ManLead_Dat$AFFINITY)
Affinities[is.na(Affinities)] <- "OTHER"

aff.query <- paste("SELECT * ",
                   "FROM specialAffinities ",
                   sep = "")

specialAffinities <- dbGetQuery(my_new_db, aff.query)
specialAffinities$AFFINITY <- gsub(" ", "", gsub("[^[:alpha:] ]", "", toupper(specialAffinities$AffinityName)))

ManLead_Dat$TempAFFINITY <- ManLead_Dat$AFFINITY
ManLead_Dat$TempAFFINITY[!(ManLead_Dat$TempAFFINITY %in% specialAffinities$AFFINITY)] <- "OTHER"
























