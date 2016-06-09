if (agnt %in% AffAgents$ZLAGENT & nrow(SubsetData) > 0) {
  
  SubsetData$ZLAGENT <- agnt
  SubsetData$ZLAGENT <- as.factor(SubsetData$ZLAGENT)
  
  Preds <- predict(object = Model,
                   newdata = SubsetData,
                   n.trees = ntrees,
                   type = "response")
  SubsetData$Pred <- Preds
  
  MaxID <- SubsetData$ID[which(SubsetData$Pred == max(SubsetData$Pred))][1]
  PredVal <- SubsetData$Pred[SubsetData$ID == MaxID]
  PredVal[is.na(PredVal)] <- 0
  PredVal <- max(PredVal)
  
  UpdateR <- data.frame(ZLAGENT = AffAgents$ZWINGMASTER[AffAgents$ZLAGENT == agnt],
                        Pred    = PredVal,
                        ID      = MaxID)
  
  LeadOut <- rbind(LeadOut, UpdateR)
  
  SubsetData        <- SubsetData[SubsetData$ID != MaxID, ]
  ManLead_Dat_Distr <- ManLead_Dat_Distr[ManLead_Dat_Distr$ID != MaxID, ]
  
  AffAgents$PerAgent[AffAgents$ZLAGENT == agnt] <- AffAgents$PerAgent[AffAgents$ZLAGENT == agnt] - 1
  
  Allocation_Dat$PerAgent[Allocation_Dat$ZLAGENT == agnt] <- Allocation_Dat$PerAgent[Allocation_Dat$ZLAGENT == agnt] - 1
  
  # Remove agent if maxed allocation reached
  AffAgents <- AffAgents[AffAgents$PerAgent > 0, ]
  
  affCount <- affCount + 1 
  
  print(paste(aff, ":", affCount))
  
}















