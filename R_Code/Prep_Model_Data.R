####################################################################################################################
# Preparing the model All_lap_Data #
####################################

DummyDat <- dummyVars(formula   =  "~.", 
                      data      =  Dat_Loop, 
                      fullRank  =  FALSE)

Dat_Loop <- as.data.frame(predict(DummyDat, Dat_Loop))

rm(DummyDat)

outcomeName     <- "STATUS"
predictorsNames <- names(Dat_Loop)[names(Dat_Loop) != outcomeName]

splitIndex <- createDataPartition(y      =  Dat_Loop[, outcomeName], 
                                  p      =  0.75, 
                                  list   =  FALSE, 
                                  times  =  1)

trainDF <- Dat_Loop[ splitIndex, ]
testDF  <- Dat_Loop[-splitIndex, ]

if (sum(trainDF$STATUS) == 0) {
  
  rowToAdd <- as.data.frame(testDF[testDF$STATUS == 1, ][1,])
  trainDF  <- rbind(trainDF, rowToAdd)
  
  testDF  <- testDF[rownames(testDF) != rownames(testDF[testDF$STATUS == 1, ][1,]),]
  
  testDF  <- rbind(testDF, trainDF[1, ])
  
  trainDF <- trainDF[(2:nrow(trainDF)), ]
  
}





