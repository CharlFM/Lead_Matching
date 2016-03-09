

predictions <- predict(objModel, testDF[, predictorsNames])

auc <- roc(testDF[,outcomeName], predictions)
plot(auc)

imp    <-  varImp(objModel, scale = F)
impdf  <-  imp$importance
Vars   <-  rownames(impdf)
Vals   <-  impdf$Overall
impdf  <-  data.frame(Vars, Vals)
impdf  <-  data.frame(impdf[impdf$Vals != 0, ])
impdf  <-  impdf[with(impdf, order(-Vals)), ]























