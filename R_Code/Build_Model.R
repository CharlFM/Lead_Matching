
objControl <- trainControl(method        =  "cv",
                           number        =  4, 
                           returnResamp  =  "none")

objModel <- train(x            =  trainDF[, predictorsNames], 
                  y            =  trainDF[, outcomeName], 
                  method       =  "glmnet",  
                  metric       =  "RMSE", 
                  trControl    =  objControl)














