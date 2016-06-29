
#  Predictions GBM --------------------------------------------------------

ntrees <- max(Model$trees.fitted)

# Test set predictions
TestPred <- predict(object = Model,
                    newdata = test,
                    n.trees = ntrees,
                    type = "response")

AUC::auc(AUC::roc(TestPred, as.factor(test$STATUS))) # 0.7839955 

TrainPred <- predict(object = Model,
                     newdata = train,
                     n.trees = ntrees,
                     type = "response")

AUC::auc(AUC::roc(TrainPred, as.factor(train$STATUS))) # 0.8052404

#   -------------------------------------------------------------------------

# To uncomment use ctrl shift c
# # XGBoost Predictions -----------------------------------------------------
# 
# test$pred <- predict(mod, data.matrix(test[, feature.names]))
# 
# plot(AUC::roc(test$pred, as.factor(test$STATUS)))
# 
# AUC::auc(AUC::roc(test$pred, as.factor(test$STATUS))) # 0.7622988
# AUC::auc(AUC::roc(round(test$pred), as.factor(test$STATUS))) # 0.7622988

#   -----------------------------------------------------------------------

# To uncomment use ctrl shift c
# # Random Testing using file as input --------------------------------------
# 
# Testrrr <- fread(paste(Path, "/Data/Tester/Lead1AllAgents.csv", sep = ""),
#                  header      =  TRUE,
#                  skip        = 0,
#                  stringsAsFactors = TRUE)
# Testrrr <- as.data.frame(Testrrr)
# 
# for (f in feature.names) {
#   if (class(Testrrr[[f]]) == "character") {
#     levels <- unique(Testrrr[[f]]) # Combines all possible factors (both from train and test)
#     Testrrr[[f]] <- factor(Testrrr[[f]], levels = levels)
#   }
#   else if (class(Testrrr[[f]]) == "integer" | class(Testrrr[[f]]) == "numeric"){
#     Testrrr[[f]] <- ifelse(is.na(Testrrr[[f]]), 0, Testrrr[[f]])   # If NA then
#   }
# }
# 
# TestrrrPred <- predict(object = Model,
#                        newdata = Testrrr,
#                        n.trees = gbm.perf(Model),
#                        type = "response")
# 
# Testrrr$pred <- TestrrrPred
# hist(TestrrrPred, breaks = 100)
























