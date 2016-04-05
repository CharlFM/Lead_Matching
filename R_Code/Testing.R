
#  Predictions GBM --------------------------------------------------------
# Test set predictions
TestPred <- predict(object = Model,
                    newdata = test,
                    n.trees = gbm.perf(Model),
                    type = "response")

AUC::auc(AUC::roc(TestPred, as.factor(Outcome_te))) # 0.7266963

TrainPred <- predict(object = Model,
                     newdata = train,
                     n.trees = gbm.perf(Model),
                     type = "response")

AUC::auc(AUC::roc(TrainPred, as.factor(Outcome_tr))) # 0.7339922

# Adding original Predicted values and finding optimal allocations

train$Pred <- TrainPred
test$Pred <- TestPred

TotPred <- rbind(subset(test, select = -ID), train)

ByAgent <- TotPred %>% 
  group_by(ZLAGENT) %>%
  top_n(1, wt = Pred)

#   -----------------------------------------------------------------------

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
























