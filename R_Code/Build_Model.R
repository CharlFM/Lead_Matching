
# Moddeling Using GBM -----------------------------------------------------

Model <- gbm.fit(y                  =  Outcome_tr,                # Dependent variable
                 x                  =  train,                     # Dataframe with features
                 distribution       =  "bernoulli",               # Alternative for binary outcome : "adaboost"
                 n.trees            =  5000,                      # Keep it large - then we prune afterwards
                 shrinkage          =  0.01,                      # The smaller, the better - tradeoff = slower
                 interaction.depth  =  3,                         # Use the results from cross validation to choose interaction depth
                 n.minobsinnode     =  10,                        # Effect on overfitting - decreasing this parameter increases the in-sample fit 
                                                                  #                       - can result in overfitting
                 nTrain             =  round(nrow(train) * 0.8),  # To select the number of trees at the end 
                 verbose            =  TRUE)                      # Print preliminary output

# To uncomment use ctrl shift c
# # Moddeling Using XGBoost -------------------------------------------------
# 
# watchlist <- list(val = dval, train = dtrain)
# 
# param <- list(  objective           = "binary:logistic", 
#                 booster             = "gbtree",
#                 eta                 = 0.01,
#                 max_depth           = 8,
#                 subsample           = 0.7,
#                 colsample_bytree    = 0.7
# )
# 
# mod <- xgb.train(   params              = param, 
#                     data                = dtrain, 
#                     nrounds             = 5000,
#                     verbose             = 1,
#                     early.stop.round    = 30,
#                     watchlist           = watchlist,
#                     maximize            = TRUE,
#                     eval.metric         = "auc"        
# )




