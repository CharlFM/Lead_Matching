

# Moddeling Using GBM -----------------------------------------------------

Model <- gbm.step(data               =  train,                     # The data
                  
                  gbm.y              =  resp,                      # Dependent variable name
                  
                  gbm.x              =  feats,                     # Feature names
                  
                  family             =  "bernoulli",               # Alternative for binary outcome : "adaboost"
                  
                  n.trees            =  50,                        # Keep it large - then we prune afterwards
                  
                  learning.rate      =  0.01,                      # The smaller, the better - tradeoff = slower
                  
                  tree.complexity    =  3,                         # 1: additive model, 2: two-way interactions, etc.
                                                                   # depth 3 means each tree will evaluate three decisions;
                                                                   #         will always yield [3 * depth + 1] nodes and [2 * depth + 1] terminal  
                                                                   #         nodes (depth 3 = 7)  
                                                                   #         because each decision yields 3 nodes, 
                                                                   #         but each decision will come from a prior node
                  
                  max.trees          = 5000,                       # max number of trees to fit before stopping
                  
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




