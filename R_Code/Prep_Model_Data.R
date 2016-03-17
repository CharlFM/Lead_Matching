####################################################################################################################
# Preparing the model Data #
############################

DB_DAT$random <- runif(nrow(DB_DAT))

train <- DB_DAT[DB_DAT$random <= 0.7, ] 
test  <- DB_DAT[DB_DAT$random >  0.7, ] 

train <- subset(train, select = -c(random, INCEPTIONDATE, CLIENTBIRTHDATE, CLIENTBANKNAME, CLIENTBANKACCOUNTTYPE, CLIENTBANKBRANCH))
test  <- subset(test,  select = -c(random, INCEPTIONDATE, CLIENTBIRTHDATE, CLIENTBANKNAME, CLIENTBANKACCOUNTTYPE, CLIENTBANKBRANCH))

response.names <- "STATUS"

feature.names <- colnames(train)
feature.names <- feature.names[feature.names != response.names]

# put IDs for text variables - replace missings by 0 - boosting runs faster
for (f in feature.names) {
  if (class(train[[f]]) == "character") {
    levels <- unique(c(train[[f]], test[[f]])) # Combines all possible factors (both from train and test)
    train[[f]] <- factor(train[[f]], levels = levels)
    test[[f]]  <- factor(test[[f]],  levels = levels)
    # train[[f]] <- ifelse(is.na(train[[f]]), 0, as.integer(factor(train[[f]], levels = levels)))   # Create a numeric ID
    # test[[f]]  <- ifelse(is.na(test[[f]]),  0, as.integer(factor(test[[f]],  levels = levels)))   # instead of a character ID (If NA then 0)
  }
  else if (class(train[[f]]) == "integer" | class(train[[f]]) == "numeric"){
    train[[f]] <- ifelse(is.na(train[[f]]), 0, train[[f]])   # If NA then
    test[[f]]  <- ifelse(is.na(test[[f]]),  0, test[[f]])    # 0
  }
}

Outcome_tr <- train$STATUS
train <- subset(train, select = -STATUS)
Outcome_te <- test$STATUS
test <- subset(test, select = -STATUS)

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

# Look at the last model built
# Relative influence among the variables can be used in variable selection
summary(Model)

# If you see one variable that's much more important than all of the rest, that could be evidence of overfitting

# Optimal number of trees based on CV
gbm.perf(Model)

# Look at the effects of each variable, does it make sense?
for(i in 1:length(Model$var.names)) {
  plot(Model, 
       i.var   =  i,
       ntrees  =  gbm.perf(Model, plot.it = FALSE),
       type    =  "response")
}

# Make predictions













