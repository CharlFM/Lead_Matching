# Prep Data GBM -----------------------------------------------------------

DB_DAT$random <- runif(nrow(DB_DAT))

DB_DAT <- subset(DB_DAT, select = -c(CLIENTIDNUMBER, 
                                     LEADPICKUPDATE, INCEPTIONDATE, CLIENTBIRTHDATE, 
                                     CLIENTBANKNAME, CLIENTBANKACCOUNTTYPE, CLIENTBANKBRANCH))

# Remove columns with duplicated values (pointless to add these in the model)
DB_DAT <- DB_DAT[sapply(DB_DAT, function(x) length(unique(x)) > 1)]

train <- DB_DAT[DB_DAT$random <= 0.7, ] # 1 if satisfied with model
test  <- DB_DAT[DB_DAT$random >  0.7, ] # 1 if satisfied with model

train <- subset(train, select = -random)
test  <- subset(test,  select = -random)

response.names <- "STATUS"

feature.names <- colnames(train)
feature.names <- feature.names[feature.names != response.names]

# put IDs for text variables - replace missings by 0 - boosting runs faster
for (f in feature.names) {
  if (class(train[[f]]) == "character") {
    levels <- unique(c(train[[f]], test[[f]])) # Combines all possible factors (both from train and test)
    train[[f]] <- factor(train[[f]], levels = levels)
    test[[f]]  <- factor(test[[f]],  levels = levels)
  }
  else if (class(train[[f]]) == "integer" | class(train[[f]]) == "numeric"){
    train[[f]] <- ifelse(is.na(train[[f]]), 0, train[[f]])   # If NA then
    test[[f]]  <- ifelse(is.na(test[[f]]),  0, test[[f]])    # 0
  }
}

resp  <- which(names(train) %in% response.names)
feats <- seq(1:ncol(train))
feats <- feats[feats != resp]

# Outcome_tr <- train$STATUS
# train <- subset(train, select = -STATUS)
# Outcome_te <- test$STATUS
# test <- subset(test, select = -STATUS)

# To uncomment use ctrl shift c
# # Prep Data XGBoost -----------------------------------------------------------------
# 
# train <- DB_DAT[DB_DAT$random <= 0.7, ] 
# test  <- DB_DAT[DB_DAT$random >  0.7, ] 
# 
# train <- subset(train, select = -c(random, INCEPTIONDATE, CLIENTBIRTHDATE, CLIENTBANKNAME, CLIENTBANKACCOUNTTYPE, CLIENTBANKBRANCH))
# test  <- subset(test,  select = -c(random, INCEPTIONDATE, CLIENTBIRTHDATE, CLIENTBANKNAME, CLIENTBANKACCOUNTTYPE, CLIENTBANKBRANCH))
# 
# response.names <- "STATUS"
# 
# feature.names <- colnames(train)
# feature.names <- feature.names[feature.names != response.names]
# 
# # put IDs for text variables - replace missings by 0 - boosting runs faster
# for (f in feature.names) {
#   if (class(train[[f]]) == "character") {
#     levels <- unique(c(train[[f]], test[[f]])) # Combines all possible factors (both from train and test)
#     train[[f]] <- ifelse(is.na(train[[f]]), 0, as.integer(factor(train[[f]], levels = levels)))   # Create a numeric ID
#     test[[f]]  <- ifelse(is.na(test[[f]]),  0, as.integer(factor(test[[f]],  levels = levels)))   # instead of a character ID (If NA then 0)
#   }
#   else if (class(train[[f]]) == "integer" | class(train[[f]]) == "numeric"){
#     train[[f]] <- ifelse(is.na(train[[f]]), 0, train[[f]])   # If NA then
#     test[[f]]  <- ifelse(is.na(test[[f]]),  0, test[[f]])    # 0
#   }
# }
# 
# tra <- train[, feature.names]
# 
# h <- base::sample(nrow(train), 0.2 * nrow(train)) # Takes a sample of 20% of the training data
# 
# # Split training data into 2 datasets
# dval <- xgb.DMatrix(data = data.matrix(tra[h,]), 
#                     label = train$STATUS[h])
# dtrain <- xgb.DMatrix(data = data.matrix(tra[-h,]), 
#                       label = train$STATUS[-h])
#  











