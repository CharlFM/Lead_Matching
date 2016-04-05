
# Look at the last model built
# Relative influence among the variables can be used in variable selection
summary(Model)

# If you see one variable that's much more important than all of the rest, that could be evidence of overfitting

# Optimal number of trees based on CV
gbm.perf(Model)

# report(Model, "GBM", "GBM_Model") <- Check this out...

# Look at the effects of each variable, does it make sense?
for(i in 1:length(Model$var.names)) {
  plot(Model, 
       i.var   =  i,
       ntrees  =  gbm.perf(Model, plot.it = FALSE),
       type    =  "response")
}

i <- 6 # 32 total
VarInf <- plot(Model, 
               i.var   =  i,
               ntrees  =  gbm.perf(Model, plot.it = FALSE),
               type    =  "response")

DatOut <- as.data.frame(cbind(VarInf$names, VarInf$stats[1,]))
colnames(DatOut) <- c(Model$var.names[i], "P(Sale)")

# To uncomment use ctrl shift c
# # Per Agent Analysis ------------------------------------------------------
# 
# All_Agents <- unique(DB_DAT$ZLAGENT)
# 
# orig_rows <- nrow(test)
# per_agent <- floor(orig_rows/length(All_Agents))
# test$ID <- seq(1:nrow(test))
# agents_w_counts <- data.frame(Agents = All_Agents)
# agents_w_counts$Counts <- 0
# 
# test3 <- test
# 
# for (i in 1:length(All_Agents)){
#   
#   test2 <- test3
#   test2 <- test2[rep(seq_len(nrow(test2)), each = length(All_Agents)), ]
#   
#   All_Agents <- rep(All_Agents, nrow(test3))
#   test2$ZLAGENT <- as.factor(All_Agents)
#   
#   TestPred2 <- predict(object = Model,
#                        newdata = test2,
#                        n.trees = gbm.perf(Model),
#                        type = "response")
#   
#   test2$Pred2 <- TestPred2
#   
#   test2 <- test2[with(test2, order(-Pred2)), ]
#   
#   test2 <- test2[!duplicated(test2$ID),]  
#   
#   tempfinDF <- test2 %>% 
#     group_by(ZLAGENT) %>%
#     top_n(per_agent, wt = Pred2)
#   
#   ff <- data.frame(table(tempfinDF$ZLAGENT))
#   ff <- ff[ff$Freq != 0, ]
#   
#   agents_w_counts <- merge(agents_w_counts, ff, by.x = "Agents", by.y = "Var1", all.x = TRUE)
#   agents_w_counts$Freq[is.na(agents_w_counts$Freq)] <- 0
#   agents_w_counts$Counts <- agents_w_counts$Counts + agents_w_counts$Freq
#   agents_w_counts <- subset(agents_w_counts, select = -Freq)
#   
#   rem_agents <- agents_w_counts$Agents[agents_w_counts$Counts >= per_agent]
#   
#   All_Agents <- unique(DB_DAT$ZLAGENT)[!(unique(DB_DAT$ZLAGENT) %in% rem_agents)]
#   
#   test3 <- test2[!(test2$ID %in% tempfinDF$ID), ]
#   test3 <- subset(test3, select = -Pred2)
#   
#   if (i == 1) {
#     finDF <- tempfinDF
#   } else {
#     finDF <- rbind(finDF, tempfinDF)
#   }
#   
#   print(i)
#   
# }
# 
# ff <- data.frame(table(finDF$ZLAGENT))
# 
# finDF$PredInc <- finDF$Pred2 / finDF$Pred
# 
# mean(finDF$Pred)   # 0.2341809
# mean(finDF$Pred2)  # min incr = 0.2638592 ; max incr = 0.5225639
# 
# mean(finDF$Pred2)/mean(finDF$Pred) # min incr = 13.3608 % ; max incr = 223.1454%

#   -----------------------------------------------------------------------

# To uncomment use ctrl shift c
# # Decision Trees (For Illustration Only) ----------------------------------
# 
# Small_Dat <- cbind(train, Outcome_tr) 
# 
# TreeFit <- ctree(as.factor(Outcome_tr) ~ CLIENTAGE + VEHICLEVALUE,
#                  data = Small_Dat)
# plot(TreeFit)

#   -----------------------------------------------------------------------









