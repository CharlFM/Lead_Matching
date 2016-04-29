# Open Connection to DB ---------------------------------------------------
# Load Current model and start allocation script
load(paste(getwd(), "/Active_Model.RData", sep = ""))

# Needs to be loaded once
source(paste(Path, "/R_Code/Load_Other_Data.R", sep = ""))

source(paste(Path, "/R_Code/OpenDB.R", sep = ""))
today <- as.character(Sys.Date())

# Get all active agents ---------------------------------------------------
agentQuery <- paste("SELECT * ",
                    "FROM agents ",
                    "WHERE `Active` = 'YES'",
                    sep = "")

activeAgent_Dat <- dbGetQuery(my_new_db, agentQuery)
activeAgent_Dat$FullName <- paste(activeAgent_Dat$AgentName, activeAgent_Dat$AgentSurname)

countQuery <- paste("SELECT * ",
                    "FROM AccessLife_Sales_File_Lead_Data ",
                    "WHERE `Status` = 'Allocated' AND `First Allocation Date` > '2016-04-20'", # Update Allocation date to when model goes live
                    sep = "")

count_Dat <- dbGetQuery(mydb, countQuery)
colnames(count_Dat) <- gsub(" ", "", colnames(count_Dat))

# Load New Data - if available --------------------------------------------
loadNewDataQuery <- paste("SELECT value ",
                          "FROM system ",
                          "WHERE description = 'newData'", 
                          sep = "")
loadNewData <- dbGetQuery(my_new_db, loadNewDataQuery)

if (as.numeric(loadNewData) == 1) {
  
  Recycle <- 0
  
  dbSendQuery(my_new_db, "UPDATE system SET value = '0' WHERE description = 'newData'")
  
  newQuery <- paste("SELECT client.*, company.* , asset.* , insuranceProduct.* , clientBank.* , dealership.* ",
                    "FROM insuranceProduct ",
                    "LEFT JOIN asset ON insuranceProduct.VINNUMBER = asset.VINNUMBER ",
                    "LEFT JOIN client ON asset.CLIENTIDNUMBER = client.CLIENTIDNUMBER ",
                    "LEFT JOIN company ON asset.COMPANYREGISTRATIONNUMBER = company.COMPANYREGISTRATIONNUMBER ",
                    "LEFT JOIN clientBank ON asset.CLIENTBANKACCNO = clientBank.CLIENTBANKACCNO AND asset.CLIENTBANKNAME = clientBank.CLIENTBANKNAME ",
                    "LEFT JOIN dealership ON dealership.BRANCHCODE = asset.BRANCHCODE ",
                    "WHERE NEWLEAD = 1",
                    sep = "") 
  
  newLead_Dat <- dbGetQuery(my_new_db, newQuery)
  newLead_Dat <- newLead_Dat[, !duplicated(colnames(newLead_Dat))]
  
  newLead_Dat$ID   <- 1:nrow(newLead_Dat)
  newLead_Dat_Orig <- newLead_Dat
  
}


if (nrow(newLead_Dat) == 0) {
  
  Recycle <- 1
  
  query <- paste("SELECT * ",
                 "FROM AccessLife_Sales_File_Lead_Data ",
                 "WHERE `Status` = 'Allocated' ",
                 "AND `First Allocation Date` BETWEEN '", as.Date(Sys.Date()) - months(7), "' AND '", as.Date(Sys.Date()) - months(1),"' ",
                 "AND `Lead Date` <> '",today ,"' ",
                 "AND `UW Status` IS NULL AND `QA Status` IS NULL AND Affinity <> 'Auto Pedigree' AND ZwingMaster <> 'Douglas Gwanyanya'",
                 sep = "")
  
  newLead_Dat <- dbGetQuery(mydb, query)
  
}

# Clean Data --------------------------------------------------------------

source(paste(Path, "/R_Code/Append_Sort.R", sep = ""))


# Determine which agents needs leads --------------------------------------
# NEEDS TO CALC EACH SUNDAY NIGHT TO DETERMINE MAX NUMBER OF LEADS PER AGENT -> Update in table
countAgents <- count_Dat %>% 
                  group_by(ZwingMaster) %>%
                  summarize(n = n())
countAgents$ZwingMaster <- as.character(countAgents$ZwingMaster)

countAgents <- merge(countAgents, activeAgent_Dat, by.x = "ZwingMaster", by.y = "FullName", all.y = TRUE)
countAgents$n[is.na(countAgents$n)] <- 0

total_available <- nrow(newLead_Dat)
allocation_size <- 5

countAgents$n[countAgents$n > 5] <- 5 ############# Temp ################### 

for (i in 1:nrow(countAgents)) {
  if (total_available > 0) {
    
    if (total_available < 5) {
      allocation_size <- total_available
    }
    
    alloc                 <- allocation_size - countAgents$n[i]
    countAgents$topup[i]  <- alloc
    total_available       <- total_available - alloc
    
  }
}

# Top-Up leads ------------------------------------------------------------
SubsetData <- newLead_Dat

countAgents$ZM <- gsub(" ", "", gsub("[^[:alpha:] ]", "", toupper(countAgents$ZwingMaster)))

ntrees <- max(Model$trees.fitted)

LeadOut <- data.frame(ZLAGENT = as.character(),
                      Pred    = as.numeric(),
                      ID      = as.integer())

for (i in 1:nrow(countAgents)) {
  
  topup <- countAgents$topup[countAgents$ZM == countAgents$ZM[i]]
  
  if (topup > 0) {
    
    SubsetData$ZLAGENT <- countAgents$ZM[i]
    SubsetData$ZLAGENT <- as.factor(SubsetData$ZLAGENT)
    
    Preds <- predict(object   =  Model,
                     newdata  =  SubsetData,
                     n.trees  =  ntrees,
                     type     =  "response")
    SubsetData$Pred <- Preds
    
    SubsetData <- SubsetData %>%
      arrange(desc(Pred))
    
    MaxID <- SubsetData$ID[1:topup]
    
    UpdateR <- data.frame(ZLAGENT = countAgents$ZwingMaster[which(countAgents$ZM %in% countAgents$ZM[i])],
                          Pred    = SubsetData$Pred[SubsetData$ID == MaxID],
                          ID      = MaxID)
    
    LeadOut <- rbind(LeadOut, UpdateR)
    
    SubsetData <- SubsetData[SubsetData$ID != MaxID, ]
  }
  
  print(paste(i, "of", nrow(countAgents)))
  
}

newLead_Dat2 <- merge(newLead_Dat_Orig, LeadOut, by.x = "ID", by.y = "ID")
newLead_Dat2 <- subset(newLead_Dat2, select = -ZLAGENT.x)
colnames(newLead_Dat2)[colnames(newLead_Dat2) == "ZLAGENT.y"] <- "ZLAGENT"

newLead_Dat2$ZLAGENT              <-  as.character(newLead_Dat2$ZLAGENT)
newLead_Dat2$FIRSTALLOCATIONDATE  <-  today
newLead_Dat2$LEADDATE             <-  today
newLead_Dat2$STATUS               <-  "Allocated"
newLead_Dat2$STATICON             <-  "Red1.png"

newLead_Dat_Orig <- newLead_Dat_Orig[!(newLead_Dat_Orig$ID %in% newLead_Dat2$ID), ]
newLead_Dat      <- newLead_Dat[!(newLead_Dat$ID %in% newLead_Dat2$ID), ]

# Update DB's -------------------------------------------------------------

for (i in 1:nrow(newLead_Dat2)) {
  
  if (Recycle == 0) {
    
    updateQuery <- paste("UPDATE asset ",
                         "SET ZLAGENT = '", newLead_Dat2$ZLAGENT[i],"', ",
                         "NEWLEAD = '0', ",
                         "STATUS = 'Allocated', ",
                         "FIRSTALLOCATIONDATE = '", today, "', ",
                         "LEADDATE = '", today ,"' ",
                         "WHERE VINNUMBER = '", newLead_Dat2$VINNUMBER[i], "'",
                         sep = "")
    dbSendQuery(my_new_db, updateQuery)
    
    InsertValues <- newLead_Dat2[i, c("CLIENTTITLE", "CLIENTFIRSTNAME", "CLIENTLASTNAME", "CLIENTIDNUMBER", "CLIENTBIRTHDATE", "MARITALSTATUS", "CLIENTPOSTALADDRESS1", "CLIENTPOSTALADDRESS2", "CLIENTPOSTALADDRESS3", "CLIENTPOSTALADDRESS4", "CLIENTPOSTALADDRESSPOSTALCODE", "CLIENTEMAILADDRESS", "CLIENTOCCUPATIONNAME", "CLIENTWORKTELEPHONENUMBER", "CLIENTHOMETELEPHONENUMBER", "CLIENTMOBILENUMBER", "SALESPERSON", "VEHICLEVALUE", "FINANCETERM", "DEPOSITVALUE", "INCEPTIONDATE", "RESIDUALVALUE", "FINANCEAMOUNT", "MODEL", "FIRSTREGISTRATIONYEAR", "ACCESSORIES", "VEHICLEUSE", "ODOMETERREADING", "DOCINSURANCECOMPANYNAME", "REGISTRATIONNUMBER", "BRANCHNAME", "DOCFINANCECOMPANYNAME", "CLIENTACCOUNTHOLDERNAME", "CLIENTBANKNAME", "CLIENTBANKBRANCH", "CLIENTBANKBRANCHCODE", "CLIENTBANKACCNO", "CLIENTBANKACCOUNTTYPE", "TRANSACTIONNUMBER", "AFFINITY", "LEADDATE", "FIRSTALLOCATIONDATE", "ZLAGENT", "STATUS", "STATICON")]
    InsertValues <- paste(InsertValues, collapse = "','")
    InsertValues <- paste("'", InsertValues, "'", sep = "")  
    InsertValues <- gsub("NA", "", InsertValues)
    
    insertQuery <- paste("INSERT INTO AccessLife_Sales_File_Lead_Data ",
                         "(`Title`, `First Name`, `Surname`, `ID Number`, `Date of Birth`, `Marital Status`, `Postal Address 1`, `Postal Address 2`, `Postal Address 3`, `Postal Address 4`, `Postal Address Code`, `E-Mail Address`, `Occupation`, `Work Tel`, `Home Tel`, `Cellphone`, `Broker - Finance Consultant`, `Purchase Price`, `Term`, `Dep or Trade in`, `Purchase Date`, `Residual`, `Principal Debt`, `Vehicle Make`, `Year Model`, `Extras - Accesories`, `Vehicle Use`, `Odometer kms`, `Short Term Insurer`, `Registration No`, `Selling Dealer`, `Financing Bank`, `Account Name`, `Bank Name`, `Branch Name`, `Branch Code`, `Account Number`, `Account Type`, `Platform Number`, `Affinity`, `Lead Date`, `First Allocation Date`, `ZwingMaster`, `Status`, `Staticon`) ",
                         "VALUES (", InsertValues, ")",
                         sep = "")
    dbSendQuery(mydb, insertQuery)
    
  } else { 
    updateQuery <- paste("UPDATE AccessLife_Sales_File_Lead_Data ",
                         "SET ZwingMaster = '", newLead_Dat2$ZLAGENT[i],"', `Lead Date` = '", today ,"' ",
                         "WHERE AutoNumber = '", newLead_Dat2$LEADNUMBER[i], "'",
                         sep = "")
    dbSendQuery(mydb, updateQuery)
  }
  
  print(i)
  
}






















