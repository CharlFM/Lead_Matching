# Insert to DB ---------------------------------------------------------------
LeadsGoingIn <- merge(ManLead_DatOrig, LeadOut, 
                         by.x = "ID", by.y = "ID", all.y = TRUE) 

d_time <- gsub(" ", "_", Sys.time())
d_time <- gsub("-", "_", d_time)
d_time <- gsub(":", "_", d_time)

LeadsGoingIn$COMCAT <- ifelse(grepl("BARLO", gsub(" ", "", toupper(LeadsGoingIn$AFFINITY))), "E", "A")
LeadsGoingIn$CLIENTBIRTHDATE <- DateConv(LeadsGoingIn$CLIENTBIRTHDATE)
LeadsGoingIn$INCEPTIONDATE <- DateConv(LeadsGoingIn$INCEPTIONDATE)

LeadsGoingIn <- LeadsGoingIn %>% arrange(desc(Pred))

# Remove single quotes
del <- plyr::colwise(function(x) str_replace_all(x, "'", ""))
LeadsGoingIn <- del(LeadsGoingIn)

# Insert
for (i in 1:nrow(LeadsGoingIn)) {
  
  vals = paste(LeadsGoingIn$CLIENTTITLE[i], LeadsGoingIn$CLIENTFIRSTNAME[i], LeadsGoingIn$CLIENTLASTNAME[i], 
               LeadsGoingIn$CLIENTIDNUMBER[i], LeadsGoingIn$CLIENTBIRTHDATE[i], LeadsGoingIn$MARITALSTATUS[i],
               LeadsGoingIn$CLIENTPOSTALADDRESS1[i], LeadsGoingIn$CLIENTPOSTALADDRESS2[i], LeadsGoingIn$CLIENTPOSTALADDRESS3[i],
               LeadsGoingIn$CLIENTPOSTALADDRESS4[i], LeadsGoingIn$CLIENTPOSTALADDRESSPOSTALCODE[i],
               LeadsGoingIn$CLIENTOCCUPATIONNAME[i], LeadsGoingIn$CLIENTWORKTELEPHONENUMBER[i], 
               LeadsGoingIn$CLIENTHOMETELEPHONENUMBER[i], LeadsGoingIn$CLIENTMOBILENUMBER[i], LeadsGoingIn$SALESPERSON[i], 
               LeadsGoingIn$VEHICLEVALUE[i], LeadsGoingIn$FINANCETERM[i], LeadsGoingIn$DEPOSITVALUE[i], 
               LeadsGoingIn$INCEPTIONDATE[i], LeadsGoingIn$RESIDUALVALUE[i], LeadsGoingIn$FINANCEAMOUNT[i], 
               LeadsGoingIn$MODEL[i], LeadsGoingIn$FIRSTREGISTRATIONYEAR[i], LeadsGoingIn$VEHICLEUSE[i], 
               LeadsGoingIn$ODOMETERREADING[i], LeadsGoingIn$DOCINSURANCECOMPANYNAME[i], LeadsGoingIn$REGISTRATIONNUMBER[i], 
               "AccessLife", LeadsGoingIn$ZLAGENT[i], "Red1.png", "Allocated", LeadsGoingIn$COMCAT[i], 
               LeadsGoingIn$BRANCHNAME[i], LeadsGoingIn$AFFINITY[i], LeadsGoingIn$DOCFINANCECOMPANYNAME[i], today, 
               LeadsGoingIn$TRANSACTIONNUMBER[i], today, LeadsGoingIn$Pred[i], LeadsGoingIn$PRODUCTTYPECATEGORYNAME[i], 
               sep = "', '")
  
  vals = paste("'", vals, "'", sep = "") 
  
  insertQuery <- paste("INSERT INTO AccessLife_Sales_File_Lead_Data ",
                       "(`Title`,`First name`, `surname`, `id number`, `Date of Birth`, `Marital status`, ",
                       "`Postal Address 1`, `Postal Address 2`, `Postal Address 3`, `Postal Address 4`, `Postal Address Code`, ",
                       "`Occupation`, `Work Tel`, `Home Tel`, `Cellphone`, `Broker - Finance Consultant`, ", 
                       "`Purchase price`, `Term`, `Dep or Trade in`, `Purchase Date`, `Residual`, `Principal Debt`, ", 
                       "`Vehicle Make`, `year Model`, `Vehicle use`, `Odometer kms`, `Short Term Insurer`, ", 
                       "`Registration No`, `Campaign`, `ZwingMaster`, `Staticon`, `Status`, `COMCat`, `Selling Dealer`, ",
                       "`Affinity`, `Financing Bank`, `Lead Date`, `Platform Number` , `First Allocation Date`, `gbm_Pred`,", 
                       "`retrenchment_allowed`) ",
                       "VALUES(", vals, ")",
                       sep = "")
  
  dbSendQuery(mydb, insertQuery)
  
  # if (i == 1) {
  #   AllQueries <- insertQuery
  # } else {
  #   AllQueries <- paste(AllQueries, insertQuery, sep = ";")
  # }
  
}

# dbSendQuery(mydb, AllQueries)
# 
# 
# 1 048 576 > 170 368
# 
# object.size(AllQueries)
# 










