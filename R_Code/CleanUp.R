# Get First Registration Year ---------------------------------------------

# Diff between car model year and purchase year
YearDf <- strsplit(UniDash(All_lead_Data$FIRSTREGISTRATIONYEAR), "-")

n <- max(sapply(YearDf, length))
l <- lapply(YearDf, function(X) c(X, rep(NA, n - length(X))))

YearDf <- data.frame(t(do.call(cbind, l)), stringsAsFactors = FALSE)
colnames(YearDf) <- paste("Year", seq(1:ncol(YearDf)), sep = "")

yyFin <- YearDf[[1]]

for (i in 1:ncol(YearDf)) {
  
  yyPrt                    <- as.numeric(YearDf[[i]])
  yyPrt[nchar(yyPrt) != 4] <- NA
  
  yyFin[!is.na(yyPrt)] <- yyPrt[!is.na(yyPrt)]
  
}

All_lead_Data$FIRSTREGISTRATIONYEAR <- as.numeric(yyFin)

# Clean Dates -------------------------------------------------------------

DateFields <- c("INCEPTIONDATE", "CLIENTBIRTHDATE", "COMMENCEMENTDATE", "EXPIRYDATE", "FIRSTDEBITDATE")

for (f in DateFields) {
  All_lead_Data[[f]]     <-  DateConv(All_lead_Data[[f]])
}

# Add Race and Culture info -----------------------------------------------
All_lead_Data$CLIENTSURNAME <- gsub(" ","", gsub("[^[:alpha:] ]", "", toupper(All_lead_Data$CLIENTLASTNAME)))

All_lead_Data <- merge(All_lead_Data, Race_Data, by.x = "CLIENTSURNAME", by.y = "SURNAME", all.x = TRUE)

colnames(All_lead_Data)[colnames(All_lead_Data) == "RACE"]    <- "RACE_SN"
colnames(All_lead_Data)[colnames(All_lead_Data) == "CULTURE"] <- "CULTURE_SN"

All_lead_Data$CLIENTNAME <- gsub(" ","", gsub("[^[:alpha:] ]", "", toupper(All_lead_Data$CLIENTFIRSTNAME)))

All_lead_Data <- merge(All_lead_Data, Race_Data, by.x = "CLIENTNAME", by.y = "SURNAME", all.x = TRUE)

colnames(All_lead_Data)[colnames(All_lead_Data) == "RACE"]    <- "RACE_FN"
colnames(All_lead_Data)[colnames(All_lead_Data) == "CULTURE"] <- "CULTURE_FN"

All_lead_Data$RACE_SN[is.na(All_lead_Data$RACE_SN)]       <- All_lead_Data$RACE_FN[is.na(All_lead_Data$RACE_SN)]
All_lead_Data$CULTURE_SN[is.na(All_lead_Data$CULTURE_SN)] <- All_lead_Data$RACE_FN[is.na(All_lead_Data$CULTURE_SN)]

colnames(All_lead_Data)[colnames(All_lead_Data) == "RACE_SN"]    <- "RACE"
colnames(All_lead_Data)[colnames(All_lead_Data) == "CULTURE_SN"] <- "CULTURE"

All_lead_Data$RACE[All_lead_Data$RACE == ""]       <- NA
All_lead_Data$CULTURE[All_lead_Data$CULTURE == ""] <- NA

All_lead_Data <- subset(All_lead_Data, select = -c(CLIENTNAME, CLIENTSURNAME, RACE_FN, CULTURE_FN))

# Cleans Title ------------------------------------------------------------

All_lead_Data$CLIENTTITLE <- gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(All_lead_Data$CLIENTTITLE)))

All_lead_Data$CLIENTTITLE[grepl("REV", All_lead_Data$CLIENTTITLE)]  <-  "REV"
All_lead_Data$CLIENTTITLE[grepl("PROF", All_lead_Data$CLIENTTITLE)] <-  "PROF"
All_lead_Data$CLIENTTITLE[grepl("ADV", All_lead_Data$CLIENTTITLE)]  <-  "ADV"
All_lead_Data$CLIENTTITLE[grepl("DOC", All_lead_Data$CLIENTTITLE)]  <-  "DR"
All_lead_Data$CLIENTTITLE[grepl("MNR", All_lead_Data$CLIENTTITLE)]  <-  "MR"
All_lead_Data$CLIENTTITLE[grepl("MEV", All_lead_Data$CLIENTTITLE)]  <-  "MRS"
All_lead_Data$CLIENTTITLE[grepl("MEJ", All_lead_Data$CLIENTTITLE)]  <-  "MISS"

All_lead_Data$CLIENTTITLE[!(All_lead_Data$CLIENTTITLE %in% c("REV", "PROF", "ADV", "DR", "MR", "MRS", "MS", "MISS"))]  <- NA

# Marital Status ----------------------------------------------------------

All_lead_Data$MARITALSTATUS <- gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(All_lead_Data$MARITALSTATUS)))

All_lead_Data$MARITALSTATUS[grepl("SIN", All_lead_Data$MARITALSTATUS)]  <-  "SINGLE"
All_lead_Data$MARITALSTATUS[grepl("ENG", All_lead_Data$MARITALSTATUS)]  <-  "ENGAGED"
All_lead_Data$MARITALSTATUS[grepl("MAR", All_lead_Data$MARITALSTATUS)]  <-  "MARRIED"
All_lead_Data$MARITALSTATUS[grepl("SEP", All_lead_Data$MARITALSTATUS)]  <-  "DIVORCED"
All_lead_Data$MARITALSTATUS[grepl("DIV", All_lead_Data$MARITALSTATUS)]  <-  "DIVORCED"
All_lead_Data$MARITALSTATUS[grepl("WID", All_lead_Data$MARITALSTATUS)]  <-  "WIDOW"

All_lead_Data$MARITALSTATUS[!(All_lead_Data$MARITALSTATUS %in% c("SINGLE", "ENGAGED", "MARRIED", "DIVORCED", "WIDOW"))]  <- NA

# Clean, append and check ID ----------------------------------------------

# CLIENTIDTYPE "OTHER ID" "RSA ID"
# Determine SA and NON SA ID
All_lead_Data$CLIENTIDNUMBER  <- gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(All_lead_Data$CLIENTIDNUMBER)))
All_lead_Data$CLIENTIDTYPE    <- "OTHER ID"

ValidOnCount <- CountAllNums(All_lead_Data$CLIENTIDNUMBER) == 13
ValidOnCount[is.na(ValidOnCount)] <- FALSE

# Parts of an SA ID:
YY  <- as.numeric(substr(All_lead_Data$CLIENTIDNUMBER,  1,  2)) # Year  of Birth (note that 1900 and 2000 will both be 00)
MM  <- as.numeric(substr(All_lead_Data$CLIENTIDNUMBER,  3,  4)) # Month of Birth
DD  <- as.numeric(substr(All_lead_Data$CLIENTIDNUMBER,  5,  6)) # Day   of Birth   
G   <- as.numeric(substr(All_lead_Data$CLIENTIDNUMBER,  7,  7)) # Gender ([0, 4] = Female and [5, 9] = Male)
SSS <- as.numeric(substr(All_lead_Data$CLIENTIDNUMBER,  8, 10)) # Birth registration number
C   <- as.numeric(substr(All_lead_Data$CLIENTIDNUMBER, 11, 11)) # SA citizen indicator (0 = SA, 1 = Not)
A   <- as.numeric(substr(All_lead_Data$CLIENTIDNUMBER, 12, 12)) # Number no longer used. Prior race indicator
Z   <- as.numeric(substr(All_lead_Data$CLIENTIDNUMBER, 13, 13)) # A checksum indicator se Luhn algorithm

YY [ValidOnCount == FALSE] <- NA
MM [ValidOnCount == FALSE] <- NA
DD [ValidOnCount == FALSE] <- NA
G  [ValidOnCount == FALSE] <- NA
SSS[ValidOnCount == FALSE] <- NA
C  [ValidOnCount == FALSE] <- NA
A  [ValidOnCount == FALSE] <- NA
Z  [ValidOnCount == FALSE] <- NA

Odd_Sum <- as.numeric(substr(All_lead_Data$CLIENTIDNUMBER, 1, 1)) + as.numeric(substr(All_lead_Data$CLIENTIDNUMBER, 3, 3)) + 
  as.numeric(substr(All_lead_Data$CLIENTIDNUMBER, 5, 5)) + as.numeric(substr(All_lead_Data$CLIENTIDNUMBER, 7, 7)) + 
  as.numeric(substr(All_lead_Data$CLIENTIDNUMBER, 9, 9)) + as.numeric(substr(All_lead_Data$CLIENTIDNUMBER, 11, 11))

Even_Sum <- sumSplitValues(as.character(
  2 * as.numeric(paste(substr(All_lead_Data$CLIENTIDNUMBER, 2, 2), substr(All_lead_Data$CLIENTIDNUMBER, 4, 4), 
                       substr(All_lead_Data$CLIENTIDNUMBER, 6, 6), substr(All_lead_Data$CLIENTIDNUMBER, 8, 8), 
                       substr(All_lead_Data$CLIENTIDNUMBER, 10, 10), substr(All_lead_Data$CLIENTIDNUMBER, 12, 12), sep = ""))))

LuhnVal <- 10 - (Odd_Sum + Even_Sum) %% 10
LuhnVal[LuhnVal == 10] <- 0

LuhnVal[ValidOnCount == FALSE] <- -999
LuhnVal[is.na(LuhnVal)]        <- -999

ValidOnMost <- (YY  %in% seq(0, 99)  &
                MM  %in% seq(1, 12)  &
                G   %in% seq(0, 10)  &
                SSS %in% seq(0, 999) &
                C   %in% c(0, 1)     &
                A   %in% seq(0, 9)   &
                Z    ==  LuhnVal)

ValidOnMost[ValidOnCount == FALSE] <- FALSE
ValidOnMost[is.na(ValidOnMost)]    <- FALSE

ValidOnAll <- (MM == 2 & DD <= 28 & YY %% 4 == 0 |
               MM == 2 & DD <= 29 & YY %% 4 != 0 |
               MM %in% c(4, 6, 9, 11) & DD <= 30 |
               MM %in% c(1, 3, 5, 7, 8, 10, 12) & DD <= 31)

ValidOnAll[ValidOnMost == FALSE] <- NA

YY[is.na(ValidOnAll)] <- NA
MM[is.na(ValidOnAll)] <- NA
DD[is.na(ValidOnAll)] <- NA
G[is.na(ValidOnAll)]  <- NA

All_lead_Data$CLIENTIDTYPE[ValidOnAll] <- "RSA ID"

# Clean Birthdate
Cur_Year <- as.numeric(substr(format(Sys.Date(), "%Y"), 3, 4))

B_Days <- data.frame(YYYY = ifelse(YY <= Cur_Year, as.numeric(paste(20, YY, sep = "")), as.numeric(paste(19, YY, sep = ""))),
                     MM   = MM,
                     DD   = DD)
B_Days <- DateConv(paste(B_Days$YYYY, B_Days$MM, B_Days$DD, sep = "-"))

Orig_B_Days <- DateConv(All_lead_Data$CLIENTBIRTHDATE)

All_lead_Data$CLIENTBIRTHDATE <- B_Days
All_lead_Data$CLIENTBIRTHDATE[is.na(All_lead_Data$CLIENTBIRTHDATE)] <- Orig_B_Days[is.na(All_lead_Data$CLIENTBIRTHDATE)]

# Clean Gender
Orig_Gen <- gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(All_lead_Data$CLIENTGENDER)))
Gender <- ifelse(G <= 4, "FEMALE", "MALE")
All_lead_Data$CLIENTGENDER <- Gender
All_lead_Data$CLIENTGENDER[is.na(All_lead_Data$CLIENTGENDER)] <- Orig_Gen[is.na(All_lead_Data$CLIENTGENDER)]

rm(YY, MM, DD, G, SSS, C, A, Z, LuhnVal, 
   ValidOnCount, ValidOnMost, ValidOnAll, 
   Orig_B_Days, B_Days, 
   Odd_Sum, Even_Sum,
   Gender, Orig_Gen)

# Get Age at purchase date
All_lead_Data$CLIENTBIRTHDATE[All_lead_Data$CLIENTBIRTHDATE == as.Date("1899-12-30")] <- NA
All_lead_Data$CLIENTAGE <- as.numeric(All_lead_Data$INCEPTIONDATE - All_lead_Data$CLIENTBIRTHDATE) / 365.25 

# Clean Contact Numbers ---------------------------------------------------

ContactFields <- c("BRANCHTELEPHONENUMBER", "BRANCHFAXNUMBER", 
                   "CLIENTMOBILENUMBER", "CLIENTWORKTELEPHONENUMBER", "CLIENTHOMETELEPHONENUMBER",
                   "BMWORKTELEPHONE", "BMMOBILENUMBER")

for (f in ContactFields) {
  All_lead_Data[[f]] <- as.numeric(gsub("[^0-9]", "", All_lead_Data[[f]]))
  All_lead_Data[[f]][!is.na(All_lead_Data[[f]]) & as.numeric(substr(All_lead_Data[[f]], 1, 2)) == 27] <- 
    substrRight(All_lead_Data[[f]][!is.na(All_lead_Data[[f]]) & as.numeric(substr(All_lead_Data[[f]], 1, 2)) == 27], 9)
  All_lead_Data[[f]][!is.na(All_lead_Data[[f]])] <- paste("0", substrRight(All_lead_Data[[f]][!is.na(All_lead_Data[[f]])], 9), sep = "")
  All_lead_Data[[f]][All_lead_Data[[f]] == 0] <- NA
}

# Clean Taken - Barlow ----------------------------------------------------

All_lead_Data$TAKEN[is.na(All_lead_Data$TAKEN)] <- 0

# Clean Numeric Fields ----------------------------------------------------

NumFields <- c("BRANCHPOADDRESSPOSTCODE", "BRANCHRESIDENTIALADDRESSPOSTCODE",
               "CLIENTPOSTALADDRESSPOSTALCODE", "CLIENTRESIDENTIALADDRESSPOSTALCODE",
               "COMPANYADDRESSPOSTCODE",
               "FIRSTREGISTRATIONYEAR", "ODOMETERREADING", 
               "VEHICLEVALUE", "RETAILPRICE", "DISCOUNTAMOUNT", "DEPOSITVALUE", "FINANCEAMOUNT", "RESIDUALVALUE",
               "REGISTRATIONFEE", "DELIVERYFEE", "ACCESSORYTOTAL",
               "PRODUCTAMOUNT", "PRODUCTVATAMOUNT", "PRODUCTTOTALAMOUNT", "PAYOVERAMOUNT", "PAYOVERVATAMOUNT", "PAYOVERTOTALAMOUNT",
               "COMMISSIONAMOUNT", "COLLECTIONFEE", "MONTHLYPREMIUM", "COVERAMOUNT", "TERM",
               "FIRSTDEBITAMOUNT", "RECURRINGDEBITAMOUNT", "FINANCETERM",
               "INTERESTRATE",
               "ADMINFEE", "BINDERFEE", "DEALERDOCUMENTATIONFEE", "VALUATIONFEE", "SUPPLIERRECOVERY", "TAKEN", "CLIENTAGE")

for (f in NumFields) {
  All_lead_Data[[f]] <- gsub(",", ".",  All_lead_Data[[f]], fixed = TRUE)
  All_lead_Data[[f]] <- gsub("[^[:^punct:].]", "", All_lead_Data[[f]], perl = TRUE)
  All_lead_Data[[f]] <- as.numeric(gsub("[[:alpha:]]", "", All_lead_Data[[f]]))
}



# Consent -----------------------------------------------------------------

ConsentFields <- c("COMPANYCONSENT", "OTHERCOMPANYCONSENT", "MARKETINGCONSENT", "LEGITINTCONSENT", "POPICONSENT")

for (f in ConsentFields) {
  All_lead_Data[[f]] <- gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(All_lead_Data[[f]])))
  All_lead_Data[[f]][All_lead_Data[[f]] %in% c("YES", "TRUE")] <- "YES"
  All_lead_Data[[f]][All_lead_Data[[f]] %in% c("NO", "FALSE")] <- "NO"
}

rm(ConsentFields)

# Cleaned Character Fields ------------------------------------------------

CharFields <- colnames(All_lead_Data)[!(colnames(All_lead_Data) %in% c(DateFields, NumFields))]

for (f in CharFields) {
  All_lead_Data[[f]] <- as.character(trim(All_lead_Data[[f]]))
  All_lead_Data[[f]] <- gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(All_lead_Data[[f]]), perl = TRUE) # Proper case
}

# To Upper
MakeUpper <- c("TRANSACTIONNUMBER", "BRANCHCODE", "CLIENTIDTYPE", "REGISTRATIONNUMBER",
               "VINNUMBER", "ENGINENUMBER")

for (f in MakeUpper) {
  All_lead_Data[[f]] <- toupper(All_lead_Data[[f]])
}

# Clean NA Data
MakeNA <- c("VINNUMBER", "ENGINENUMBER")

for (f in MakeNA) {
  All_lead_Data[[f]][All_lead_Data[[f]] == "NO"] <- NA
}


# To Lower
MakeLower <- c("CLIENTEMAILADDRESS", "BMEMAILADDRESS")

for (f in MakeLower) {
  All_lead_Data[[f]] <- tolower(All_lead_Data[[f]])
}

# Fix First Reg Year and New/Used -----------------------------------------

All_lead_Data$NEWUSED[is.na(All_lead_Data$NEWUSED)] <- ifelse(All_lead_Data$ODOMETERREADING[is.na(All_lead_Data$NEWUSED)] < 100,
                                                              "New",
                                                              "Used")

All_lead_Data$NEWUSED[is.na(All_lead_Data$NEWUSED) & 
                        as.numeric(All_lead_Data$FIRSTREGISTRATIONYEAR) < as.numeric(format(Sys.Date(), "%Y"))] <- "Used" 

All_lead_Data$FIRSTREGISTRATIONYEAR[is.na(All_lead_Data$FIRSTREGISTRATIONYEAR) & 
                                      All_lead_Data$ODOMETERREADING < 100] <- ifelse(as.numeric(format(Sys.Date(), "%m")) == 12, 
                                                                                     as.numeric(format(Sys.Date(), "%Y")) + 1,
                                                                                     format(Sys.Date(), "%Y")) 

All_lead_Data$FIRSTREGISTRATIONYEAR <- as.numeric(All_lead_Data$FIRSTREGISTRATIONYEAR)


# Fix Vinnumber where missing ---------------------------------------------

All_lead_Data$VINNUMBER <- gsub(" ", "", All_lead_Data$VINNUMBER)
All_lead_Data$VINNUMBER[is.na(All_lead_Data$VINNUMBER) | All_lead_Data$VINNUMBER == ""] <- paste(All_lead_Data$CLIENTIDNUMBER, 
                                                                                                 All_lead_Data$MANUFACTURER, 
                                                                                                 All_lead_Data$MODEL, 
                                                                                                 All_lead_Data$FIRSTREGISTRATIONYEAR)


# Fix Affinity ------------------------------------------------------------
All_lead_Data$AFFINITY <- word(All_lead_Data$AFFINITY, 1)
All_lead_Data$AFFINITY[All_lead_Data$AFFINITY == "Super"] <- "Super Group"
All_lead_Data$AFFINITY[All_lead_Data$AFFINITY == "Auto"] <- "Auto Pedigree"

# Remove Unusable data ----------------------------------------------------
# Get Taken up policies
All_lead_Data$PRODUCTTYPECATEGORYNAME[is.na(All_lead_Data$PRODUCTTYPECATEGORYNAME)] <- "Retrenchment"
TakenDat <- All_lead_Data$TRANSACTIONNUMBER[All_lead_Data$TAKEN == 1 & All_lead_Data$PRODUCTTYPECATEGORYNAME != "Retrenchment"]
TakenDat <- TakenDat[!duplicated(TakenDat)]

# Remove duplicates
All_lead_Data <- All_lead_Data[!(All_lead_Data$TAKEN == 1 & All_lead_Data$PRODUCTTYPECATEGORYNAME != "Retrenchment"), ]
All_lead_Data <- All_lead_Data[!duplicated(All_lead_Data$TRANSACTIONNUMBER), ]
All_lead_Data <- All_lead_Data[!duplicated(All_lead_Data$CLIENTIDNUMBER), ]

# Remove taken up policies from data
All_lead_Data <- All_lead_Data[!(All_lead_Data$TRANSACTIONNUMBER %in% TakenDat), ]

# Remove cash sales
All_lead_Data$CASHTRANSACTION <- gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(All_lead_Data$CASHTRANSACTION)))
All_lead_Data$CASHTRANSACTION[All_lead_Data$AFFINITY == "Barloworld" & All_lead_Data$DOCFINANCECOMPANYNAME == "Cash"] <- "YES"
All_lead_Data$CASHTRANSACTION[is.na(All_lead_Data$CASHTRANSACTION)] <- "NO"
All_lead_Data <- All_lead_Data[All_lead_Data$CASHTRANSACTION != "YES", ]

# Dedupe against ID number from DB
# Get DB ID numbers
query  <- paste("SELECT `ID Number` FROM AccessLife_Sales_File_Lead_Data", sep = "")
IDNums <- dbGetQuery(mydb, query)
colnames(IDNums) <- "IDNUM"
IDNums <- data.frame(IDNUM = IDNums[!duplicated(IDNums$IDNUM), ])

All_lead_Data <- All_lead_Data[!(All_lead_Data$CLIENTIDNUMBER %in% IDNums$IDNUM), ]

# Remove old people
All_lead_Data <- All_lead_Data[All_lead_Data$CLIENTAGE < 60, ]

# Remove commercial - non self employed
All_lead_Data$CLIENTEMPLOYMENTTYPE[is.na(All_lead_Data$CLIENTEMPLOYMENTTYPE)] <- "Unkown"
All_lead_Data <- All_lead_Data[!(All_lead_Data$CLIENTCATEGORY == "Commercial" & All_lead_Data$CLIENTEMPLOYMENTTYPE != "Self-Employed"), ]

rm(IDNums)























