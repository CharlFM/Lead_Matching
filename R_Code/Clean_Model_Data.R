# Drop useless columns
DB_DAT$CLIENTFULLPOSTAL <- paste(DB_DAT$CLIENTPOSTALADDRESS1, 
                                 DB_DAT$CLIENTPOSTALADDRESS2, 
                                 DB_DAT$CLIENTPOSTALADDRESS3, 
                                 DB_DAT$CLIENTPOSTALADDRESS4,
                                 DB_DAT$CLIENTPOSTALADDRESSPOSTALCODE,
                                 sep = " ")

DB_DAT <- subset(DB_DAT, select = -c(CLIENTWORKTELEPHONENUMBER, CLIENTHOMETELEPHONENUMBER, CLIENTMOBILENUMBER,
                                     REGISTRATIONNUMBER, CLIENTACCOUNTHOLDERNAME, CLIENTBANKBRANCHCODE, CLIENTBANKACCNO,
                                     TRANSACTIONNUMBER, CLIENTPOSTALADDRESS1, CLIENTPOSTALADDRESS2, CLIENTPOSTALADDRESS3,
                                     CLIENTPOSTALADDRESS4, CLIENTPOSTALADDRESSPOSTALCODE))

#########################################
# Clean Zestlife agent and afinity info #
#########################################

# Agent
DB_DAT$ZLAGENT <- gsub(" ", "", gsub("[^[:alpha:] ]", "", toupper(DB_DAT$ZLAGENT)))

# Remove unknown agent data - no point in modelling this
DB_DAT <- DB_DAT[!(DB_DAT$ZLAGENT == "" | is.na(DB_DAT$ZLAGENT)), ]

# Select only active agents
DB_DAT <- DB_DAT[DB_DAT$ZLAGENT %in% Active_Agents_Data$ACTIVEAGENTS, ]

# Affinity
DB_DAT$AFFINITY <- gsub(" ", "", gsub("[^[:alpha:] ]", "", toupper(DB_DAT$AFFINITY)))

# Remove unknown agent data - This should be junk data
DB_DAT <- DB_DAT[!(DB_DAT$AFFINITY == "" | is.na(DB_DAT$AFFINITY)), ]

##############################################
# Clear data that isn't a sale or a non sale #
##############################################

DB_DAT$STATUS <- gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(DB_DAT$STATUS)))

DB_DAT$STATUS2 <- gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(DB_DAT$STATUS2)))

#             Status                    Outcome   Reason
toKeep <- c("DECLINED",               #    0      The lead declined the sale
            "LEADBUSY",               #  1/0/x    Depending on the secondary status 
                                      #               could be that the agent never got hold of the lead hence no outcome
            "SALE",                   #    1      The lead bought the product
            "PROVISIONALSALE",        #    1      The agent sold the product, now it is with underwriting - if they decline it 
                                      #           is not the agent's fault (from a sale point of view)
            "UWDECLINED",             #    1      The agent sold the product but UW decided it is a fail
            "CANCELLED",              #    1      The lead cancelled the policy, hence it was a sale, but then the policy stopped
            "CANCELLATION",           #    1      Same as above
            "DONOTCALL",              #    0      The lead declined and said the agent should not call
            "DECLINEDDONOTCALL")      #    0      Same as above

########################################################## 
# EXCLUDED #
############

#             Status                   
#           "INVALID"                      x      Incorrect number - can't get hold of lead
#           "ENDORSEMENT"                  x      We are looking at first time sales not upgraded policies - this is where the client contacted us
#           "ALLOCATED"                    x      Allocated means the lead is untouched
#           "ALLCOATED"                    x      Same as above
#           "NA"                           x      We don't have info - remove
#           ""                             x      Same as above
#           "RED1PNG"                      x      Same as above

DB_DAT <- DB_DAT %>% 
  filter(STATUS %in% toKeep)

DB_DAT$STATUS2[grepl("SALE", DB_DAT$STATUS2)]                 <- "SALE"
DB_DAT$STATUS2[grepl("NOTINTERESTED", DB_DAT$STATUS2)]        <- "DECLINED"
DB_DAT$STATUS2[!(DB_DAT$STATUS2 %in% c("SALE", "DECLINED"))]  <- "X"

DB_DAT$STATUS[DB_DAT$STATUS == "LEADBUSY" & DB_DAT$STATUS2 == "SALE"]      <- "SALE"
DB_DAT$STATUS[DB_DAT$STATUS == "LEADBUSY" & DB_DAT$STATUS2 == "DECLINED"]  <- "DECLINED"
DB_DAT$STATUS[DB_DAT$STATUS == "LEADBUSY" & DB_DAT$STATUS2 == "X"]         <- "X"

DB_DAT <- DB_DAT[DB_DAT$STATUS != "X", ]

DB_DAT$STATUS[DB_DAT$STATUS %in% c("SALE", "PROVISIONALSALE", "CANCELLED", "CANCELLATION", "UWDECLINED")]  <- 1 # SALE
DB_DAT$STATUS[DB_DAT$STATUS %in% c("DECLINED", "DONOTCALL", "DECLINEDDONOTCALL")]                          <- 0 # NO SALE

DB_DAT <- subset(DB_DAT, select = -c(STATUS2, LEADNUMBER))

DB_DAT$STATUS <- as.numeric(DB_DAT$STATUS)

prop.table(table(DB_DAT$STATUS))

########################################################## 
# Cleaned Inception Date #
##########################

# Populate missing dates (for older leads)
DB_DAT$INCEPTIONDATE <- trim(DB_DAT$INCEPTIONDATE)
DB_DAT$INCEPTIONDATE[DB_DAT$INCEPTIONDATE == ""]         <- NA
DB_DAT$INCEPTIONDATE[DB_DAT$INCEPTIONDATE == "########"] <- NA

DB_DAT$INCEPTIONDATE[is.na(DB_DAT$INCEPTIONDATE)] <- DB_DAT$FIRSTALLOCATIONDATE[is.na(DB_DAT$INCEPTIONDATE)]  
DB_DAT$INCEPTIONDATE[is.na(DB_DAT$INCEPTIONDATE)] <- DB_DAT$LEADDATE[is.na(DB_DAT$INCEPTIONDATE)]

DB_DAT$LEADDATE[is.na(DB_DAT$LEADDATE)] <- DB_DAT$FIRSTALLOCATIONDATE[is.na(DB_DAT$LEADDATE)]  
DB_DAT$LEADDATE <- DateConv(DB_DAT$LEADDATE)

# # Uncomment using ctrl shift c if needed
# # Average Sales per month by agent ----------------------------------------
# 
# Temp_DB <- DB_DAT
# Temp_DB$ByYear <- format(Temp_DB$LEADDATE, "%Y")
# Temp_DB$ByMonth <- format(Temp_DB$LEADDATE, "%m")
# Temp_DB$ByYearMonth <- paste(Temp_DB$ByYear, Temp_DB$ByMonth)
# 
# Results <- Temp_DB %>% 
#   group_by(ByYearMonth, ZLAGENT) %>%
#   summarise(TotSales = sum(STATUS, na.rm = TRUE),
#             Count = n(),
#             Pen = TotSales/Count)
# mean(Results$TotSales)
# 
# ResultsTot <- Temp_DB %>% 
#   group_by(ByYearMonth) %>%
#   summarise(TotSales = sum(STATUS, na.rm = TRUE),
#             Count = n(),
#             Pen = TotSales/Count)
# mean(ResultsTot$TotSales)
# 
# Res2 <- Results %>% 
#   group_by(ZLAGENT) %>%
#   summarise(Mins     = min(TotSales, na.rm = TRUE),
#             Averages = mean(TotSales, na.rm = TRUE),
#             Maxs     = max(TotSales, na.rm = TRUE))
# 
# rm(Temp_DB, Results, ResultsTot, Res2)

#   -----------------------------------------------------------------------

DB_DAT <- subset(DB_DAT, select = -c(FIRSTALLOCATIONDATE, LEADDATE))

################################################################################

DB_DAT$INCEPTIONDATE <- DateConv(DB_DAT$INCEPTIONDATE)

# Subset on more recent data (this is reliable from 2013 and NA's are old data as well)

DB_DAT <- DB_DAT[!is.na(DB_DAT$INCEPTIONDATE), ]
DB_DAT <- DB_DAT[DB_DAT$INCEPTIONDATE > as.Date("2012/12/31"), ]

########################################################## 
# Clean ID Number, birthdate and age #
######################################

# CLIENTIDTYPE "OTHER ID" "RSA ID"
# Determine SA and NON SA ID
DB_DAT$CLIENTIDNUMBER <- gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(DB_DAT$CLIENTIDNUMBER)))
DB_DAT$CLIENTIDTYPE   <- "OTHER ID"

ValidOnCount <- CountAllNums(DB_DAT$CLIENTIDNUMBER) == 13

# Parts of an SA ID:
YY  <- as.numeric(substr(DB_DAT$CLIENTIDNUMBER,  1,  2)) # Year  of Birth (note that 1900 and 2000 will both be 00)
MM  <- as.numeric(substr(DB_DAT$CLIENTIDNUMBER,  3,  4)) # Month of Birth
DD  <- as.numeric(substr(DB_DAT$CLIENTIDNUMBER,  5,  6)) # Day   of Birth   
G   <- as.numeric(substr(DB_DAT$CLIENTIDNUMBER,  7,  7)) # Gender ([0, 4] = Female and [5, 9] = Male)
SSS <- as.numeric(substr(DB_DAT$CLIENTIDNUMBER,  8, 10)) # Birth registration number
C   <- as.numeric(substr(DB_DAT$CLIENTIDNUMBER, 11, 11)) # SA citizen indicator (0 = SA, 1 = Not)
A   <- as.numeric(substr(DB_DAT$CLIENTIDNUMBER, 12, 12)) # Number no longer used. Prior race indicator
Z   <- as.numeric(substr(DB_DAT$CLIENTIDNUMBER, 13, 13)) # A checksum indicator se Luhn algorithm

YY [ValidOnCount == FALSE] <- NA
MM [ValidOnCount == FALSE] <- NA
DD [ValidOnCount == FALSE] <- NA
G  [ValidOnCount == FALSE] <- NA
SSS[ValidOnCount == FALSE] <- NA
C  [ValidOnCount == FALSE] <- NA
A  [ValidOnCount == FALSE] <- NA
Z  [ValidOnCount == FALSE] <- NA

Odd_Sum <- as.numeric(substr(DB_DAT$CLIENTIDNUMBER, 1, 1)) + as.numeric(substr(DB_DAT$CLIENTIDNUMBER, 3, 3)) + 
  as.numeric(substr(DB_DAT$CLIENTIDNUMBER, 5, 5)) + as.numeric(substr(DB_DAT$CLIENTIDNUMBER, 7, 7)) + 
  as.numeric(substr(DB_DAT$CLIENTIDNUMBER, 9, 9)) + as.numeric(substr(DB_DAT$CLIENTIDNUMBER, 11, 11))

Even_Sum <- sumSplitValues(as.character(
  2 * as.numeric(paste(substr(DB_DAT$CLIENTIDNUMBER, 2, 2), substr(DB_DAT$CLIENTIDNUMBER, 4, 4), 
                       substr(DB_DAT$CLIENTIDNUMBER, 6, 6), substr(DB_DAT$CLIENTIDNUMBER, 8, 8), 
                       substr(DB_DAT$CLIENTIDNUMBER, 10, 10), substr(DB_DAT$CLIENTIDNUMBER, 12, 12), sep = ""))))

LuhnVal <- 10 - (Odd_Sum + Even_Sum) %% 10

LuhnVal[ValidOnCount == FALSE] <- NA

ValidOnMost <- (YY  %in% seq(0, 99)  &
                MM  %in% seq(1, 12)  &
                G   %in% seq(0, 10)  &
                SSS %in% seq(0, 999) &
                C   %in% c(0, 1)     &
                A   %in% seq(0, 9)   &
                Z    ==  LuhnVal)

ValidOnMost[ValidOnCount == FALSE] <- NA

ValidOnAll <- (MM == 2 & DD <= 28 & YY %% 4 == 0 |
               MM == 2 & DD <= 29 & YY %% 4 != 0 |
               MM %in% c(4, 6, 9, 11) & DD <= 30 |
               MM %in% c(1, 3, 5, 7, 8, 10, 12) & DD <= 31)

ValidOnAll[is.na(ValidOnCount)] <- NA

YY[is.na(ValidOnAll)] <- NA
MM[is.na(ValidOnAll)] <- NA
DD[is.na(ValidOnAll)] <- NA

DB_DAT$CLIENTIDTYPE[ValidOnAll] <- "RSA ID"

# Clean Birthdate
Cur_Year <- as.numeric(substr(format(Sys.Date(), "%Y"), 3, 4))
B_Days   <- as.Date(paste(ifelse(YY <= Cur_Year, as.numeric(paste(20, YY, sep = "")), as.numeric(paste(19, YY, sep = ""))),
                                 MM,
                                 DD, sep = "-"))

Orig_B_Days <- DateConv(DB_DAT$CLIENTBIRTHDATE)

DB_DAT$CLIENTBIRTHDATE <- B_Days
DB_DAT$CLIENTBIRTHDATE[is.na(DB_DAT$CLIENTBIRTHDATE)] <- Orig_B_Days[is.na(DB_DAT$CLIENTBIRTHDATE)]

rm(YY, MM, DD, G, SSS, C, A, Z, LuhnVal, ValidOnCount, ValidOnMost, ValidOnAll, Cur_Year, Orig_B_Days, B_Days, Odd_Sum, Even_Sum, toKeep)

# Get Age at purchase date
DB_DAT$CLIENTBIRTHDATE[DB_DAT$CLIENTBIRTHDATE == as.Date("1899-12-30")] <- NA
DB_DAT$CLIENTAGE <- as.numeric(DB_DAT$INCEPTIONDATE - DB_DAT$CLIENTBIRTHDATE) / 365.25 

DB_DAT$TooYoung <- 0
DB_DAT$TooYoung[DB_DAT$CLIENTAGE < 19] <- 1

DB_DAT <- DB_DAT[DB_DAT$TooYoung == 0, ]

DB_DAT <- subset(DB_DAT, select = -TooYoung)

########################################################## 
# Clean Marital Status #
########################

DB_DAT$MARITALSTATUS <- gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(DB_DAT$MARITALSTATUS)))

DB_DAT$MARITALSTATUS[grepl("SINGLE", DB_DAT$MARITALSTATUS)]     <-  "SINGLE"
DB_DAT$MARITALSTATUS[grepl("ENGAGED", DB_DAT$MARITALSTATUS)]    <-  "ENGAGED"
DB_DAT$MARITALSTATUS[grepl("MARRIED", DB_DAT$MARITALSTATUS)]    <-  "MARRIED"
DB_DAT$MARITALSTATUS[grepl("SEPARATED", DB_DAT$MARITALSTATUS)]  <-  "DIVORCED"
DB_DAT$MARITALSTATUS[grepl("DIVORCED", DB_DAT$MARITALSTATUS)]   <-  "DIVORCED"
DB_DAT$MARITALSTATUS[grepl("WIDOW", DB_DAT$MARITALSTATUS)]      <-  "WIDOW"

DB_DAT$MARITALSTATUS[!(DB_DAT$MARITALSTATUS %in% c("SINGLE", "ENGAGED", "MARRIED", "DIVORCED", "WIDOW"))]  <- NA

########################################################## 
# Clean Marital Status #
########################

DB_DAT$CLIENTTITLE <- gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(DB_DAT$CLIENTTITLE)))

DB_DAT$CLIENTTITLE[grepl("REV", DB_DAT$CLIENTTITLE)]     <-  "REV"
DB_DAT$CLIENTTITLE[grepl("PROF", DB_DAT$CLIENTTITLE)]    <-  "PROF"
DB_DAT$CLIENTTITLE[grepl("ADV", DB_DAT$CLIENTTITLE)]     <-  "ADV"
DB_DAT$CLIENTTITLE[grepl("DOCTOR", DB_DAT$CLIENTTITLE)]  <-  "DR"

DB_DAT$CLIENTTITLE[!(DB_DAT$CLIENTTITLE %in% c("REV", "PROF", "ADV", "DR", "MR", "MRS", "MS", "MISS"))]  <- NA

########################################################## 
# Clean Race and Surname #
##########################

# Get Race Info
DB_DAT$CLIENTLASTNAME <- gsub(" ","", gsub("[^[:alpha:] ]", "", toupper(DB_DAT$CLIENTLASTNAME)))

DB_DAT <- merge(DB_DAT, Race_Data, by.x = "CLIENTLASTNAME", by.y = "SURNAME", all.x = TRUE)

colnames(DB_DAT)[colnames(DB_DAT) == "RACE"]    <- "RACE_SN"
colnames(DB_DAT)[colnames(DB_DAT) == "CULTURE"] <- "CULTURE_SN"

DB_DAT <- merge(DB_DAT, Race_Data, by.x = "CLIENTFIRSTNAME", by.y = "SURNAME", all.x = TRUE)

colnames(DB_DAT)[colnames(DB_DAT) == "RACE"]    <- "RACE_FN"
colnames(DB_DAT)[colnames(DB_DAT) == "CULTURE"] <- "CULTURE_FN"

DB_DAT$RACE_SN[is.na(DB_DAT$RACE_SN)]       <- DB_DAT$RACE_FN[is.na(DB_DAT$RACE_SN)]
DB_DAT$CULTURE_SN[is.na(DB_DAT$CULTURE_SN)] <- DB_DAT$RACE_FN[is.na(DB_DAT$CULTURE_SN)]

DB_DAT <- subset(DB_DAT, select = -c(CLIENTFIRSTNAME, RACE_FN, CULTURE_FN))

colnames(DB_DAT)[colnames(DB_DAT) == "RACE_SN"]    <- "RACE"
colnames(DB_DAT)[colnames(DB_DAT) == "CULTURE_SN"] <- "CULTURE"

DB_DAT$RACE[DB_DAT$RACE == ""] <- NA
DB_DAT$CULTURE[DB_DAT$CULTURE == ""] <- NA

rm(Race_Data)

########################################################## 
# Clean Email Info #
####################

# Clean email info
DB_DAT$CLIENTEMAILADDRESS_FULL_DOMAIN <- sub(".*\\@", "", DB_DAT$CLIENTEMAILADDRESS)
DB_DAT$CLIENTEMAILADDRESS_SUB_DOMAIN  <- sub("\\..*$", "", DB_DAT$CLIENTEMAILADDRESS_FULL_DOMAIN)
DB_DAT$CLIENTEMAILADDRESS_SUB_DOMAIN  <- gsub("[^[:alpha:]]", "", toupper(DB_DAT$CLIENTEMAILADDRESS_SUB_DOMAIN))

DB_DAT$CLIENTEMAILADDRESS_EXTENTION   <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", DB_DAT$CLIENTEMAILADDRESS_FULL_DOMAIN)))
DB_DAT$CLIENTEMAILADDRESS_EXTENTION   <- gsub("[^[:alpha:]]", "", toupper(DB_DAT$CLIENTEMAILADDRESS_EXTENTION))

DB_DAT$CLIENTEMAILADDRESS_DOTS        <- as.numeric(countLetter(DB_DAT$CLIENTEMAILADDRESS_FULL_DOMAIN, "."))
DB_DAT                                <- subset(DB_DAT, select = -c(CLIENTEMAILADDRESS_FULL_DOMAIN, CLIENTEMAILADDRESS))

temp_tbl    <- data.frame(table(DB_DAT$CLIENTEMAILADDRESS_SUB_DOMAIN)) 
Keeper      <- as.character(temp_tbl$Var1[temp_tbl$Freq/nrow(DB_DAT) > 0.005])
DB_DAT$CLIENTEMAILADDRESS_SUB_DOMAIN[!(DB_DAT$CLIENTEMAILADDRESS_SUB_DOMAIN %in% Keeper)] <- "OTHER"

temp_tbl    <- data.frame(table(DB_DAT$CLIENTEMAILADDRESS_EXTENTION)) 
Keeper      <- as.character(temp_tbl$Var1[temp_tbl$Freq/nrow(DB_DAT) > 0.005])
DB_DAT$CLIENTEMAILADDRESS_EXTENTION[!(DB_DAT$CLIENTEMAILADDRESS_EXTENTION %in% Keeper)] <- "OTHER"

rm(temp_tbl, Keeper)

########################################################## 
# Clean Occupation Info #
#########################

DB_DAT$CLIENTOCCUPATIONNAME <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", DB_DAT$CLIENTOCCUPATIONNAME)))
DB_DAT$CLIENTOCCUPATIONNAME[DB_DAT$CLIENTOCCUPATIONNAME == ""] <- NA

temp_tbl    <- data.frame(table(DB_DAT$CLIENTOCCUPATIONNAME)) 
Keeper      <- as.character(temp_tbl$Var1[temp_tbl$Freq/nrow(DB_DAT) > 0.005])
DB_DAT$CLIENTOCCUPATIONNAME[!(DB_DAT$CLIENTOCCUPATIONNAME %in% Keeper)] <- "OTHER"

rm(temp_tbl, Keeper)

########################################################## 
# Clean Vehicle Use Info #
##########################

DB_DAT$VEHICLEUSE <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", DB_DAT$VEHICLEUSE)))
DB_DAT$VEHICLEUSE[DB_DAT$VEHICLEUSE == ""] <- NA

########################################################## 
# Clean Finance and Insurance Company Info #
############################################

DB_DAT$DOCINSURANCECOMPANYNAME <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", DB_DAT$DOCINSURANCECOMPANYNAME)))
DB_DAT$DOCINSURANCECOMPANYNAME[DB_DAT$DOCINSURANCECOMPANYNAME == ""] <- NA

temp_tbl    <- data.frame(table(DB_DAT$DOCINSURANCECOMPANYNAME)) 
Keeper      <- as.character(temp_tbl$Var1[temp_tbl$Freq/nrow(DB_DAT) > 0.005])
DB_DAT$DOCINSURANCECOMPANYNAME[!(DB_DAT$DOCINSURANCECOMPANYNAME %in% Keeper)] <- "OTHER"

DB_DAT$DOCFINANCECOMPANYNAME <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", DB_DAT$DOCFINANCECOMPANYNAME)))
DB_DAT$DOCFINANCECOMPANYNAME[DB_DAT$DOCFINANCECOMPANYNAME == ""] <- NA

temp_tbl    <- data.frame(table(DB_DAT$DOCFINANCECOMPANYNAME)) 
Keeper      <- as.character(temp_tbl$Var1[temp_tbl$Freq/nrow(DB_DAT) > 0.005])
DB_DAT$DOCFINANCECOMPANYNAME[!(DB_DAT$DOCFINANCECOMPANYNAME %in% Keeper)] <- "OTHER"

rm(temp_tbl, Keeper)

########################################################## 
# Clean Client Banking Info #
#############################

DB_DAT$CLIENTBANKNAME <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", DB_DAT$CLIENTBANKNAME)))
DB_DAT$CLIENTBANKNAME[DB_DAT$CLIENTBANKNAME == ""] <- NA

temp_tbl    <- data.frame(table(DB_DAT$CLIENTBANKNAME)) 
Keeper      <- as.character(temp_tbl$Var1[temp_tbl$Freq/nrow(DB_DAT) > 0.005])
DB_DAT$CLIENTBANKNAME[!(DB_DAT$CLIENTBANKNAME %in% Keeper)] <- "OTHER"

DB_DAT$CLIENTBANKBRANCH <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", DB_DAT$CLIENTBANKBRANCH)))
DB_DAT$CLIENTBANKBRANCH[DB_DAT$CLIENTBANKBRANCH == ""] <- NA
DB_DAT$CLIENTBANKBRANCH <- ifelse(DB_DAT$CLIENTBANKBRANCH != "", paste(DB_DAT$CLIENTBANKNAME, DB_DAT$CLIENTBANKBRANCH, sep = "_"), "")

temp_tbl    <- data.frame(table(DB_DAT$CLIENTBANKBRANCH)) 
Keeper      <- as.character(temp_tbl$Var1[temp_tbl$Freq/nrow(DB_DAT) > 0.005])
DB_DAT$CLIENTBANKBRANCH[!(DB_DAT$CLIENTBANKBRANCH %in% Keeper)] <- "OTHER"

DB_DAT$CLIENTBANKACCOUNTTYPE <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", DB_DAT$CLIENTBANKACCOUNTTYPE)))
DB_DAT$CLIENTBANKACCOUNTTYPE[DB_DAT$CLIENTBANKACCOUNTTYPE == ""] <- NA

temp_tbl    <- data.frame(table(DB_DAT$CLIENTBANKACCOUNTTYPE)) 
Keeper      <- as.character(temp_tbl$Var1[temp_tbl$Freq/nrow(DB_DAT) > 0.005])
DB_DAT$CLIENTBANKACCOUNTTYPE[!(DB_DAT$CLIENTBANKACCOUNTTYPE %in% Keeper)] <- "OTHER"

rm(temp_tbl, Keeper)

########################################################## 
# Clean Branch and Salesman Info #
##################################

DB_DAT$BRANCHNAME <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", DB_DAT$BRANCHNAME)))

DB_DAT$SALESPERSON <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", DB_DAT$SALESPERSON)))

DB_DAT$SALESPERSON <- ifelse(DB_DAT$SALESPERSON != "", paste(DB_DAT$BRANCHNAME, DB_DAT$SALESPERSON, sep = "_"), "")

DB_DAT$SALESPERSON[DB_DAT$SALESPERSON == ""] <- NA
DB_DAT$BRANCHNAME[DB_DAT$BRANCHNAME == ""]   <- NA

temp_tbl    <- data.frame(table(DB_DAT$SALESPERSON)) 
Keeper      <- as.character(temp_tbl$Var1[temp_tbl$Freq/nrow(DB_DAT) > 0.005])
DB_DAT$SALESPERSON[!(DB_DAT$SALESPERSON %in% Keeper)] <- "OTHER"

temp_tbl    <- data.frame(table(DB_DAT$BRANCHNAME)) 
Keeper      <- as.character(temp_tbl$Var1[temp_tbl$Freq/nrow(DB_DAT) > 0.005])
DB_DAT$BRANCHNAME[!(DB_DAT$BRANCHNAME %in% Keeper)] <- "OTHER"

rm(temp_tbl, Keeper)

########################################################## 
# Clean All Numeric Variables #
###############################

Options <- c("FINANCETERM", "VEHICLEVALUE", "DEPOSITVALUE", "RESIDUALVALUE", "FINANCEAMOUNT", "ODOMETERREADING")

for (opt in Options) {
  DB_DAT[[opt]]                        <-  as.numeric(DB_DAT[[opt]])
  DB_DAT[[opt]][is.na(DB_DAT[[opt]])]  <-  0
}

rm(Options, opt)

########################################################## 
# Clean Year Model Variables #
##############################

# Diff between car model year and purchase year
YearDf <- strsplit(UniDash(DB_DAT$FIRSTREGISTRATIONYEAR), "-")

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

DB_DAT$FIRSTREGISTRATIONYEAR <- as.numeric(yyFin)

#Vehicle age at purchase
DB_DAT$VEHICLEAGE <- as.numeric(format(DB_DAT$INCEPTIONDATE, "%Y")) - DB_DAT$FIRSTREGISTRATIONYEAR

# Note - the negative one values (-1) is due to cars being bought at the end of the year (hence bought in 2015/12/01, thus it is
#                                                                                         classified as a 2016 model : age = -1)

rm(n, l, YearDf, yyFin, i, yyPrt)

########################################################## 
# Clean Car Model Variables #
#############################

ModDf <- strsplit(DB_DAT$MODEL, " ")

n <- max(sapply(ModDf, length))
l <- lapply(ModDf, function(X) c(X, rep(NA, n - length(X))))

ModDf <- data.frame(t(do.call(cbind, l)), stringsAsFactors = FALSE)
colnames(ModDf) <- paste("Model", seq(1:ncol(ModDf)), sep = "")

ModDf[[1]][is.na(ModDf[[1]])] <- ""
ModDf[[2]][is.na(ModDf[[2]])] <- ""

DB_DAT$MANUFACTURER <- ModDf[[1]]
DB_DAT$MODEL        <- paste(ModDf[[1]], ModDf[[2]], sep = "")

DB_DAT$MANUFACTURER[DB_DAT$MANUFACTURER == ""] <- NA
DB_DAT$MODEL[DB_DAT$MODEL == ""]               <- NA

DB_DAT$MANUFACTURER <- gsub("[^[:alnum:] ]", "", toupper(gsub("^.*?\\.", "", DB_DAT$MANUFACTURER))) 
DB_DAT$MODEL <- gsub("[^[:alnum:] ]", "", toupper(gsub("^.*?\\.", "", DB_DAT$MODEL))) 

rm(n, l, ModDf)

temp_tbl    <- data.frame(table(DB_DAT$MODEL)) 
Keeper      <- as.character(temp_tbl$Var1[temp_tbl$Freq/nrow(DB_DAT) > 0.005])
DB_DAT$MODEL[!(DB_DAT$MODEL %in% Keeper)] <- "OTHER"

temp_tbl    <- data.frame(table(DB_DAT$MANUFACTURER)) 
Keeper      <- as.character(temp_tbl$Var1[temp_tbl$Freq/nrow(DB_DAT) > 0.005])
DB_DAT$MANUFACTURER[!(DB_DAT$MANUFACTURER %in% Keeper)] <- "OTHER"

rm(temp_tbl, Keeper)

########################################################## 
# Count Number Of Accessories #
###############################

DB_DAT$ACCESSORIES <- str_count(DB_DAT$ACCESSORIES, ";")

########################################################## 
# Remove Non Model Data #
#########################

DB_DAT <- subset(DB_DAT, select = -CLIENTLASTNAME)

########################################################## 
# Clean Address Info #
######################

# Clean province info
Provinces <- c("WESTERNCAPE", "WESTERNPROVINCE", "LIMPOPO", "MPUMALANGA", "GAUTENG", 
               "EASTERNCAPE", "KWAZULUNATAL", "NORTHERNCAPE", "FREESTATE", "NORTHWEST")

AdrDf <- strsplit(DB_DAT$CLIENTFULLPOSTAL, " ")

n <- max(sapply(AdrDf, length))
l <- lapply(AdrDf, function(X) c(X, rep(NA, n - length(X))))

AdrDf <- data.frame(t(do.call(cbind, l)), stringsAsFactors = FALSE)
colnames(AdrDf) <- paste("Address", seq(1:ncol(AdrDf)), sep = "")

for (i in 1:n) {
  AdrDf[[i]] <- gsub("[^[:alnum:] ]", "", toupper(gsub("^.*?\\.", "", AdrDf[[i]]))) 
  AdrDf[[i]][AdrDf[[i]] == ""] <- NA
}

PostalDF <- AdrDf

for (i in 1:n) {
  PostalDF[[i]] <- as.numeric(PostalDF[[i]])
}

PostalDF <- data.matrix(PostalDF)
PostalDF <- PostalDF/PostalDF
PostalDF[is.na(PostalDF)] <- 0

tempDf <- PostalDF

PostalDF <- as.data.frame(t(apply(PostalDF, 1, cumsum)))

PostalDF[tempDf == 0] <- 0

PostCol <- max.col(PostalDF, "first")

PcodeFin <- do.call(c, args = as.list(AdrDf))

DB_DAT$CLIENTPOSTALADDRESSPOSTALCODE <- PcodeFin[(1:NROW(AdrDf)) + NROW(AdrDf) * (PostCol - 1)]
DB_DAT$CLIENTPOSTALADDRESSPOSTALCODE <- as.character(as.numeric(DB_DAT$CLIENTPOSTALADDRESSPOSTALCODE))
City_Post_Data$POSTALCODE <- as.character(as.numeric(City_Post_Data$POSTALCODE))

DB_DAT <- merge(DB_DAT, City_Post_Data, by.x = "CLIENTPOSTALADDRESSPOSTALCODE", by.y = "POSTALCODE", all.x = TRUE)

rm(Provinces, City_Data, AdrDf, n, l, tempDf, i, PostCol, PcodeFin, City_Post_Data, City_Prov_Dat, PostalDF, ProvinceDat, Pcode)

DB_DAT <- subset(DB_DAT, select = -CLIENTFULLPOSTAL)

colnames(DB_DAT)[colnames(DB_DAT) == "CITY"]      <-  "CLIENTPOSTALADDRESSCITY"
colnames(DB_DAT)[colnames(DB_DAT) == "PROVINCE"]  <-  "CLIENTPOSTALADDRESSPROVINCE"
colnames(DB_DAT)[colnames(DB_DAT) == "SUBURB"]    <-  "CLIENTPOSTALADDRESSSUBURB"

temp_tbl    <- data.frame(table(DB_DAT$CLIENTPOSTALADDRESSCITY)) 
Keeper      <- as.character(temp_tbl$Var1[temp_tbl$Freq/nrow(DB_DAT) > 0.005])
DB_DAT$CLIENTPOSTALADDRESSCITY[!(DB_DAT$CLIENTPOSTALADDRESSCITY %in% Keeper)] <- "OTHER"

temp_tbl    <- data.frame(table(DB_DAT$CLIENTPOSTALADDRESSPOSTALCODE)) 
Keeper      <- as.character(temp_tbl$Var1[temp_tbl$Freq/nrow(DB_DAT) > 0.005])
DB_DAT$CLIENTPOSTALADDRESSPOSTALCODE[!(DB_DAT$CLIENTPOSTALADDRESSPOSTALCODE %in% Keeper)] <- "OTHER"

temp_tbl    <- data.frame(table(DB_DAT$CLIENTPOSTALADDRESSSUBURB)) 
Keeper      <- as.character(temp_tbl$Var1[temp_tbl$Freq/nrow(DB_DAT) > 0.005])
DB_DAT$CLIENTPOSTALADDRESSSUBURB[!(DB_DAT$CLIENTPOSTALADDRESSSUBURB %in% Keeper)] <- "OTHER"

rm(temp_tbl, Keeper)

# Clean Lead Pickup Date and get time-to-call

DB_DAT <- DB_DAT[!is.na(DB_DAT$LEADPICKUPDATE), ]

DB_DAT$LEADPICKUPDATE <- DateConv(DB_DAT$LEADPICKUPDATE)
DB_DAT$TIMETOCALL <- as.numeric(DB_DAT$LEADPICKUPDATE - DB_DAT$INCEPTIONDATE)

DB_DAT <- DB_DAT[DB_DAT$TIMETOCALL >= 0, ]

DB_DAT$WEEKDAY  <- weekdays(DB_DAT$LEADPICKUPDATE)
DB_DAT$WEEKEND  <- "Weekday"
DB_DAT$WEEKEND[DB_DAT$WEEKDAY == "Saturday" | DB_DAT$WEEKDAY == "Sunday"]  <- "Weekend"

DB_DAT$WEEKTIME <- "Early"
DB_DAT$WEEKTIME[DB_DAT$WEEKDAY == "Wednesday"]  <- "Mid"
DB_DAT$WEEKTIME[DB_DAT$WEEKDAY == "Thursday" | DB_DAT$WEEKDAY == "Friday"]  <- "Late"
DB_DAT$WEEKTIME[DB_DAT$WEEKDAY == "Saturday" | DB_DAT$WEEKDAY == "Sunday"]  <- "Weekend"

#########################################################################

EnricoURL   <- paste("http://kayaposoft.com/enrico/json/v1.0/index.php?action=getPublicHolidaysForDateRange&fromDate=01-01-2013&toDate=31-12-",
                     format(Sys.Date(),"%Y"), 
                     "&country=zaf&region=all",
                     sep = "")
Enrico_data <- fromJSON(EnricoURL)

Enrico_data$date$month <- str_pad(Enrico_data$date$month, width = 2, side = "left", pad = "0")
Enrico_data$date$day   <- str_pad(Enrico_data$date$day,   width = 2, side = "left", pad = "0")

Enrico_data$Clean_Date <- as.Date(paste(Enrico_data$date$year, Enrico_data$date$month, Enrico_data$date$day, sep = "-"))
Enrico_data$PUBHOLIDAY <- "Public_Holiday"

Enrico_data <- subset(Enrico_data, select = c(Clean_Date, PUBHOLIDAY))

DB_DAT <- merge(DB_DAT, Enrico_data, by.x = "LEADPICKUPDATE", by.y = "Clean_Date", all.x = TRUE)

DB_DAT$PUBHOLIDAY[is.na(DB_DAT$PUBHOLIDAY)] <- "Normal_Day"

rm(Enrico_data)

#########################################################################

# POSIX*t objects need both date and time specified
# Here, the particular date doesn't matter - just that there is one.
DB_DAT$TempTime <- strptime(paste("2001-01-01", DB_DAT$LEADPICKUPTIME), format = "%Y-%m-%d %H:%M")

# Use round.Date to round, then format to format
DB_DAT$LEADPICKUPTIME <- as.numeric(format(round(DB_DAT$TempTime, units = "hours"), format = "%H"))

# Impute missing Lead Pickup Times - fitting distribution to observed data and simulating over unkown
hist(DB_DAT$LEADPICKUPTIME)
data <- DB_DAT$LEADPICKUPTIME[!is.na(DB_DAT$LEADPICKUPTIME)]
mix_mdl <- normalmixEM(data)

plot(mix_mdl, which = 2)

mu1 <- mix_mdl$mu[1]
mu2 <- mix_mdl$mu[2]
sig1 <- mix_mdl$sigma[1]
sig2 <- mix_mdl$sigma[2]
cpct <- mix_mdl$lambda[1]

bimodalData <- bimodalDistFunc(n = sum(is.na(DB_DAT$LEADPICKUPTIME)), cpct, mu1, mu2, sig1, sig2)
bimodalData <- round(bimodalData)
bimodalData[bimodalData < 5]  <- 5
bimodalData[bimodalData > 21] <- 21
hist(bimodalData)
summary(bimodalData)

DB_DAT$LEADPICKUPTIME[is.na(DB_DAT$LEADPICKUPTIME)] <- bimodalData
hist(DB_DAT$LEADPICKUPTIME)

#########################################################################

DB_DAT$DAYTIME <- "Morning"
DB_DAT$DAYTIME[DB_DAT$LEADPICKUPTIME %in% 12:14] <- "Lunch"
DB_DAT$DAYTIME[DB_DAT$LEADPICKUPTIME %in% 15:18] <- "Midday"
DB_DAT$DAYTIME[DB_DAT$LEADPICKUPTIME %in% 19:24] <- "Night"

DB_DAT <- subset(DB_DAT, select = -TempTime)






