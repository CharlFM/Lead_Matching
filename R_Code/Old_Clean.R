# Clean ID
newLead_Dat$CLIENTIDNUMBER <- gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(newLead_Dat$CLIENTIDNUMBER)))
newLead_Dat$CLIENTIDTYPE   <- "OTHER ID"

ValidOnCount <- CountAllNums(newLead_Dat$CLIENTIDNUMBER) == 13

# Parts of an SA ID:
YY  <- as.numeric(substr(newLead_Dat$CLIENTIDNUMBER,  1,  2)) # Year  of Birth (note that 1900 and 2000 will both be 00)
MM  <- as.numeric(substr(newLead_Dat$CLIENTIDNUMBER,  3,  4)) # Month of Birth
DD  <- as.numeric(substr(newLead_Dat$CLIENTIDNUMBER,  5,  6)) # Day   of Birth   
G   <- as.numeric(substr(newLead_Dat$CLIENTIDNUMBER,  7,  7)) # Gender ([0, 4] = Female and [5, 9] = Male)
SSS <- as.numeric(substr(newLead_Dat$CLIENTIDNUMBER,  8, 10)) # Birth registration number
C   <- as.numeric(substr(newLead_Dat$CLIENTIDNUMBER, 11, 11)) # SA citizen indicator (0 = SA, 1 = Not)
A   <- as.numeric(substr(newLead_Dat$CLIENTIDNUMBER, 12, 12)) # Number no longer used. Prior race indicator
Z   <- as.numeric(substr(newLead_Dat$CLIENTIDNUMBER, 13, 13)) # A checksum indicator se Luhn algorithm

YY [ValidOnCount == FALSE] <- NA
MM [ValidOnCount == FALSE] <- NA
DD [ValidOnCount == FALSE] <- NA
G  [ValidOnCount == FALSE] <- NA
SSS[ValidOnCount == FALSE] <- NA
C  [ValidOnCount == FALSE] <- NA
A  [ValidOnCount == FALSE] <- NA
Z  [ValidOnCount == FALSE] <- NA

Odd_Sum <- as.numeric(substr(newLead_Dat$CLIENTIDNUMBER, 1, 1)) + as.numeric(substr(newLead_Dat$CLIENTIDNUMBER, 3, 3)) + 
  as.numeric(substr(newLead_Dat$CLIENTIDNUMBER, 5, 5)) + as.numeric(substr(newLead_Dat$CLIENTIDNUMBER, 7, 7)) + 
  as.numeric(substr(newLead_Dat$CLIENTIDNUMBER, 9, 9)) + as.numeric(substr(newLead_Dat$CLIENTIDNUMBER, 11, 11))

Even_Sum <- sumSplitValues(as.character(
  2 * as.numeric(paste(substr(newLead_Dat$CLIENTIDNUMBER, 2, 2), substr(newLead_Dat$CLIENTIDNUMBER, 4, 4), 
                       substr(newLead_Dat$CLIENTIDNUMBER, 6, 6), substr(newLead_Dat$CLIENTIDNUMBER, 8, 8), 
                       substr(newLead_Dat$CLIENTIDNUMBER, 10, 10), substr(newLead_Dat$CLIENTIDNUMBER, 12, 12), sep = ""))))

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

newLead_Dat$CLIENTIDTYPE[ValidOnAll] <- "RSA ID"

# Clean Birthdate
Cur_Year <- as.numeric(substr(format(Sys.Date(), "%Y"), 3, 4))
B_Days   <- as.Date(paste(ifelse(YY <= Cur_Year, as.numeric(paste(20, YY, sep = "")), as.numeric(paste(19, YY, sep = ""))),
                          MM,
                          DD, sep = "-"))

Orig_B_Days <- DateConv(newLead_Dat$CLIENTBIRTHDATE)

newLead_Dat$CLIENTBIRTHDATE <- B_Days
newLead_Dat$CLIENTBIRTHDATE[is.na(newLead_Dat$CLIENTBIRTHDATE)] <- Orig_B_Days[is.na(newLead_Dat$CLIENTBIRTHDATE)]

rm(YY, MM, DD, G, SSS, C, A, Z, LuhnVal, ValidOnCount, ValidOnMost, ValidOnAll, Cur_Year, Orig_B_Days, B_Days, Odd_Sum, Even_Sum)

# Get Age at purchase date
newLead_Dat$CLIENTBIRTHDATE[newLead_Dat$CLIENTBIRTHDATE == as.Date("1899-12-30")] <- NA
newLead_Dat$CLIENTAGE <- as.numeric(newLead_Dat$INCEPTIONDATE - newLead_Dat$CLIENTBIRTHDATE) / 365.25 

# Cleans Race and Culture -------------------------------------------------
newLead_Dat$CLIENTLASTNAME <- gsub(" ","", gsub("[^[:alpha:] ]", "", toupper(newLead_Dat$CLIENTLASTNAME)))

newLead_Dat <- merge(newLead_Dat, Race_Data, by.x = "CLIENTLASTNAME", by.y = "SURNAME", all.x = TRUE)

colnames(newLead_Dat)[colnames(newLead_Dat) == "RACE"]    <- "RACE_SN"
colnames(newLead_Dat)[colnames(newLead_Dat) == "CULTURE"] <- "CULTURE_SN"

newLead_Dat <- merge(newLead_Dat, Race_Data, by.x = "CLIENTFIRSTNAME", by.y = "SURNAME", all.x = TRUE)

colnames(newLead_Dat)[colnames(newLead_Dat) == "RACE"]    <- "RACE_FN"
colnames(newLead_Dat)[colnames(newLead_Dat) == "CULTURE"] <- "CULTURE_FN"

newLead_Dat$RACE_SN[is.na(newLead_Dat$RACE_SN)]       <- newLead_Dat$RACE_FN[is.na(newLead_Dat$RACE_SN)]
newLead_Dat$CULTURE_SN[is.na(newLead_Dat$CULTURE_SN)] <- newLead_Dat$RACE_FN[is.na(newLead_Dat$CULTURE_SN)]

newLead_Dat <- subset(newLead_Dat, select = -c(CLIENTFIRSTNAME, RACE_FN, CULTURE_FN))

colnames(newLead_Dat)[colnames(newLead_Dat) == "RACE_SN"]    <- "RACE"
colnames(newLead_Dat)[colnames(newLead_Dat) == "CULTURE_SN"] <- "CULTURE"

DB_Ra <- Model$var.levels[which(Model$var.names == "RACE")][[1]]
DB_Cu <- Model$var.levels[which(Model$var.names == "CULTURE")][[1]]

newLead_Dat$RACE[!(newLead_Dat$RACE %in% DB_Ra)]       <- NA
newLead_Dat$CULTURE[!(newLead_Dat$CULTURE %in% DB_Cu)] <- NA

rm(Race_Data, DB_Ra, DB_Cu)
















