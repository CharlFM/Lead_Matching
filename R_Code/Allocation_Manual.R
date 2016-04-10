
# Load Data ---------------------------------------------------------------

load(paste(getwd(), "/Offline_WS.RData", sep = ""))

# Cleans data used for modelling
source(paste(Path, "/R_Code/Clean_Model_Data.R", sep = ""))

# Loads City/Race/ etc data - For Allocation and Modelling
source(paste(Path, "/R_Code/Load_Other_Data.R", sep = ""))

# Load current model
load(paste(getwd(), "/Active_Model.RData", sep = ""))

# Loads manual allocation Data
DB_Names <- read_excel(paste(Path, "/Data/Lead_Col_Names/DBNAMES.xlsx", sep = ""),
                       sheet = 1,
                       col_names = TRUE)

ManLead_Dat <- read_excel(paste(Path, "/Data/Manual_Data/Man_Lead_Data.xlsx", sep = ""),
                          sheet = 1,
                          col_names = TRUE)

Allocation_Dat <- read_excel(paste(Path, "/Data/Manual_Data/Allocation_Data.xlsx", sep = ""),
                             sheet = 1,
                             col_names = TRUE)

colnames(ManLead_Dat)  <- gsub(" ","", gsub("[^[:alnum:] ]", "", gsub("X.","", toupper(colnames(ManLead_Dat)))))
DB_Names$Original <- gsub(" ","", gsub("[^[:alnum:] ]", "", gsub("X.","", toupper(DB_Names$Original))))

ManLead_Dat <- ManLead_Dat[, colnames(ManLead_Dat) %in% DB_Names$Original]
colnames(ManLead_Dat) <- DB_Names$New[match(colnames(ManLead_Dat), DB_Names$Original)]


# Prepare Model Data ------------------------------------------------------

# Drop useless columns
ManLead_Dat$CLIENTFULLPOSTAL <- paste(ManLead_Dat$CLIENTPOSTALADDRESS1, 
                                 ManLead_Dat$CLIENTPOSTALADDRESS2, 
                                 ManLead_Dat$CLIENTPOSTALADDRESS3, 
                                 ManLead_Dat$CLIENTPOSTALADDRESS4,
                                 ManLead_Dat$CLIENTPOSTALADDRESSPOSTALCODE,
                                 sep = " ")

ManLead_Dat <- subset(ManLead_Dat, select = -c(CLIENTFIRSTNAME, CLIENTWORKTELEPHONENUMBER, CLIENTHOMETELEPHONENUMBER, CLIENTMOBILENUMBER,
                                     REGISTRATIONNUMBER, CLIENTACCOUNTHOLDERNAME, CLIENTBANKBRANCHCODE, CLIENTBANKACCNO,
                                     TRANSACTIONNUMBER, CLIENTPOSTALADDRESS1, CLIENTPOSTALADDRESS2, CLIENTPOSTALADDRESS3,
                                     CLIENTPOSTALADDRESS4, CLIENTPOSTALADDRESSPOSTALCODE))

ManLead_Dat <- subset(ManLead_Dat, select = -LEADNUMBER)

#
# Cleaned Inception Date 
#

# Populate missing dates (for older leads)
ManLead_Dat$INCEPTIONDATE <- trim(ManLead_Dat$INCEPTIONDATE)
ManLead_Dat$INCEPTIONDATE[ManLead_Dat$INCEPTIONDATE == ""]         <- NA
ManLead_Dat$INCEPTIONDATE[ManLead_Dat$INCEPTIONDATE == "########"] <- NA

ManLead_Dat$INCEPTIONDATE[is.na(ManLead_Dat$INCEPTIONDATE)] <- ManLead_Dat$FIRSTALLOCATIONDATE[is.na(ManLead_Dat$INCEPTIONDATE)]  
ManLead_Dat$INCEPTIONDATE[is.na(ManLead_Dat$INCEPTIONDATE)] <- ManLead_Dat$LEADDATE[is.na(ManLead_Dat$INCEPTIONDATE)]

ManLead_Dat$LEADDATE[is.na(ManLead_Dat$LEADDATE)] <- ManLead_Dat$FIRSTALLOCATIONDATE[is.na(ManLead_Dat$LEADDATE)]  
ManLead_Dat$LEADDATE <- DateConv(ManLead_Dat$LEADDATE)

# Uncomment using ctrl shift c if needed
# # Average Sales per month by agent ----------------------------------------
# 
# Temp_DB <- ManLead_Dat
# Temp_DB$ByYear <- format(Temp_DB$LEADDATE, "%Y")
# Temp_DB$ByMonth <- format(Temp_DB$LEADDATE, "%m")
# Temp_DB$ByYearMonth <- paste(Temp_DB$ByYear, Temp_DB$ByMonth)
# 
# Results <- Temp_DB %>% 
#   group_by(ByYearMonth, ZLAGENT) %>%
#   summarise(TotSales = sum(STATUS, na.rm = TRUE))
# mean(Results$TotSales)
# 
# ResultsTot <- Temp_DB %>% 
#   group_by(ByYearMonth) %>%
#   summarise(TotSales = sum(STATUS, na.rm = TRUE))
# mean(ResultsTot$TotSales)
# 
# Res2 <- Results %>% 
#   group_by(ZLAGENT) %>%
#   summarise(Mins     = min(TotSales, na.rm = TRUE),
#             Averages = mean(TotSales, na.rm = TRUE),
#             Maxs     = max(TotSales, na.rm = TRUE))
# 
# rm(Temp_DB, Results, ResultsTot, Res2)

ManLead_Dat <- subset(ManLead_Dat, select = -c(FIRSTALLOCATIONDATE, LEADDATE))

#

ManLead_Dat$INCEPTIONDATE <- DateConv(ManLead_Dat$INCEPTIONDATE)

#
# Clean ID Number, birthdate and age
#

# CLIENTIDTYPE "OTHER ID" "RSA ID"
# Determine SA and NON SA ID
ManLead_Dat$CLIENTIDNUMBER <- gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(ManLead_Dat$CLIENTIDNUMBER)))
ManLead_Dat$CLIENTIDTYPE   <- "OTHER ID"

ValidOnCount <- CountAllNums(ManLead_Dat$CLIENTIDNUMBER) == 13

# Parts of an SA ID:
YY  <- as.numeric(substr(ManLead_Dat$CLIENTIDNUMBER,  1,  2)) # Year  of Birth (note that 1900 and 2000 will both be 00)
MM  <- as.numeric(substr(ManLead_Dat$CLIENTIDNUMBER,  3,  4)) # Month of Birth
DD  <- as.numeric(substr(ManLead_Dat$CLIENTIDNUMBER,  5,  6)) # Day   of Birth   
G   <- as.numeric(substr(ManLead_Dat$CLIENTIDNUMBER,  7,  7)) # Gender ([0, 4] = Female and [5, 9] = Male)
SSS <- as.numeric(substr(ManLead_Dat$CLIENTIDNUMBER,  8, 10)) # Birth registration number
C   <- as.numeric(substr(ManLead_Dat$CLIENTIDNUMBER, 11, 11)) # SA citizen indicator (0 = SA, 1 = Not)
A   <- as.numeric(substr(ManLead_Dat$CLIENTIDNUMBER, 12, 12)) # Number no longer used. Prior race indicator
Z   <- as.numeric(substr(ManLead_Dat$CLIENTIDNUMBER, 13, 13)) # A checksum indicator se Luhn algorithm

YY [ValidOnCount == FALSE] <- NA
MM [ValidOnCount == FALSE] <- NA
DD [ValidOnCount == FALSE] <- NA
G  [ValidOnCount == FALSE] <- NA
SSS[ValidOnCount == FALSE] <- NA
C  [ValidOnCount == FALSE] <- NA
A  [ValidOnCount == FALSE] <- NA
Z  [ValidOnCount == FALSE] <- NA

Odd_Sum <- as.numeric(substr(ManLead_Dat$CLIENTIDNUMBER, 1, 1)) + as.numeric(substr(ManLead_Dat$CLIENTIDNUMBER, 3, 3)) + 
  as.numeric(substr(ManLead_Dat$CLIENTIDNUMBER, 5, 5)) + as.numeric(substr(ManLead_Dat$CLIENTIDNUMBER, 7, 7)) + 
  as.numeric(substr(ManLead_Dat$CLIENTIDNUMBER, 9, 9)) + as.numeric(substr(ManLead_Dat$CLIENTIDNUMBER, 11, 11))

Even_Sum <- sumSplitValues(as.character(
  2 * as.numeric(paste(substr(ManLead_Dat$CLIENTIDNUMBER, 2, 2), substr(ManLead_Dat$CLIENTIDNUMBER, 4, 4), 
                       substr(ManLead_Dat$CLIENTIDNUMBER, 6, 6), substr(ManLead_Dat$CLIENTIDNUMBER, 8, 8), 
                       substr(ManLead_Dat$CLIENTIDNUMBER, 10, 10), substr(ManLead_Dat$CLIENTIDNUMBER, 12, 12), sep = ""))))

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

ManLead_Dat$CLIENTIDTYPE[ValidOnAll] <- "RSA ID"

# Clean Birthdate
Cur_Year <- as.numeric(substr(format(Sys.Date(), "%Y"), 3, 4))
B_Days   <- as.Date(paste(ifelse(YY <= Cur_Year, as.numeric(paste(20, YY, sep = "")), as.numeric(paste(19, YY, sep = ""))),
                          MM,
                          DD, sep = "-"))

Orig_B_Days <- DateConv(ManLead_Dat$CLIENTBIRTHDATE)

ManLead_Dat$CLIENTBIRTHDATE <- B_Days
ManLead_Dat$CLIENTBIRTHDATE[is.na(ManLead_Dat$CLIENTBIRTHDATE)] <- Orig_B_Days[is.na(ManLead_Dat$CLIENTBIRTHDATE)]

rm(YY, MM, DD, G, SSS, C, A, Z, LuhnVal, ValidOnCount, ValidOnMost, ValidOnAll, Cur_Year, Orig_B_Days, B_Days, Odd_Sum, Even_Sum)

# Get Age at purchase date
ManLead_Dat$CLIENTBIRTHDATE[ManLead_Dat$CLIENTBIRTHDATE == as.Date("1899-12-30")] <- NA
ManLead_Dat$CLIENTAGE <- as.numeric(ManLead_Dat$INCEPTIONDATE - ManLead_Dat$CLIENTBIRTHDATE) / 365.25 

ManLead_Dat$TooYoung <- 0
ManLead_Dat$TooYoung[ManLead_Dat$CLIENTAGE < 19] <- 1

ManLead_Dat <- ManLead_Dat[ManLead_Dat$TooYoung == 0, ]

ManLead_Dat <- subset(ManLead_Dat, select = -TooYoung)

#
# Clean Marital Status
#

ManLead_Dat$MARITALSTATUS <- gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(ManLead_Dat$MARITALSTATUS)))

ManLead_Dat$MARITALSTATUS[grepl("SINGLE", ManLead_Dat$MARITALSTATUS)]     <-  "SINGLE"
ManLead_Dat$MARITALSTATUS[grepl("ENGAGED", ManLead_Dat$MARITALSTATUS)]    <-  "ENGAGED"
ManLead_Dat$MARITALSTATUS[grepl("MARRIED", ManLead_Dat$MARITALSTATUS)]    <-  "MARRIED"
ManLead_Dat$MARITALSTATUS[grepl("SEPARATED", ManLead_Dat$MARITALSTATUS)]  <-  "DIVORCED"
ManLead_Dat$MARITALSTATUS[grepl("DIVORCED", ManLead_Dat$MARITALSTATUS)]   <-  "DIVORCED"
ManLead_Dat$MARITALSTATUS[grepl("WIDOW", ManLead_Dat$MARITALSTATUS)]      <-  "WIDOW"

DB_MS <- unique(DB_DAT$MARITALSTATUS)

ManLead_Dat$MARITALSTATUS[!(ManLead_Dat$MARITALSTATUS %in% DB_MS)] <- NA

rm(DB_MS)

#
# Clean Title 
#

ManLead_Dat$CLIENTTITLE <- gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(ManLead_Dat$CLIENTTITLE)))

ManLead_Dat$CLIENTTITLE[grepl("REV", ManLead_Dat$CLIENTTITLE)]     <-  "REV"
ManLead_Dat$CLIENTTITLE[grepl("PROF", ManLead_Dat$CLIENTTITLE)]    <-  "PROF"
ManLead_Dat$CLIENTTITLE[grepl("ADV", ManLead_Dat$CLIENTTITLE)]     <-  "ADV"
ManLead_Dat$CLIENTTITLE[grepl("DOCTOR", ManLead_Dat$CLIENTTITLE)]  <-  "DR"

DB_TL <- unique(DB_DAT$CLIENTTITLE)

ManLead_Dat$CLIENTTITLE[!(ManLead_Dat$CLIENTTITLE %in% DB_TL)] <- NA

rm(DB_TL)

#
# Clean Race and Surname
#

# Get Race Info
ManLead_Dat$CLIENTLASTNAME <- gsub(" ","", gsub("[^[:alpha:] ]", "", toupper(ManLead_Dat$CLIENTLASTNAME)))

ManLead_Dat <- merge(ManLead_Dat, Race_Data, by.x = "CLIENTLASTNAME", by.y = "SURNAME", all.x = TRUE)

ManLead_Dat$RACE[ManLead_Dat$RACE == ""] <- NA
ManLead_Dat$CULTURE[ManLead_Dat$CULTURE == ""] <- NA

rm(Race_Data)

#
# Clean Email Info
#

# Clean email info
ManLead_Dat$CLIENTEMAILADDRESS_FULL_DOMAIN <- sub(".*\\@", "", ManLead_Dat$CLIENTEMAILADDRESS)
ManLead_Dat$CLIENTEMAILADDRESS_SUB_DOMAIN  <- sub("\\..*$", "", ManLead_Dat$CLIENTEMAILADDRESS_FULL_DOMAIN)
ManLead_Dat$CLIENTEMAILADDRESS_SUB_DOMAIN  <- gsub("[^[:alpha:]]", "", toupper(ManLead_Dat$CLIENTEMAILADDRESS_SUB_DOMAIN))

ManLead_Dat$CLIENTEMAILADDRESS_EXTENTION   <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", ManLead_Dat$CLIENTEMAILADDRESS_FULL_DOMAIN)))
ManLead_Dat$CLIENTEMAILADDRESS_EXTENTION   <- gsub("[^[:alpha:]]", "", toupper(ManLead_Dat$CLIENTEMAILADDRESS_EXTENTION))

ManLead_Dat$CLIENTEMAILADDRESS_DOTS        <- as.numeric(countLetter(ManLead_Dat$CLIENTEMAILADDRESS_FULL_DOMAIN, "."))
ManLead_Dat                                <- subset(ManLead_Dat, select = -c(CLIENTEMAILADDRESS_FULL_DOMAIN, CLIENTEMAILADDRESS))

DB_EmSd <- unique(DB_DAT$CLIENTEMAILADDRESS_SUB_DOMAIN)

ManLead_Dat$CLIENTEMAILADDRESS_SUB_DOMAIN[!(ManLead_Dat$CLIENTEMAILADDRESS_SUB_DOMAIN %in% DB_EmSd)] <- "OTHER"

DB_EmExt <- unique(DB_DAT$CLIENTEMAILADDRESS_EXTENTION)

ManLead_Dat$CLIENTEMAILADDRESS_EXTENTION[!(ManLead_Dat$CLIENTEMAILADDRESS_EXTENTION %in% DB_EmExt)] <- "OTHER"

rm(DB_EmSd, DB_EmExt)

#
# Clean Occupation Info
#

ManLead_Dat$CLIENTOCCUPATIONNAME <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", ManLead_Dat$CLIENTOCCUPATIONNAME)))
ManLead_Dat$CLIENTOCCUPATIONNAME[ManLead_Dat$CLIENTOCCUPATIONNAME == ""] <- NA

DB_Occ <- unique(DB_DAT$CLIENTOCCUPATIONNAME)

ManLead_Dat$CLIENTOCCUPATIONNAME[!(ManLead_Dat$CLIENTOCCUPATIONNAME %in% DB_Occ)] <- "OTHER"

rm(DB_Occ)

#
# Clean Vehicle Use Info 
#

ManLead_Dat$VEHICLEUSE <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", ManLead_Dat$VEHICLEUSE)))
ManLead_Dat$VEHICLEUSE[ManLead_Dat$VEHICLEUSE == ""] <- NA

DB_Vuse <- unique(DB_DAT$VEHICLEUSE)

ManLead_Dat$VEHICLEUSE[!(ManLead_Dat$VEHICLEUSE %in% DB_Vuse)] <- NA

rm(DB_Vuse)

#
# Clean Finance and Insurance Company Info 
#

ManLead_Dat$DOCINSURANCECOMPANYNAME <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", ManLead_Dat$DOCINSURANCECOMPANYNAME)))
ManLead_Dat$DOCINSURANCECOMPANYNAME[ManLead_Dat$DOCINSURANCECOMPANYNAME == ""] <- NA

DB_InsCo <- unique(DB_DAT$DOCINSURANCECOMPANYNAME)

ManLead_Dat$DOCINSURANCECOMPANYNAME[!(ManLead_Dat$DOCINSURANCECOMPANYNAME %in% DB_InsCo)] <- "OTHER"

ManLead_Dat$DOCFINANCECOMPANYNAME <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", ManLead_Dat$DOCFINANCECOMPANYNAME)))
ManLead_Dat$DOCFINANCECOMPANYNAME[ManLead_Dat$DOCFINANCECOMPANYNAME == ""] <- NA

DB_FinCo <- unique(DB_DAT$DOCFINANCECOMPANYNAME)

ManLead_Dat$DOCFINANCECOMPANYNAME[!(ManLead_Dat$DOCFINANCECOMPANYNAME %in% DB_FinCo)] <- "OTHER"

rm(DB_InsCo, DB_FinCo)

#
# Clean Client Banking Info 
#

ManLead_Dat$CLIENTBANKNAME <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", ManLead_Dat$CLIENTBANKNAME)))
ManLead_Dat$CLIENTBANKNAME[ManLead_Dat$CLIENTBANKNAME == ""] <- NA

DB_CBa <- unique(DB_DAT$CLIENTBANKNAME)

ManLead_Dat$CLIENTBANKNAME[!(ManLead_Dat$CLIENTBANKNAME %in% DB_CBa)] <- "OTHER"

ManLead_Dat$CLIENTBANKBRANCH <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", ManLead_Dat$CLIENTBANKBRANCH)))
ManLead_Dat$CLIENTBANKBRANCH[ManLead_Dat$CLIENTBANKBRANCH == ""] <- NA
ManLead_Dat$CLIENTBANKBRANCH <- ifelse(ManLead_Dat$CLIENTBANKBRANCH != "", paste(ManLead_Dat$CLIENTBANKNAME, ManLead_Dat$CLIENTBANKBRANCH, sep = "_"), "")

DB_CBr <- unique(DB_DAT$CLIENTBANKBRANCH)

ManLead_Dat$CLIENTBANKBRANCH[!(ManLead_Dat$CLIENTBANKBRANCH %in% DB_CBr)] <- "OTHER"

ManLead_Dat$CLIENTBANKACCOUNTTYPE <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", ManLead_Dat$CLIENTBANKACCOUNTTYPE)))
ManLead_Dat$CLIENTBANKACCOUNTTYPE[ManLead_Dat$CLIENTBANKACCOUNTTYPE == ""] <- NA

DB_CBt <- unique(DB_DAT$CLIENTBANKACCOUNTTYPE)

ManLead_Dat$CLIENTBANKACCOUNTTYPE[!(ManLead_Dat$CLIENTBANKACCOUNTTYPE %in% DB_CBt)] <- "OTHER"

rm(DB_CBr, DB_CBa, DB_CBt)

#
# Clean Branch and Salesman Info
#

ManLead_Dat$BRANCHNAME <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", ManLead_Dat$BRANCHNAME)))

ManLead_Dat$SALESPERSON <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", ManLead_Dat$SALESPERSON)))

ManLead_Dat$SALESPERSON <- ifelse(ManLead_Dat$SALESPERSON != "", paste(ManLead_Dat$BRANCHNAME, ManLead_Dat$SALESPERSON, sep = "_"), "")

ManLead_Dat$SALESPERSON[ManLead_Dat$SALESPERSON == ""] <- NA
ManLead_Dat$BRANCHNAME[ManLead_Dat$BRANCHNAME == ""]   <- NA

DB_Sp <- unique(DB_DAT$SALESPERSON)

ManLead_Dat$SALESPERSON[!(ManLead_Dat$SALESPERSON %in% DB_Sp)] <- "OTHER"

DB_Bn <- unique(DB_DAT$BRANCHNAME)

ManLead_Dat$BRANCHNAME[!(ManLead_Dat$BRANCHNAME %in% DB_Bn)] <- "OTHER"

rm(DB_Bn)

#
# Clean All Numeric Variables 
#

Options <- c("FINANCETERM", "VEHICLEVALUE", "DEPOSITVALUE", "RESIDUALVALUE", "FINANCEAMOUNT", "ODOMETERREADING")

for (opt in Options) {
  ManLead_Dat[[opt]]                        <-  as.numeric(ManLead_Dat[[opt]])
  ManLead_Dat[[opt]][is.na(ManLead_Dat[[opt]])]  <-  0
}

rm(Options, opt)

#
# Clean Year Model Variables 
#

# Diff between car model year and purchase year
YearDf <- strsplit(UniDash(ManLead_Dat$FIRSTREGISTRATIONYEAR), "-")

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

ManLead_Dat$FIRSTREGISTRATIONYEAR <- as.numeric(yyFin)

# Vehicle age at purchase
ManLead_Dat$VEHICLEAGE <- as.numeric(format(ManLead_Dat$INCEPTIONDATE, "%Y")) - ManLead_Dat$FIRSTREGISTRATIONYEAR

# Note - the negative one values (-1) is due to cars being bought at the end of the year (hence bought in 2015/12/01, thus it is
#                                                                                         classified as a 2016 model : age = -1)

rm(n, l, YearDf, yyFin, i, yyPrt)

#
# Clean Car Model Variables
#

ModDf <- strsplit(ManLead_Dat$MODEL, " ")

n <- max(sapply(ModDf, length))
l <- lapply(ModDf, function(X) c(X, rep(NA, n - length(X))))

ModDf <- data.frame(t(do.call(cbind, l)), stringsAsFactors = FALSE)
colnames(ModDf) <- paste("Model", seq(1:ncol(ModDf)), sep = "")

ModDf[[1]][is.na(ModDf[[1]])] <- ""
ModDf[[2]][is.na(ModDf[[2]])] <- ""

ManLead_Dat$MANUFACTURER <- ModDf[[1]]
ManLead_Dat$MODEL        <- paste(ModDf[[1]], ModDf[[2]], sep = "")

ManLead_Dat$MANUFACTURER[ManLead_Dat$MANUFACTURER == ""] <- NA
ManLead_Dat$MODEL[ManLead_Dat$MODEL == ""]               <- NA

ManLead_Dat$MANUFACTURER <- gsub("[^[:alnum:] ]", "", toupper(gsub("^.*?\\.", "", ManLead_Dat$MANUFACTURER))) 
ManLead_Dat$MODEL <- gsub("[^[:alnum:] ]", "", toupper(gsub("^.*?\\.", "", ManLead_Dat$MODEL))) 

rm(n, l, ModDf)

DB_Mod <- unique(DB_DAT$MODEL)

ManLead_Dat$MODEL[!(ManLead_Dat$MODEL %in% DB_Mod)] <- "OTHER"

DB_Man <- unique(DB_DAT$MANUFACTURER)

ManLead_Dat$MANUFACTURER[!(ManLead_Dat$MANUFACTURER %in% DB_Man)] <- "OTHER"

rm(DB_Mod, DB_Man)

#
# Count Number Of Accessories 
#

ManLead_Dat$ACCESSORIES <- str_count(ManLead_Dat$ACCESSORIES, ";")

#
# Remove Non Model Data 
#

ManLead_Dat <- subset(ManLead_Dat, select = -c(CLIENTIDNUMBER, CLIENTLASTNAME))

#
# Clean Address Info 
#

# Clean province info
Provinces <- c("WESTERNCAPE", "WESTERNPROVINCE", "LIMPOPO", "MPUMALANGA", "GAUTENG", 
               "EASTERNCAPE", "KWAZULUNATAL", "NORTHERNCAPE", "FREESTATE", "NORTHWEST")

AdrDf <- strsplit(ManLead_Dat$CLIENTFULLPOSTAL, " ")

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

ManLead_Dat$CLIENTPOSTALADDRESSPOSTALCODE <- PcodeFin[(1:NROW(AdrDf)) + NROW(AdrDf) * (PostCol - 1)]
ManLead_Dat$CLIENTPOSTALADDRESSPOSTALCODE <- as.character(as.numeric(ManLead_Dat$CLIENTPOSTALADDRESSPOSTALCODE))
City_Post_Data$POSTALCODE <- as.character(as.numeric(City_Post_Data$POSTALCODE))

ManLead_Dat <- merge(ManLead_Dat, City_Post_Data, by.x = "CLIENTPOSTALADDRESSPOSTALCODE", by.y = "POSTALCODE", all.x = TRUE)

rm(Provinces, City_Data, AdrDf, n, l, tempDf, i, PostCol, PcodeFin, City_Post_Data, City_Prov_Dat, PostalDF, ProvinceDat, Pcode)

ManLead_Dat <- subset(ManLead_Dat, select = -CLIENTFULLPOSTAL)

colnames(ManLead_Dat)[colnames(ManLead_Dat) == "CITY"]      <-  "CLIENTPOSTALADDRESSCITY"
colnames(ManLead_Dat)[colnames(ManLead_Dat) == "PROVINCE"]  <-  "CLIENTPOSTALADDRESSPROVINCE"
colnames(ManLead_Dat)[colnames(ManLead_Dat) == "SUBURB"]    <-  "CLIENTPOSTALADDRESSSUBURB"

DB_City <- unique(DB_DAT$CLIENTPOSTALADDRESSCITY)

ManLead_Dat$CLIENTPOSTALADDRESSCITY[!(ManLead_Dat$CLIENTPOSTALADDRESSCITY %in% DB_City)] <- "OTHER"

DB_PCode <- unique(DB_DAT$CLIENTPOSTALADDRESSPOSTALCODE)

ManLead_Dat$CLIENTPOSTALADDRESSPOSTALCODE[!(ManLead_Dat$CLIENTPOSTALADDRESSPOSTALCODE %in% DB_PCode)] <- "OTHER"

DB_PSub <- unique(DB_DAT$CLIENTPOSTALADDRESSSUBURB)

ManLead_Dat$CLIENTPOSTALADDRESSSUBURB[!(ManLead_Dat$CLIENTPOSTALADDRESSSUBURB %in% DB_PSub)] <- "OTHER"

rm(DB_City, DB_PSub, DB_PCode)

ManLead_Dat  <- subset(ManLead_Dat,  select = -c(INCEPTIONDATE, CLIENTBIRTHDATE, CLIENTBANKNAME, CLIENTBANKACCOUNTTYPE, CLIENTBANKBRANCH))

response.names <- "STATUS"

feature.names <- colnames(ManLead_Dat)
feature.names <- feature.names[feature.names != response.names]

# put IDs for text variables - replace missings by 0 - boosting runs faster
for (f in feature.names) {
  if (class(ManLead_Dat[[f]]) == "character") {
    levels <- unique(c(ManLead_Dat[[f]]))
    ManLead_Dat[[f]] <- factor(ManLead_Dat[[f]], levels = levels)
  }
  else if (class(train[[f]]) == "integer" | class(train[[f]]) == "numeric"){
    ManLead_Dat[[f]] <- ifelse(is.na(ManLead_Dat[[f]]), 0, ManLead_Dat[[f]])
  }
}

# Allocate ----------------------------------------------------------------

Allocation_Dat <- separate(Allocation_Dat, col = Team, into = c("Aff1", "Aff2"))
Allocation_Dat$Aff1 <- gsub(" ", "", gsub("[^[:alpha:] ]", "", toupper(Allocation_Dat$Aff1)))
Allocation_Dat$Aff2 <- gsub(" ", "", gsub("[^[:alpha:] ]", "", toupper(Allocation_Dat$Aff2)))

Allocation_Dat$ZLAGENT <- gsub(" ", "", gsub("[^[:alpha:] ]", "", toupper(Allocation_Dat$ZwingMaster)))

ManLead_Dat$AFFINITY <- gsub(" ", "", gsub("[^[:alpha:] ]", "", toupper(ManLead_Dat$AFFINITY)))

Affins <- c(unique(Allocation_Dat$Aff2), unique(Allocation_Dat$Aff1)) 
Affins <- Affins[!is.na(Affins)]
Affins[Affins == "BARLOW"] <- "BARLOWORLD"

ManLead_Dat$TempAFFINITY <- ManLead_Dat$AFFINITY
ManLead_Dat$TempAFFINITY[!(ManLead_Dat$TempAFFINITY %in% Affins)] <- "OTHER"

ManLead_Dat$ID <- seq(1:nrow(ManLead_Dat))

TotBar   <- sum(Allocation_Dat$Barlow, na.rm = TRUE)
BarCount <- 0

LeadOut <- data.frame(ZLAGENT = as.character(),
                      Pred    = as.numeric(),
                      ID      = as.integer())

for (aff in Affins) {
  
  SubsetData <- ManLead_Dat[grepl(aff, ManLead_Dat$TempAFFINITY), ]

  Agents <- c(unique(Allocation_Dat$ZLAGENT[Allocation_Dat$Aff1 == aff]), unique(Allocation_Dat$ZLAGENT[Allocation_Dat$Aff2 == aff]))
  Agents <- Agents[!is.na(Agents)]
  
  if (grepl("BARLOW", aff)) {

    # Get all barlow agents
    Bar_Agents <- unique(Allocation_Dat$ZLAGENT[!is.na(Allocation_Dat$Barlow)])
    
    # Assing until Barlow cap is reached - also add to total cap
    while (BarCount < TotBar + 1) {
      
      for (agent in Agents) {
        
        SubsetData$ZLAGENT <- agent
        Preds <- predict(object = Model,
                         newdata = SubsetData,
                         n.trees = gbm.perf(Model),
                         type = "response")
        SubsetData$Pred <- Preds
        
        MaxID <- SubsetData$ID[which(SubsetData$Pred == max(SubsetData$Pred))][1]
        
        UpdateR <- data.frame(ZLAGENT = Allocation_Dat$ZwingMaster[Allocation_Dat$ZLAGENT == agent],
                              Pred    = SubsetData$Pred[SubsetData$ID == MaxID],
                              ID      = MaxID)
        
        LeadOut <- rbind(LeadOut, UpdateR)
        
        SubsetData <- SubsetData[SubsetData$ID != MaxID, ]
        
        Allocation_Dat$Barlow[Allocation_Dat$ZLAGENT == agent] <- Allocation_Dat$Barlow[Allocation_Dat$ZLAGENT == agent] - 1
        
        # Remove agent if maxed allocation reached
        Agents <- unique(Allocation_Dat$ZLAGENT[!is.na(Allocation_Dat$Barlow) & Allocation_Dat$Barlow != 0])
        
        
        BarCount <- BarCount + 1
      
        print(BarCount)
        
      }
      
    }
    
  }
  
  for (agent in Agents) {
    
    ManLead_Dat$ZLAGENT <- agent
    Preds <- predict(object = Model,
                     newdata = ManLead_Dat,
                     n.trees = gbm.perf(Model),
                     type = "response")
    ManLead_Dat$Pred <- Preds
    
    LeadOut$ZLAGENT[which(ManLead_Dat$Pred == max(ManLead_Dat$Pred))] <- Allocation_Dat$ZwingMaster[Allocation_Dat$ZLAGENT == agent]
    LeadOut$Pred[which(ManLead_Dat$Pred == max(ManLead_Dat$Pred))] <- ManLead_Dat$Pred[which(ManLead_Dat$Pred == max(ManLead_Dat$Pred))]
    
    Allocation_Dat$Counter[Allocation_Dat$ZLAGENT == agent] <- Allocation_Dat$Counter[Allocation_Dat$ZLAGENT == agent] + 1
    
  }
  
}


ManLead_Dat <- merge(ManLead_Dat, LeadOut, by.x = "ID", by.y = "ID", all.x = TRUE)

LeadOut %>% group_by(ZLAGENT) %>% summarise(n = n())





# Per Agent Analysis ------------------------------------------------------

All_Agents <- unique(Allocation_Dat$ZLAGENT)

orig_rows <- nrow(ManLead_Dat)
per_agent <- floor(orig_rows/length(All_Agents))
ManLead_Dat$ID <- seq(1:nrow(ManLead_Dat))
agents_w_counts <- data.frame(Agents = All_Agents)
agents_w_counts$Counts <- 0

test3 <- ManLead_Dat

for (i in 1:length(All_Agents)){
  
  test2 <- test3
  test2 <- test2[rep(seq_len(nrow(test2)), each = length(All_Agents)), ]
  
  All_Agents <- rep(All_Agents, nrow(test3))
  test2$ZLAGENT <- as.factor(All_Agents)
  
  TestPred2 <- predict(object = Model,
                       newdata = test2,
                       n.trees = gbm.perf(Model),
                       type = "response")
  
  test2$Pred2 <- TestPred2
  
  test2 <- test2[with(test2, order(-Pred2)), ]
  
  test2 <- test2[!duplicated(test2$ID),]  
  
  tempfinDF <- test2 %>% 
    group_by(ZLAGENT) %>%
    top_n(per_agent, wt = Pred2)
  
  ff <- data.frame(table(tempfinDF$ZLAGENT))
  ff <- ff[ff$Freq != 0, ]
  
  agents_w_counts <- merge(agents_w_counts, ff, by.x = "Agents", by.y = "Var1", all.x = TRUE)
  agents_w_counts$Freq[is.na(agents_w_counts$Freq)] <- 0
  agents_w_counts$Counts <- agents_w_counts$Counts + agents_w_counts$Freq
  agents_w_counts <- subset(agents_w_counts, select = -Freq)
  
  rem_agents <- agents_w_counts$Agents[agents_w_counts$Counts >= per_agent]
  
  All_Agents <- unique(DB_DAT$ZLAGENT)[!(unique(DB_DAT$ZLAGENT) %in% rem_agents)]
  
  test3 <- test2[!(test2$ID %in% tempfinDF$ID), ]
  test3 <- subset(test3, select = -Pred2)
  
  if (i == 1) {
    finDF <- tempfinDF
  } else {
    finDF <- rbind(finDF, tempfinDF)
  }
  
  print(i)
  
}

ff <- data.frame(table(finDF$ZLAGENT))















