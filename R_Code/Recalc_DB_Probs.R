# Load Data ---------------------------------------------------------------

# Load current model
load(paste(getwd(), "/Active_Model.RData", sep = ""))

# Loads City/Race/ etc data - For Allocation and Modelling
source(paste(Path, "/R_Code/Load_Other_Data.R", sep = ""))

# Opens DB
source(paste(Path, "/R_Code/OpenDB.R", sep = ""))

today <- as.character(Sys.Date())

ntrees <- max(Model$trees.fitted)
# Loads manual allocation Data
DB_Names <- read_excel(paste(Path, "/Data/Lead_Col_Names/DBNAMES.xlsx", sep = ""),
                       sheet = 1,
                       col_names = TRUE)

query <- paste("SELECT * ",
               "FROM AccessLife_Sales_File_Lead_Data ",
               "WHERE `Status` = 'Allocated' ",
               "AND `First Allocation Date` BETWEEN '", as.Date(today) - months(6), "' AND '", as.Date(today),"' ",
               "AND `UW Status` IS NULL AND `QA Status` IS NULL",
               sep = "")

ManLead_Dat <- dbGetQuery(mydb, query)

colnames(ManLead_Dat)  <- gsub(" ", "", gsub("[^[:alnum:] ]", "", gsub("X.", "", toupper(colnames(ManLead_Dat)))))
DB_Names$Original      <- gsub(" ", "", gsub("[^[:alnum:] ]", "", gsub("X.", "", toupper(DB_Names$Original))))

ManLead_Dat <- ManLead_Dat[, colnames(ManLead_Dat) %in% DB_Names$Original]
colnames(ManLead_Dat) <- DB_Names$New[match(colnames(ManLead_Dat), DB_Names$Original)]

ManLead_Dat$ID <- seq(1:nrow(ManLead_Dat))

ManLead_Dat_Orig <- ManLead_Dat

# Prepare Model Data ------------------------------------------------------

# Drop useless columns
ManLead_Dat$CLIENTFULLPOSTAL <- paste(ManLead_Dat$CLIENTPOSTALADDRESS1, 
                                      ManLead_Dat$CLIENTPOSTALADDRESS2, 
                                      ManLead_Dat$CLIENTPOSTALADDRESS3, 
                                      ManLead_Dat$CLIENTPOSTALADDRESS4,
                                      ManLead_Dat$CLIENTPOSTALADDRESSPOSTALCODE,
                                      sep = " ")

ManLead_Dat <- subset(ManLead_Dat, select = -c(CLIENTWORKTELEPHONENUMBER, CLIENTHOMETELEPHONENUMBER, CLIENTMOBILENUMBER,
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
# Continue Cleaning -------------------------------------------------------

ManLead_Dat <- subset(ManLead_Dat, select = -LEADDATE)

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
ValidOnCount[is.na(ValidOnCount)] <- FALSE

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

ManLead_Dat$CLIENTIDTYPE[ValidOnAll] <- "RSA ID"

# Clean Birthdate
Cur_Year <- as.numeric(substr(format(Sys.Date(), "%Y"), 3, 4))

B_Days <- data.frame(YYYY = ifelse(YY <= Cur_Year, as.numeric(paste(20, YY, sep = "")), as.numeric(paste(19, YY, sep = ""))),
                     MM   = MM,
                     DD   = DD)
B_Days <- DateConv(paste(B_Days$YYYY, B_Days$MM, B_Days$DD, sep = "-"))

Orig_B_Days <- DateConv(ManLead_Dat$CLIENTBIRTHDATE)

ManLead_Dat$CLIENTBIRTHDATE <- B_Days
ManLead_Dat$CLIENTBIRTHDATE[is.na(ManLead_Dat$CLIENTBIRTHDATE)] <- Orig_B_Days[is.na(ManLead_Dat$CLIENTBIRTHDATE)]

rm(YY, MM, DD, G, SSS, C, A, Z, LuhnVal, ValidOnCount, ValidOnMost, ValidOnAll, Cur_Year, Orig_B_Days, B_Days, Odd_Sum, Even_Sum)

# Get Age at purchase date
ManLead_Dat$CLIENTBIRTHDATE[ManLead_Dat$CLIENTBIRTHDATE == as.Date("1899-12-30")] <- NA
ManLead_Dat$CLIENTAGE <- as.numeric(ManLead_Dat$INCEPTIONDATE - ManLead_Dat$CLIENTBIRTHDATE) / 365.25 

# Cutting off the max and min Age (from model)
ManLead_Dat$CLIENTAGE[ManLead_Dat$CLIENTAGE < Model$var.levels[which(Model$var.names == "CLIENTAGE")][[1]][1]]  <- Model$var.levels[which(Model$var.names == "CLIENTAGE")][[1]][1]
ManLead_Dat$CLIENTAGE[ManLead_Dat$CLIENTAGE > Model$var.levels[which(Model$var.names == "CLIENTAGE")][[1]][11]] <- Model$var.levels[which(Model$var.names == "CLIENTAGE")][[1]][11]

ManLead_Dat$TooYoung <- 0
ManLead_Dat$TooYoung[ManLead_Dat$CLIENTAGE < 17] <- 1

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

DB_MS <- Model$var.levels[which(Model$var.names == "MARITALSTATUS")][[1]]

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

DB_TL <- Model$var.levels[which(Model$var.names == "CLIENTTITLE")][[1]]

ManLead_Dat$CLIENTTITLE[!(ManLead_Dat$CLIENTTITLE %in% DB_TL)] <- NA

rm(DB_TL)

#
# Clean Race and Surname
#

# Get Race Info
ManLead_Dat$CLIENTLASTNAME <- gsub(" ","", gsub("[^[:alpha:] ]", "", toupper(ManLead_Dat$CLIENTLASTNAME)))

ManLead_Dat <- merge(ManLead_Dat, Race_Data, by.x = "CLIENTLASTNAME", by.y = "SURNAME", all.x = TRUE)

colnames(ManLead_Dat)[colnames(ManLead_Dat) == "RACE"]    <- "RACE_SN"
colnames(ManLead_Dat)[colnames(ManLead_Dat) == "CULTURE"] <- "CULTURE_SN"

ManLead_Dat <- merge(ManLead_Dat, Race_Data, by.x = "CLIENTFIRSTNAME", by.y = "SURNAME", all.x = TRUE)

colnames(ManLead_Dat)[colnames(ManLead_Dat) == "RACE"]    <- "RACE_FN"
colnames(ManLead_Dat)[colnames(ManLead_Dat) == "CULTURE"] <- "CULTURE_FN"

ManLead_Dat$RACE_SN[is.na(ManLead_Dat$RACE_SN)]       <- ManLead_Dat$RACE_FN[is.na(ManLead_Dat$RACE_SN)]
ManLead_Dat$CULTURE_SN[is.na(ManLead_Dat$CULTURE_SN)] <- ManLead_Dat$RACE_FN[is.na(ManLead_Dat$CULTURE_SN)]

ManLead_Dat <- subset(ManLead_Dat, select = -c(CLIENTFIRSTNAME, RACE_FN, CULTURE_FN))

colnames(ManLead_Dat)[colnames(ManLead_Dat) == "RACE_SN"]    <- "RACE"
colnames(ManLead_Dat)[colnames(ManLead_Dat) == "CULTURE_SN"] <- "CULTURE"

DB_Ra <- Model$var.levels[which(Model$var.names == "RACE")][[1]]
DB_Cu <- Model$var.levels[which(Model$var.names == "CULTURE")][[1]]

ManLead_Dat$RACE[!(ManLead_Dat$RACE %in% DB_Ra)]       <- NA
ManLead_Dat$CULTURE[!(ManLead_Dat$CULTURE %in% DB_Cu)] <- NA

rm(Race_Data, DB_Ra, DB_Cu)

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

DB_EmSd <- Model$var.levels[which(Model$var.names == "CLIENTEMAILADDRESS_SUB_DOMAIN")][[1]]

ManLead_Dat$CLIENTEMAILADDRESS_SUB_DOMAIN[!(ManLead_Dat$CLIENTEMAILADDRESS_SUB_DOMAIN %in% DB_EmSd)] <- "OTHER"

DB_EmExt <- Model$var.levels[which(Model$var.names == "CLIENTEMAILADDRESS_EXTENTION")][[1]]

ManLead_Dat$CLIENTEMAILADDRESS_EXTENTION[!(ManLead_Dat$CLIENTEMAILADDRESS_EXTENTION %in% DB_EmExt)] <- "OTHER"

rm(DB_EmSd, DB_EmExt)

#
# Clean Occupation Info
#

ManLead_Dat$CLIENTOCCUPATIONNAME <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", ManLead_Dat$CLIENTOCCUPATIONNAME)))
ManLead_Dat$CLIENTOCCUPATIONNAME[ManLead_Dat$CLIENTOCCUPATIONNAME == ""] <- NA

DB_Occ <- Model$var.levels[which(Model$var.names == "CLIENTOCCUPATIONNAME")][[1]]

ManLead_Dat$CLIENTOCCUPATIONNAME[!(ManLead_Dat$CLIENTOCCUPATIONNAME %in% DB_Occ)] <- "OTHER"

rm(DB_Occ)

#
# Clean Vehicle Use Info 
#

ManLead_Dat$VEHICLEUSE <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", ManLead_Dat$VEHICLEUSE)))
ManLead_Dat$VEHICLEUSE[ManLead_Dat$VEHICLEUSE == ""] <- NA

DB_Vuse <- Model$var.levels[which(Model$var.names == "VEHICLEUSE")][[1]]

ManLead_Dat$VEHICLEUSE[!(ManLead_Dat$VEHICLEUSE %in% DB_Vuse)] <- NA

rm(DB_Vuse)

#
# Clean Finance and Insurance Company Info 
#

ManLead_Dat$DOCINSURANCECOMPANYNAME <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", ManLead_Dat$DOCINSURANCECOMPANYNAME)))
ManLead_Dat$DOCINSURANCECOMPANYNAME[ManLead_Dat$DOCINSURANCECOMPANYNAME == ""] <- NA

DB_InsCo <- Model$var.levels[which(Model$var.names == "DOCINSURANCECOMPANYNAME")][[1]]

ManLead_Dat$DOCINSURANCECOMPANYNAME[!(ManLead_Dat$DOCINSURANCECOMPANYNAME %in% DB_InsCo)] <- "OTHER"

ManLead_Dat$DOCFINANCECOMPANYNAME <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", ManLead_Dat$DOCFINANCECOMPANYNAME)))
ManLead_Dat$DOCFINANCECOMPANYNAME[ManLead_Dat$DOCFINANCECOMPANYNAME == ""] <- NA

DB_FinCo <- Model$var.levels[which(Model$var.names == "DOCFINANCECOMPANYNAME")][[1]]

ManLead_Dat$DOCFINANCECOMPANYNAME[!(ManLead_Dat$DOCFINANCECOMPANYNAME %in% DB_FinCo)] <- "OTHER"

rm(DB_InsCo, DB_FinCo)

#
# Clean Branch and Salesman Info
#

ManLead_Dat$BRANCHNAME <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", ManLead_Dat$BRANCHNAME)))

ManLead_Dat$SALESPERSON <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", ManLead_Dat$SALESPERSON)))

ManLead_Dat$SALESPERSON <- ifelse(ManLead_Dat$SALESPERSON != "", paste(ManLead_Dat$BRANCHNAME, ManLead_Dat$SALESPERSON, sep = "_"), "")

ManLead_Dat$SALESPERSON[ManLead_Dat$SALESPERSON == ""] <- NA
ManLead_Dat$BRANCHNAME[ManLead_Dat$BRANCHNAME == ""]   <- NA

if (length(which(Model$var.names == "SALESPERSON")) == 0) {
  ManLead_Dat <- subset(ManLead_Dat, select = -SALESPERSON)
} else {
  DB_Sp <- Model$var.levels[which(Model$var.names == "SALESPERSON")][[1]]
  
  ManLead_Dat$SALESPERSON[!(ManLead_Dat$SALESPERSON %in% DB_Sp)] <- "OTHER"
  rm(DB_Sp)
}

DB_Bn <- Model$var.levels[which(Model$var.names == "BRANCHNAME")][[1]]

ManLead_Dat$BRANCHNAME[!(ManLead_Dat$BRANCHNAME %in% DB_Bn)] <- "OTHER"

rm(DB_Bn)

# Fix time to call
ManLead_Dat$TIMETOCALL     <- as.numeric(as.Date(today) - ManLead_Dat$INCEPTIONDATE)

ManLead_Dat <- subset(ManLead_Dat, select = -INCEPTIONDATE)


#
# Clean All Numeric Variables 
#

Options <- c("FINANCETERM", "VEHICLEVALUE", "DEPOSITVALUE", "RESIDUALVALUE", "FINANCEAMOUNT", "ODOMETERREADING", "TIMETOCALL")

for (opt in Options) {
  ManLead_Dat[[opt]]                             <-  as.numeric(ManLead_Dat[[opt]])
  ManLead_Dat[[opt]][is.na(ManLead_Dat[[opt]])]  <-  0
  
  ManLead_Dat[[opt]][ManLead_Dat[[opt]] < Model$var.levels[which(Model$var.names == opt)][[1]][1]]  <- Model$var.levels[which(Model$var.names == opt)][[1]][1]
  ManLead_Dat[[opt]][ManLead_Dat[[opt]] > Model$var.levels[which(Model$var.names == opt)][[1]][11]] <- Model$var.levels[which(Model$var.names == opt)][[1]][1]
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

ManLead_Dat$FIRSTREGISTRATIONYEAR[ManLead_Dat$FIRSTREGISTRATIONYEAR < Model$var.levels[which(Model$var.names == "FIRSTREGISTRATIONYEAR")][[1]][1]]  <- Model$var.levels[which(Model$var.names == "FIRSTREGISTRATIONYEAR")][[1]][1]
ManLead_Dat$FIRSTREGISTRATIONYEAR[ManLead_Dat$FIRSTREGISTRATIONYEAR > Model$var.levels[which(Model$var.names == "FIRSTREGISTRATIONYEAR")][[1]][11]] <- Model$var.levels[which(Model$var.names == "FIRSTREGISTRATIONYEAR")][[1]][11]

# Vehicle age at purchase
ManLead_Dat$VEHICLEAGE <- as.numeric(format(ManLead_Dat$INCEPTIONDATE, "%Y")) - ManLead_Dat$FIRSTREGISTRATIONYEAR

ManLead_Dat$VEHICLEAGE[ManLead_Dat$VEHICLEAGE < Model$var.levels[which(Model$var.names == "VEHICLEAGE")][[1]][1]]  <- Model$var.levels[which(Model$var.names == "VEHICLEAGE")][[1]][1]
ManLead_Dat$VEHICLEAGE[ManLead_Dat$VEHICLEAGE > Model$var.levels[which(Model$var.names == "VEHICLEAGE")][[1]][11]] <- Model$var.levels[which(Model$var.names == "VEHICLEAGE")][[1]][11]

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

DB_Mod <- Model$var.levels[which(Model$var.names == "MODEL")][[1]]

ManLead_Dat$MODEL[!(ManLead_Dat$MODEL %in% DB_Mod)] <- "OTHER"

DB_Man <- Model$var.levels[which(Model$var.names == "MANUFACTURER")][[1]]

ManLead_Dat$MANUFACTURER[!(ManLead_Dat$MANUFACTURER %in% DB_Man)] <- "OTHER"

rm(DB_Mod, DB_Man)

#
# Count Number Of Accessories 
#

ManLead_Dat$ACCESSORIES <- str_count(ManLead_Dat$ACCESSORIES, ";")

ManLead_Dat$ACCESSORIES[ManLead_Dat$ACCESSORIES < Model$var.levels[which(Model$var.names == "ACCESSORIES")][[1]][1]]  <- Model$var.levels[which(Model$var.names == "ACCESSORIES")][[1]][1]
ManLead_Dat$ACCESSORIES[ManLead_Dat$ACCESSORIES > Model$var.levels[which(Model$var.names == "ACCESSORIES")][[1]][11]] <- Model$var.levels[which(Model$var.names == "ACCESSORIES")][[1]][11]


#
# Remove Non Model Data 
#

ManLead_Dat <- subset(ManLead_Dat, select = -c(CLIENTLASTNAME))

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

DB_City <- Model$var.levels[which(Model$var.names == "CLIENTPOSTALADDRESSCITY")][[1]]

ManLead_Dat$CLIENTPOSTALADDRESSCITY[!(ManLead_Dat$CLIENTPOSTALADDRESSCITY %in% DB_City)] <- "OTHER"

DB_PCode <- Model$var.levels[which(Model$var.names == "CLIENTPOSTALADDRESSPOSTALCODE")][[1]]

ManLead_Dat$CLIENTPOSTALADDRESSPOSTALCODE[!(ManLead_Dat$CLIENTPOSTALADDRESSPOSTALCODE %in% DB_PCode)] <- "OTHER"

DB_PSub <- Model$var.levels[which(Model$var.names == "CLIENTPOSTALADDRESSSUBURB")][[1]]

ManLead_Dat$CLIENTPOSTALADDRESSSUBURB[!(ManLead_Dat$CLIENTPOSTALADDRESSSUBURB %in% DB_PSub)] <- "OTHER"

rm(DB_City, DB_PSub, DB_PCode)

ManLead_Dat  <- subset(ManLead_Dat,  select = -c(CLIENTBIRTHDATE, CLIENTBANKNAME, CLIENTBANKACCOUNTTYPE, CLIENTBANKBRANCH))

ManLead_Dat$AFFINITY <- gsub(" ", "", gsub("[^[:alpha:] ]", "", toupper(ManLead_Dat$AFFINITY)))

response.names <- "STATUS"

# Clean Agent details
ManLead_Dat$ZLAGENT <- gsub(" ", "", gsub("[^[:alpha:] ]", "", toupper(ManLead_Dat$ZLAGENT)))

# Remove useless columns
ManLead_Dat <- subset(ManLead_Dat, select = -c(FIRSTALLOCATIONDATE))

# Extract model levels only
feature.names <- colnames(ManLead_Dat)
feature.names <- feature.names[feature.names != response.names]
feature.names <- feature.names[feature.names != "CLIENTIDNUMBER"]

for (f in feature.names) {
  if (class(ManLead_Dat[[f]]) == "character") {
    levels <- Model$var.levels[which(Model$var.names == f)][[1]]
    #    levels <- unique(c(ManLead_Dat[[f]]))
    ManLead_Dat[[f]] <- factor(ManLead_Dat[[f]], levels = levels)
  }
  else if (class(ManLead_Dat[[f]]) == "integer" | class(ManLead_Dat[[f]]) == "numeric"){
    ManLead_Dat[[f]] <- ifelse(is.na(ManLead_Dat[[f]]), 0, ManLead_Dat[[f]])
  }
}

ManLead_Dat$AFFINITY <- as.character(ManLead_Dat$AFFINITY)

#########################################################################

ManLead_Dat$LEADPICKUPDATE <- as.Date(today)

#########################################################################

ManLead_Dat$LEADPICKUPTIME <-  as.numeric(format(round(Sys.time(), units = "hours"), format = "%H"))

#########################################################################

ManLead_Dat$DAYTIME <- "Morning"
ManLead_Dat$DAYTIME[ManLead_Dat$LEADPICKUPTIME %in% 12:14] <- "Lunch"
ManLead_Dat$DAYTIME[ManLead_Dat$LEADPICKUPTIME %in% 15:18] <- "Midday"
ManLead_Dat$DAYTIME[ManLead_Dat$LEADPICKUPTIME %in% 19:24] <- "Night"

#########################################################################

ManLead_Dat$WEEKDAY  <- weekdays(as.Date(today))

#########################################################################

ManLead_Dat$WEEKEND  <- "Weekday"
ManLead_Dat$WEEKEND[ManLead_Dat$WEEKDAY == "Saturday" | ManLead_Dat$WEEKDAY == "Sunday"]  <- "Weekend"

#########################################################################

ManLead_Dat$WEEKTIME <- "Early"
ManLead_Dat$WEEKTIME[ManLead_Dat$WEEKDAY == "Wednesday"]  <- "Mid"
ManLead_Dat$WEEKTIME[ManLead_Dat$WEEKDAY == "Thursday" | ManLead_Dat$WEEKDAY == "Friday"]  <- "Late"
ManLead_Dat$WEEKTIME[ManLead_Dat$WEEKDAY == "Saturday" | ManLead_Dat$WEEKDAY == "Sunday"]  <- "Weekend"

#########################################################################

EnricoURL   <- paste("http://kayaposoft.com/enrico/json/v1.0/index.php?action=getPublicHolidaysForDateRange&fromDate=01-01-2013&toDate=31-12-",
                     format(as.Date(today),"%Y"), 
                     "&country=zaf&region=all",
                     sep = "")
Enrico_data <- fromJSON(EnricoURL)

Enrico_data$date$month <- str_pad(Enrico_data$date$month, width = 2, side = "left", pad = "0")
Enrico_data$date$day   <- str_pad(Enrico_data$date$day,   width = 2, side = "left", pad = "0")

Enrico_data$Clean_Date <- as.Date(paste(Enrico_data$date$year, Enrico_data$date$month, Enrico_data$date$day, sep = "-"))
Enrico_data$PUBHOLIDAY <- "Public_Holiday"

Enrico_data <- subset(Enrico_data, select = c(Clean_Date, PUBHOLIDAY))

ManLead_Dat <- merge(ManLead_Dat, Enrico_data, by.x = "LEADPICKUPDATE", by.y = "Clean_Date", all.x = TRUE)

ManLead_Dat$PUBHOLIDAY[is.na(ManLead_Dat$PUBHOLIDAY)] <- "Normal_Day"

rm(Enrico_data)

#########################################################################

names(ManLead_Dat)[!(names(ManLead_Dat) %in% Model$var.names)]
Model$var.names[!(Model$var.names %in% names(ManLead_Dat))]

# Allocate ----------------------------------------------------------------
ManLead_Dat$ZLAGENT

Preds <- predict(object   =  Model,
                 newdata  =  ManLead_Dat,
                 n.trees  =  ntrees,
                 type     =  "response")
ManLead_Dat$Pred <- Preds

PredOut <- subset(ManLead_Dat, select = c(ID, Pred))

ManLead_Dat2 <- merge(ManLead_Dat_Orig, PredOut, by.x = "ID", by.y = "ID")

# Update DB ---------------------------------------------------------------

for (i in 1:nrow(ManLead_Dat2)) {
  
  updateQuery <- paste("UPDATE AccessLife_Sales_File_Lead_Data ",
                       "SET `gbm_Pred` = '", ManLead_Dat2$Pred[i], "' ",
                       "WHERE AutoNumber = '", ManLead_Dat2$LEADNUMBER[i], "'",
                       sep = "")
  if (i == 1) {
    AllQueries <- updateQuery
  } else {
    AllQueries <- paste(AllQueries, updateQuery, sep = ";")
  }
  
  
}

dbSendQuery(mydb, updateQuery)


















