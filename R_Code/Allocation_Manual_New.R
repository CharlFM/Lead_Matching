
# Clears Memory
options(java.parameters = "-Xmx4g")
rm(list = ls())
gc()

Path <- getwd()

source(paste(Path, "/R_Code/Initialize.R", sep = ""))

# Opens DB
source(paste(Path, "/R_Code/OpenDB.R", sep = ""))

# Load Data ---------------------------------------------------------------
# Loads New lead data to DB - For Allocation
source(paste(Path, "/R_Code/Load_Lead_Data.R", sep = "")) 

# Loads City/Race/ etc data - For Allocation and Modelling
source(paste(Path, "/R_Code/Load_Other_Data.R", sep = ""))

# Load current model
today <- as.character(Sys.Date())
load(paste(getwd(), "/Active_Model.RData", sep = ""))
ntrees <- max(Model$trees.fitted)

# Loads manual allocation Data
DB_Names <- read_excel(paste(Path, "/Data/Lead_Col_Names/DBNAMES.xlsx", sep = ""),
                       sheet = 1,
                       col_names = TRUE)

All_lead_Data$FIRSTALLOCATIONDATE <- today
All_lead_Data$LEADDATE            <- today

ManLead_Dat <- All_lead_Data[, colnames(All_lead_Data) %in% c(DB_Names$New, 
                                                              "CLIENTAGE", "CLIENTIDTYPE", "PRODUCTTYPECATEGORYNAME", "TAKEN")]
ManLead_Dat$PRODUCTTYPECATEGORYNAME <- ifelse(ManLead_Dat$TAKEN == 1 & ManLead_Dat$PRODUCTTYPECATEGORYNAME == "Retrenchment", 1, 0)

agent.query <- paste("SELECT * ",
                     "FROM agents ",
                     "WHERE `Active` = 'YES' ",
                     sep = "")

Allocation_Dat <- dbGetQuery(my_new_db, agent.query)

ManLead_Dat$ID <- seq(1:nrow(ManLead_Dat))

ManLead_DatOrig <- ManLead_Dat

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

#
# Cleaned Inception Date 
#

# Populate missing dates (for older leads)
ManLead_Dat$INCEPTIONDATE <- trim(ManLead_Dat$INCEPTIONDATE)
ManLead_Dat$INCEPTIONDATE[ManLead_Dat$INCEPTIONDATE == ""]         <- NA
ManLead_Dat$INCEPTIONDATE[ManLead_Dat$INCEPTIONDATE == "########"] <- NA

ManLead_Dat$INCEPTIONDATE[is.na(ManLead_Dat$INCEPTIONDATE)] <- ManLead_Dat$FIRSTALLOCATIONDATE[is.na(ManLead_Dat$INCEPTIONDATE)]  
ManLead_Dat$INCEPTIONDATE[is.na(ManLead_Dat$INCEPTIONDATE)] <- ManLead_Dat$LEADDATE[is.na(ManLead_Dat$INCEPTIONDATE)]

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

ManLead_Dat <- subset(ManLead_Dat, select = -c(LEADDATE))

#

ManLead_Dat$INCEPTIONDATE <- DateConv(ManLead_Dat$INCEPTIONDATE)

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

DB_MS <- Model$var.levels[which(Model$var.names == "MARITALSTATUS")][[1]]

ManLead_Dat$MARITALSTATUS[!(ManLead_Dat$MARITALSTATUS %in% DB_MS)] <- NA

rm(DB_MS)

#
# Clean Title 
#

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

# DB_Bn <- Model$var.levels[which(Model$var.names == "BRANCHNAME")][[1]]
# 
# ManLead_Dat$BRANCHNAME[!(ManLead_Dat$BRANCHNAME %in% DB_Bn)] <- "OTHER"
# 
# rm(DB_Bn)


# Fix time to call
ManLead_Dat$TIMETOCALL     <- as.numeric(as.Date(today) - ManLead_Dat$INCEPTIONDATE)

ManLead_Dat <- subset(ManLead_Dat, select = -c(INCEPTIONDATE, FIRSTALLOCATIONDATE))

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

colnames(ManLead_Dat)[colnames(ManLead_Dat) == "CITY"]      <-  "CLIENTPOSTALADDRESS2"
colnames(ManLead_Dat)[colnames(ManLead_Dat) == "PROVINCE"]  <-  "CLIENTPOSTALADDRESS3"
colnames(ManLead_Dat)[colnames(ManLead_Dat) == "SUBURB"]    <-  "CLIENTPOSTALADDRESS4"

IDnPostal <- subset(ManLead_Dat, select = c(ID, CLIENTPOSTALADDRESS2, CLIENTPOSTALADDRESS3, CLIENTPOSTALADDRESS4, CLIENTPOSTALADDRESSPOSTALCODE))

# Update original Data Postal info also
ManLead_DatOrig <- subset(ManLead_DatOrig, select = -c(CLIENTPOSTALADDRESS2, CLIENTPOSTALADDRESS3, CLIENTPOSTALADDRESS4, CLIENTPOSTALADDRESSPOSTALCODE))
ManLead_DatOrig <- merge(ManLead_DatOrig, IDnPostal, by = "ID")
ManLead_DatOrig$CLIENTPOSTALADDRESS2 <- gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(ManLead_DatOrig$CLIENTPOSTALADDRESS2), perl = TRUE)
ManLead_DatOrig$CLIENTPOSTALADDRESS3 <- gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(ManLead_DatOrig$CLIENTPOSTALADDRESS3), perl = TRUE)
ManLead_DatOrig$CLIENTPOSTALADDRESS4 <- gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(ManLead_DatOrig$CLIENTPOSTALADDRESS4), perl = TRUE)
ManLead_DatOrig$CLIENTPOSTALADDRESSPOSTALCODE <- str_pad(ManLead_DatOrig$CLIENTPOSTALADDRESSPOSTALCODE, width = 4, side = "left", pad = "0")

ManLead_DatOrig$RACE <- gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(ManLead_Dat$RACE), perl = TRUE)

# Set Names back
colnames(ManLead_Dat)[colnames(ManLead_Dat) == "CLIENTPOSTALADDRESS2"]  <-  "CLIENTPOSTALADDRESSCITY"
colnames(ManLead_Dat)[colnames(ManLead_Dat) == "CLIENTPOSTALADDRESS3"]  <-  "CLIENTPOSTALADDRESSPROVINCE"
colnames(ManLead_Dat)[colnames(ManLead_Dat) == "CLIENTPOSTALADDRESS4"]  <-  "CLIENTPOSTALADDRESSSUBURB"

# Continue cleaning
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

feature.names <- colnames(ManLead_Dat)
feature.names <- feature.names[feature.names != response.names]
feature.names <- feature.names[feature.names != "CLIENTIDNUMBER"]


#### CHANGED BRANCHNAME to a factor before hand so that the values were not limited to the factors used by the model.
ManLead_Dat[["BRANCHNAME"]] <- factor(ManLead_Dat[["BRANCHNAME"]])


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
ManLead_Dat$ZLAGENT <- ""

names(ManLead_Dat)[!(names(ManLead_Dat) %in% Model$var.names)]
Model$var.names[!(Model$var.names %in% names(ManLead_Dat))]


# Allocate ----------------------------------------------------------------

source(paste(Path, "/R_Code/Allocation_Auto.R", sep = ""))

LeadOut <- data.frame(ZLAGENT = as.character(),
                      Pred    = as.numeric(),
                      ID      = as.integer())

ManLead_Dat_Distr <- ManLead_Dat

for (aff in specialAffinities$AFFINITY) { 
  
  affOrig <- specialAffinities$AffinityName[specialAffinities$AFFINITY == aff]
  
  affTot   <- sum(ManLead_Dat$TempAFFINITY == aff)
  affCount <- 0
  
  SubsetData <- ManLead_Dat[grepl(aff, ManLead_Dat$TempAFFINITY), ]
  SubsetData <- SubsetData[, c(Model$var.names, "ID")]
  
  AffAgents <- Allocation_Dat[Allocation_Dat[[affOrig]] == "YES", ] 
  
  if (nrow(SubsetData) > 0) {
    
    # First allocate only's 
    while (nrow(SubsetData) > 0 & sum(AffAgents$PerAgent[AffAgents$ONLY == "YES"] > 0)) {
      for (agnt in AffAgents$ZLAGENT[AffAgents$ONLY == "YES"]) {
        source(paste(Path, "/R_Code/SubsetSort.R", sep = ""))
        if (affTot == affCount) {break()}
      }
    }
    
    # Then rest
    if (nrow(SubsetData) > 0) {
      while (nrow(SubsetData) > 0 & sum(AffAgents$PerAgent[AffAgents$ONLY != "YES"]) > 0) {
        for (agnt in AffAgents$ZLAGENT[AffAgents$ONLY != "YES"]) {
          source(paste(Path, "/R_Code/SubsetSort.R", sep = ""))
          if (affTot == affCount) {break()}
        }
      } 
    } 
    
  }
  
} 

Allocation_Dat <- Allocation_Dat[Allocation_Dat$PerAgent > 0, ]
Allocation_Dat <- Allocation_Dat[Allocation_Dat$Owner == "NONE", ]
Allocation_Dat <- Allocation_Dat[Allocation_Dat$ONLY == "NO", ]

if (nrow(ManLead_Dat_Distr) > 0) {
  
  aff      <- "Balancing"
  affCount <- 0
  
  SubsetData <- ManLead_Dat_Distr
  SubsetData <- SubsetData[, c(Model$var.names, "ID")]
  
  AffAgents  <- Allocation_Dat
  
  while (nrow(ManLead_Dat_Distr) > 0) {
    for (agnt in Allocation_Dat$ZLAGENT) {
      source(paste(Path, "/R_Code/SubsetSort.R", sep = ""))
      
    }
  }
}


ManLead_Dat <- subset(ManLead_Dat, select = -ZLAGENT)
ManLead_Dat2 <- merge(ManLead_Dat, LeadOut, 
                      by.x = "ID", by.y = "ID")

ManLead_Dat2$COMCAT <- ifelse(grepl("BARLO", gsub(" ", "", toupper(ManLead_Dat2$AFFINITY))), "E", "A")

d_time <- gsub(" ", "_", Sys.time())
d_time <- gsub("-", "_", d_time)
d_time <- gsub(":", "_", d_time)

write.csv(ManLead_Dat2, paste(Path, "/Output/New/Results_", d_time, ".csv", sep = ""))

# # Insert to DB ---------------------------------------------------------------
source(paste(Path, "/R_Code/Insert.R", sep = ""))





















