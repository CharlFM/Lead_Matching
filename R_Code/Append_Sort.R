# Adds some variables -----------------------------------------------------
# Lead Pickup Day and Time
newLead_Dat$LEADPICKUPTIME <- as.numeric(format(round(Sys.time(), units = "hours"), format = "%H"))

newLead_Dat$DAYTIME <- "Morning"
newLead_Dat$DAYTIME[newLead_Dat$LEADPICKUPTIME %in% 12:14] <- "Lunch"
newLead_Dat$DAYTIME[newLead_Dat$LEADPICKUPTIME %in% 15:18] <- "Midday"
newLead_Dat$DAYTIME[newLead_Dat$LEADPICKUPTIME %in% 19:24] <- "Night"

newLead_Dat$WEEKDAY  <- weekdays(Sys.Date())
newLead_Dat$WEEKEND  <- "Weekday"
newLead_Dat$WEEKEND[newLead_Dat$WEEKDAY == "Saturday" | newLead_Dat$WEEKDAY == "Sunday"]  <- "Weekend"

newLead_Dat$WEEKTIME <- "Early"
newLead_Dat$WEEKTIME[newLead_Dat$WEEKDAY == "Wednesday"]  <- "Mid"
newLead_Dat$WEEKTIME[newLead_Dat$WEEKDAY == "Thursday" | newLead_Dat$WEEKDAY == "Friday"]  <- "Late"
newLead_Dat$WEEKTIME[newLead_Dat$WEEKDAY == "Saturday" | newLead_Dat$WEEKDAY == "Sunday"]  <- "Weekend"

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

if (as.Date(Sys.Date()) %in% Enrico_data$Clean_Date) {
  newLead_Dat$PUBHOLIDAY <- "Public_Holiday"
} else {
  newLead_Dat$PUBHOLIDAY <- "Normal_Day"
}

rm(Enrico_data)


# Time from vehicle purchase date to now
newLead_Dat$INCEPTIONDATE <- DateConv(newLead_Dat$INCEPTIONDATE)
newLead_Dat$TIMETOCALL <- as.numeric(as.Date(Sys.time()) - newLead_Dat$INCEPTIONDATE)


# Vehicle Age
newLead_Dat$VEHICLEAGE <- as.numeric(format(newLead_Dat$INCEPTIONDATE, "%Y")) - as.numeric(newLead_Dat$FIRSTREGISTRATIONYEAR)


# Cleans Email Data
newLead_Dat$CLIENTEMAILADDRESS_FULL_DOMAIN <- sub(".*\\@", "", newLead_Dat$CLIENTEMAILADDRESS)
newLead_Dat$CLIENTEMAILADDRESS_SUB_DOMAIN  <- sub("\\..*$", "", newLead_Dat$CLIENTEMAILADDRESS_FULL_DOMAIN)
newLead_Dat$CLIENTEMAILADDRESS_SUB_DOMAIN  <- gsub("[^[:alpha:]]", "", toupper(newLead_Dat$CLIENTEMAILADDRESS_SUB_DOMAIN))

newLead_Dat$CLIENTEMAILADDRESS_EXTENTION   <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", newLead_Dat$CLIENTEMAILADDRESS_FULL_DOMAIN)))
newLead_Dat$CLIENTEMAILADDRESS_EXTENTION   <- gsub("[^[:alpha:]]", "", toupper(newLead_Dat$CLIENTEMAILADDRESS_EXTENTION))

newLead_Dat$CLIENTEMAILADDRESS_DOTS        <- as.numeric(countLetter(newLead_Dat$CLIENTEMAILADDRESS_FULL_DOMAIN, "."))
newLead_Dat                                <- subset(newLead_Dat, select = -c(CLIENTEMAILADDRESS_FULL_DOMAIN, CLIENTEMAILADDRESS))

DB_EmSd <- Model$var.levels[which(Model$var.names == "CLIENTEMAILADDRESS_SUB_DOMAIN")][[1]]

newLead_Dat$CLIENTEMAILADDRESS_SUB_DOMAIN[!(newLead_Dat$CLIENTEMAILADDRESS_SUB_DOMAIN %in% DB_EmSd)] <- "OTHER"

DB_EmExt <- Model$var.levels[which(Model$var.names == "CLIENTEMAILADDRESS_EXTENTION")][[1]]

newLead_Dat$CLIENTEMAILADDRESS_EXTENTION[!(newLead_Dat$CLIENTEMAILADDRESS_EXTENTION %in% DB_EmExt)] <- "OTHER"

rm(DB_EmSd, DB_EmExt)


# Cleans Data -------------------------------------------------------------
# Get Postal Adress Data
newLead_Dat$CLIENTFULLPOSTAL <- paste(newLead_Dat$CLIENTPOSTALADDRESS1, 
                                      newLead_Dat$CLIENTPOSTALADDRESS2, 
                                      newLead_Dat$CLIENTPOSTALADDRESS3, 
                                      newLead_Dat$CLIENTPOSTALADDRESS4,
                                      newLead_Dat$CLIENTPOSTALADDRESS5,
                                      newLead_Dat$CLIENTPOSTALADDRESS6,
                                      newLead_Dat$CLIENTPOSTALADDRESS7,
                                      newLead_Dat$CLIENTPOSTALADDRESSSUBURB,
                                      newLead_Dat$CLIENTPOSTALADDRESSCITY,
                                      newLead_Dat$CLIENTPOSTALADDRESSPOSTALCODE,
                                      sep = " ")
# Clean province info
Provinces <- c("WESTERNCAPE", "WESTERNPROVINCE", "LIMPOPO", "MPUMALANGA", "GAUTENG", 
               "EASTERNCAPE", "KWAZULUNATAL", "NORTHERNCAPE", "FREESTATE", "NORTHWEST")

AdrDf <- strsplit(newLead_Dat$CLIENTFULLPOSTAL, " ")

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

newLead_Dat$CLIENTPOSTALADDRESSPOSTALCODE <- PcodeFin[(1:NROW(AdrDf)) + NROW(AdrDf) * (PostCol - 1)]
newLead_Dat$CLIENTPOSTALADDRESSPOSTALCODE <- as.character(as.numeric(newLead_Dat$CLIENTPOSTALADDRESSPOSTALCODE))
City_Post_Data$POSTALCODE <- as.character(as.numeric(City_Post_Data$POSTALCODE))

newLead_Dat <- merge(newLead_Dat, City_Post_Data, by.x = "CLIENTPOSTALADDRESSPOSTALCODE", by.y = "POSTALCODE", all.x = TRUE)

rm(Provinces, AdrDf, n, l, tempDf, i, PostCol, PcodeFin, PostalDF)

newLead_Dat <- subset(newLead_Dat, select = -CLIENTFULLPOSTAL)

colnames(newLead_Dat)[colnames(newLead_Dat) == "CITY"]      <-  "CLIENTPOSTALADDRESSCITY"
colnames(newLead_Dat)[colnames(newLead_Dat) == "PROVINCE"]  <-  "CLIENTPOSTALADDRESSPROVINCE"
colnames(newLead_Dat)[colnames(newLead_Dat) == "SUBURB"]    <-  "CLIENTPOSTALADDRESSSUBURB"

DB_City <- Model$var.levels[which(Model$var.names == "CLIENTPOSTALADDRESSCITY")][[1]]

newLead_Dat$CLIENTPOSTALADDRESSCITY[!(newLead_Dat$CLIENTPOSTALADDRESSCITY %in% DB_City)] <- "OTHER"

DB_PCode <- Model$var.levels[which(Model$var.names == "CLIENTPOSTALADDRESSPOSTALCODE")][[1]]

newLead_Dat$CLIENTPOSTALADDRESSPOSTALCODE[!(newLead_Dat$CLIENTPOSTALADDRESSPOSTALCODE %in% DB_PCode)] <- "OTHER"

DB_PSub <- Model$var.levels[which(Model$var.names == "CLIENTPOSTALADDRESSSUBURB")][[1]]

newLead_Dat$CLIENTPOSTALADDRESSSUBURB[!(newLead_Dat$CLIENTPOSTALADDRESSSUBURB %in% DB_PSub)] <- "OTHER"

rm(DB_City, DB_PSub, DB_PCode)

# Cleans Title
newLead_Dat$CLIENTTITLE <- gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(newLead_Dat$CLIENTTITLE)))

newLead_Dat$CLIENTTITLE[grepl("REV", newLead_Dat$CLIENTTITLE)]     <-  "REV"
newLead_Dat$CLIENTTITLE[grepl("PROF", newLead_Dat$CLIENTTITLE)]    <-  "PROF"
newLead_Dat$CLIENTTITLE[grepl("ADV", newLead_Dat$CLIENTTITLE)]     <-  "ADV"
newLead_Dat$CLIENTTITLE[grepl("DOCTOR", newLead_Dat$CLIENTTITLE)]  <-  "DR"

DB_TL <- Model$var.levels[which(Model$var.names == "CLIENTTITLE")][[1]]

newLead_Dat$CLIENTTITLE[!(newLead_Dat$CLIENTTITLE %in% DB_TL)] <- NA

rm(DB_TL)

# Marital Status
newLead_Dat$MARITALSTATUS <- gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(newLead_Dat$MARITALSTATUS)))

newLead_Dat$MARITALSTATUS[grepl("SINGLE", newLead_Dat$MARITALSTATUS)]     <-  "SINGLE"
newLead_Dat$MARITALSTATUS[grepl("ENGAGED", newLead_Dat$MARITALSTATUS)]    <-  "ENGAGED"
newLead_Dat$MARITALSTATUS[grepl("MARRIED", newLead_Dat$MARITALSTATUS)]    <-  "MARRIED"
newLead_Dat$MARITALSTATUS[grepl("SEPARATED", newLead_Dat$MARITALSTATUS)]  <-  "DIVORCED"
newLead_Dat$MARITALSTATUS[grepl("DIVORCED", newLead_Dat$MARITALSTATUS)]   <-  "DIVORCED"
newLead_Dat$MARITALSTATUS[grepl("WIDOW", newLead_Dat$MARITALSTATUS)]      <-  "WIDOW"

DB_MS <- Model$var.levels[which(Model$var.names == "MARITALSTATUS")][[1]]

newLead_Dat$MARITALSTATUS[!(newLead_Dat$MARITALSTATUS %in% DB_MS)] <- NA

rm(DB_MS)

# Occupation
newLead_Dat$CLIENTOCCUPATIONNAME <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", newLead_Dat$CLIENTOCCUPATIONNAME)))
newLead_Dat$CLIENTOCCUPATIONNAME[newLead_Dat$CLIENTOCCUPATIONNAME == ""] <- NA

DB_Occ <- Model$var.levels[which(Model$var.names == "CLIENTOCCUPATIONNAME")][[1]]

newLead_Dat$CLIENTOCCUPATIONNAME[!(newLead_Dat$CLIENTOCCUPATIONNAME %in% DB_Occ)] <- "OTHER"

rm(DB_Occ)

# Model

ModDf <- strsplit(newLead_Dat$MODEL, " ")

n <- max(sapply(ModDf, length))
l <- lapply(ModDf, function(X) c(X, rep(NA, n - length(X))))

ModDf <- data.frame(t(do.call(cbind, l)), stringsAsFactors = FALSE)
colnames(ModDf) <- paste("Model", seq(1:ncol(ModDf)), sep = "")

ModDf[[1]][is.na(ModDf[[1]])] <- ""
ModDf[[2]][is.na(ModDf[[2]])] <- ""

newLead_Dat$MANUFACTURER <- ModDf[[1]]
newLead_Dat$MODEL        <- paste(ModDf[[1]], ModDf[[2]], sep = "")

newLead_Dat$MANUFACTURER[newLead_Dat$MANUFACTURER == ""] <- NA
newLead_Dat$MODEL[newLead_Dat$MODEL == ""]               <- NA

newLead_Dat$MANUFACTURER <- gsub("[^[:alnum:] ]", "", toupper(gsub("^.*?\\.", "", newLead_Dat$MANUFACTURER))) 
newLead_Dat$MODEL <- gsub("[^[:alnum:] ]", "", toupper(gsub("^.*?\\.", "", newLead_Dat$MODEL))) 

rm(n, l, ModDf)

DB_Mod <- Model$var.levels[which(Model$var.names == "MODEL")][[1]]

newLead_Dat$MODEL[!(newLead_Dat$MODEL %in% DB_Mod)] <- "OTHER"

DB_Man <- Model$var.levels[which(Model$var.names == "MANUFACTURER")][[1]]

newLead_Dat$MANUFACTURER[!(newLead_Dat$MANUFACTURER %in% DB_Man)] <- "OTHER"

rm(DB_Mod, DB_Man)

# Year
# Diff between car model year and purchase year
YearDf <- strsplit(UniDash(newLead_Dat$FIRSTREGISTRATIONYEAR), "-")

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

newLead_Dat$FIRSTREGISTRATIONYEAR <- as.numeric(yyFin)

# Vehicle age at purchase
newLead_Dat$VEHICLEAGE <- as.numeric(format(newLead_Dat$INCEPTIONDATE, "%Y")) - newLead_Dat$FIRSTREGISTRATIONYEAR

# Accesories count
newLead_Dat$ACCESSORIES <- str_count(newLead_Dat$ACCESSORIES, ";")

# Vehicle Use
newLead_Dat$VEHICLEUSE <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", newLead_Dat$VEHICLEUSE)))
newLead_Dat$VEHICLEUSE[newLead_Dat$VEHICLEUSE == ""] <- NA

DB_Vuse <- Model$var.levels[which(Model$var.names == "VEHICLEUSE")][[1]]

newLead_Dat$VEHICLEUSE[!(newLead_Dat$VEHICLEUSE %in% DB_Vuse)] <- NA

rm(DB_Vuse)

# Finance and Inusrance info
newLead_Dat$DOCINSURANCECOMPANYNAME <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", newLead_Dat$DOCINSURANCECOMPANYNAME)))
newLead_Dat$DOCINSURANCECOMPANYNAME[newLead_Dat$DOCINSURANCECOMPANYNAME == ""] <- NA

DB_InsCo <- Model$var.levels[which(Model$var.names == "DOCINSURANCECOMPANYNAME")][[1]]

newLead_Dat$DOCINSURANCECOMPANYNAME[!(newLead_Dat$DOCINSURANCECOMPANYNAME %in% DB_InsCo)] <- "OTHER"

newLead_Dat$DOCFINANCECOMPANYNAME <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", newLead_Dat$DOCFINANCECOMPANYNAME)))
newLead_Dat$DOCFINANCECOMPANYNAME[newLead_Dat$DOCFINANCECOMPANYNAME == ""] <- NA

DB_FinCo <- Model$var.levels[which(Model$var.names == "DOCFINANCECOMPANYNAME")][[1]]

newLead_Dat$DOCFINANCECOMPANYNAME[!(newLead_Dat$DOCFINANCECOMPANYNAME %in% DB_FinCo)] <- "OTHER"

rm(DB_InsCo, DB_FinCo)

# Branch and Salesman info
newLead_Dat$BRANCHNAME <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", newLead_Dat$BRANCHNAME)))

newLead_Dat$SALESPERSON <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", newLead_Dat$SALESPERSON)))

newLead_Dat$SALESPERSON <- ifelse(newLead_Dat$SALESPERSON != "", paste(newLead_Dat$BRANCHNAME, newLead_Dat$SALESPERSON, sep = "_"), "")

newLead_Dat$SALESPERSON[newLead_Dat$SALESPERSON == ""] <- NA
newLead_Dat$BRANCHNAME[newLead_Dat$BRANCHNAME == ""]   <- NA

if (length(which(Model$var.names == "SALESPERSON")) == 0) {
  newLead_Dat <- subset(newLead_Dat, select = -SALESPERSON)
} else {
  DB_Sp <- Model$var.levels[which(Model$var.names == "SALESPERSON")][[1]]
  
  newLead_Dat$SALESPERSON[!(newLead_Dat$SALESPERSON %in% DB_Sp)] <- "OTHER"
  rm(DB_Sp)
}

DB_Bn <- Model$var.levels[which(Model$var.names == "BRANCHNAME")][[1]]

newLead_Dat$BRANCHNAME[!(newLead_Dat$BRANCHNAME %in% DB_Bn)] <- "OTHER"

rm(DB_Bn)

# Affinity
newLead_Dat$AFFINITY <- gsub(" ", "", gsub("[^[:alpha:] ]", "", toupper(newLead_Dat$AFFINITY)))

DB_Af <- Model$var.levels[which(Model$var.names == "AFFINITY")][[1]]

newLead_Dat$AFFINITY[!(newLead_Dat$AFFINITY %in% DB_Af)] <- "OTHER"

rm(DB_Af)

# ID Type
newLead_Dat$CLIENTIDTYPE <- toupper(trim(newLead_Dat$CLIENTIDTYPE))

# Race and Culture
newLead_Dat$RACE <- gsub(" ","", gsub("[^[:alpha:] ]", "", toupper(newLead_Dat$RACE)))
newLead_Dat$CULTURE <- gsub(" ","", gsub("[^[:alpha:] ]", "", toupper(newLead_Dat$CULTURE)))

DB_Ra <- Model$var.levels[which(Model$var.names == "RACE")][[1]]
DB_Cu <- Model$var.levels[which(Model$var.names == "CULTURE")][[1]]

newLead_Dat$RACE[!(newLead_Dat$RACE %in% DB_Ra)]       <- NA
newLead_Dat$CULTURE[!(newLead_Dat$CULTURE %in% DB_Cu)] <- NA

rm(DB_Ra, DB_Cu)


# Clean All Numeric Variables 
Options <- c("FINANCETERM", "VEHICLEVALUE", "DEPOSITVALUE", "RESIDUALVALUE", "FINANCEAMOUNT", "ODOMETERREADING", "TIMETOCALL", "LEADPICKUPTIME",
             "VEHICLEAGE", "FIRSTREGISTRATIONYEAR", "ACCESSORIES", "CLIENTAGE")

for (opt in Options) {
  newLead_Dat[[opt]]                             <-  as.numeric(newLead_Dat[[opt]])
  newLead_Dat[[opt]][is.na(newLead_Dat[[opt]])]  <-  0
  
  newLead_Dat[[opt]][newLead_Dat[[opt]] < Model$var.levels[which(Model$var.names == opt)][[1]][1]]  <- Model$var.levels[which(Model$var.names == opt)][[1]][1]
  newLead_Dat[[opt]][newLead_Dat[[opt]] > Model$var.levels[which(Model$var.names == opt)][[1]][11]] <- Model$var.levels[which(Model$var.names == opt)][[1]][1]
}

rm(Options, opt)


# Select Model Columns and ID variable
newLead_Dat <- newLead_Dat[, c(Model$var.names, "ID")]

response.names <- "STATUS"

feature.names <- colnames(newLead_Dat)
feature.names <- feature.names[feature.names != response.names]
feature.names <- feature.names[feature.names != "CLIENTIDNUMBER"]

for (f in feature.names) {
  if (class(newLead_Dat[[f]]) == "character") {
    levels <- Model$var.levels[which(Model$var.names == f)][[1]]
    #    levels <- unique(c(newLead_Dat[[f]]))
    newLead_Dat[[f]] <- factor(newLead_Dat[[f]], levels = levels)
  }
  else if (class(newLead_Dat[[f]]) == "integer" | class(newLead_Dat[[f]]) == "numeric"){
    newLead_Dat[[f]] <- ifelse(is.na(newLead_Dat[[f]]), 0, newLead_Dat[[f]])
  }
}



