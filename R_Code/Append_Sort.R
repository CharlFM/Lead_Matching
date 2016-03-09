# Clean adress names
All_lap_Data$POSTALADDRESS1 <- gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(All_lap_Data$POSTALADDRESS1)))
All_lap_Data$POSTALADDRESS2 <- gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(All_lap_Data$POSTALADDRESS2)))
All_lap_Data$POSTALADDRESS3 <- gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(All_lap_Data$POSTALADDRESS3)))

# Clean province info
Provinces <- c("WESTERNCAPE", "LIMPOPO", "MPUMALANGA", "GAUTENG", "EASTERNCAPE", "KWAZULUNATAL", "NORTHERNCAPE", "FREESTATE", "NORTHWEST")

# Select given province info
All_lap_Data$PROVINCE <- ""
All_lap_Data$PROVINCE[All_lap_Data$POSTALADDRESS3 %in% Provinces] <- All_lap_Data$POSTALADDRESS3[All_lap_Data$POSTALADDRESS3 %in% Provinces]
All_lap_Data$PROVINCE[All_lap_Data$POSTALADDRESS2 %in% Provinces] <- All_lap_Data$POSTALADDRESS2[All_lap_Data$POSTALADDRESS2 %in% Provinces]
All_lap_Data$PROVINCE[All_lap_Data$POSTALADDRESS1 %in% Provinces] <- All_lap_Data$POSTALADDRESS1[All_lap_Data$POSTALADDRESS1 %in% Provinces]

# Merge City info with each of the postal adressess and complete where missing
All_lap_Data <- merge(All_lap_Data, City_Data, by.x = "POSTALADDRESS1", by.y = "CITY", all.x = TRUE)
All_lap_Data$PROVINCENAME[is.na(All_lap_Data$PROVINCENAME)] <- ""
All_lap_Data$PROVINCE[All_lap_Data$PROVINCE == ""] <- All_lap_Data$PROVINCENAME[All_lap_Data$PROVINCE == ""]
All_lap_Data <- subset(All_lap_Data, select = -PROVINCENAME)

All_lap_Data <- merge(All_lap_Data, City_Data, by.x = "POSTALADDRESS2", by.y = "CITY", all.x = TRUE)
All_lap_Data$PROVINCENAME[is.na(All_lap_Data$PROVINCENAME)] <- ""
All_lap_Data$PROVINCE[All_lap_Data$PROVINCE == ""] <- All_lap_Data$PROVINCENAME[All_lap_Data$PROVINCE == ""]
All_lap_Data <- subset(All_lap_Data, select = -PROVINCENAME)

All_lap_Data <- merge(All_lap_Data, City_Data, by.x = "POSTALADDRESS3", by.y = "CITY", all.x = TRUE)
All_lap_Data$PROVINCENAME[is.na(All_lap_Data$PROVINCENAME)] <- ""
All_lap_Data$PROVINCE[All_lap_Data$PROVINCE == ""] <- All_lap_Data$PROVINCENAME[All_lap_Data$PROVINCE == ""]
All_lap_Data <- subset(All_lap_Data, select = -PROVINCENAME)

rm(Provinces, City_Data)

#####################################################################################################
# Determine Duration #
######################

fileXLSDate <- file.mtime(paste(Path, "/Data/AssetLife Data/", lap_File_List[lapfile], sep = ""))

All_lap_Data$COMMENCEMENTDATEOFPOLICY <- DateConv(All_lap_Data$COMMENCEMENTDATEOFPOLICY)
All_lap_Data$COMMENCEMENTDATEOFPOLICY <- firstDayMonth(All_lap_Data$COMMENCEMENTDATEOFPOLICY)
All_lap_Data$STATUSEFFECTIVEENDDATE   <- DateConv(All_lap_Data$STATUSEFFECTIVEENDDATE)

All_lap_Data$STATUSEFFECTIVEENDDATE <- as.Date(ifelse(All_lap_Data$STATUSEFFECTIVEENDDATE < All_lap_Data$COMMENCEMENTDATEOFPOLICY, 
                                               All_lap_Data$COMMENCEMENTDATEOFPOLICY, 
                                               All_lap_Data$STATUSEFFECTIVEENDDATE))

# If there is an end date and the number of collected premiums equals 0, then force the end date to equal start date
All_lap_Data$STATUSEFFECTIVEENDDATE[!is.na(All_lap_Data$STATUSEFFECTIVEENDDATE) & 
                                      All_lap_Data$TOTALAMOUNTPAIDSINCEINCEPTIONTOCURRENTMONTH == 0] <- 
  All_lap_Data$COMMENCEMENTDATEOFPOLICY[!is.na(All_lap_Data$STATUSEFFECTIVEENDDATE) & 
                                        All_lap_Data$TOTALAMOUNTPAIDSINCEINCEPTIONTOCURRENTMONTH == 0]
  
All_lap_Data$DURATION                               <-  as.numeric(((All_lap_Data$STATUSEFFECTIVEENDDATE - 
                                                                       All_lap_Data$COMMENCEMENTDATEOFPOLICY) / 365.25) * 12)
All_lap_Data$DURATION[is.na(All_lap_Data$DURATION)] <-  as.numeric(((as.Date(substr(fileXLSDate, 1, 10)) - 
                                                                       All_lap_Data$COMMENCEMENTDATEOFPOLICY[is.na(All_lap_Data$DURATION)]) / 365.25) * 12)

rm(lap_File_List, lapfile)

#####################################################################################################
# Determine Status #
####################

# If there is end date = Lapse, else Active ( or 1, 0)
All_lap_Data$STATUS[!is.na(All_lap_Data$STATUSEFFECTIVEENDDATE)] <- "LAP"    
All_lap_Data$STATUS[is.na(All_lap_Data$STATUSEFFECTIVEENDDATE)]  <- "ACT"

# Set threshold of lapse duration (we are conserned with reducing lapses in months 0 - 5)
All_lap_Data$STATUS[All_lap_Data$DURATION > 6 & All_lap_Data$STATUS == "LAP"]  <- "ACT"

#  To distinguish between NTU's and LAP include the following line
# All_lap_Data$STATUS[All_lap_Data$DURATION == 0 & All_lap_Data$STATUS == "LAP"]  <- "NTU"

#####################################################################################################
# Weight Cleaning #
###################

# Rename
colnames(All_lap_Data)[which(names(All_lap_Data) == "WEIGHT130KGSORWEIGHTOLDPOLICIES")] <- "WEIGHT"

# If weight is outside the interquartile range, bring it to IQR boundry.
# first find the interquartile range
weight <- gsub(" ", "", toupper(All_lap_Data$WEIGHT))
weight[weight == "NOTANSWERED" | weight == ""] <- NA

temp_weight  <-  as.numeric(weight)
AboveMean    <-  mean(temp_weight[temp_weight > 130], na.rm = TRUE)
BelowMean    <-  mean(temp_weight[temp_weight < 130], na.rm = TRUE)

weight[weight == "YES"]  <-  as.character(AboveMean)
weight[weight == "NO"]   <-  as.character(BelowMean)
weight                   <-  as.numeric(weight)

mean  <-  mean(weight, na.rm = TRUE)
IQR   <-  IQR(weight,  na.rm = TRUE)

weight[weight > mean + 2*IQR] <- mean + IQR
weight[weight < mean - 2*IQR] <- mean - IQR

All_lap_Data$WEIGHT <-  weight

rm(mean, IQR, weight, temp_weight, BelowMean, AboveMean)

#####################################################################################################
# Minor Cleaning #
##################

### Voice Log day + Month ###
All_lap_Data$VOICELOGGED       <-  DateConv(All_lap_Data$VOICELOGGED)
All_lap_Data$VOICELOGGEDDAY    <-  format(All_lap_Data$VOICELOGGED, format = "%d")
All_lap_Data$VOICELOGGEDMONTH  <-  format(All_lap_Data$VOICELOGGED, format = "%m")

### Clean smoking columns ###
All_lap_Data$SMOKERNONSMOKER <- gsub(" ","",gsub("[^[:alnum:] ]", "", toupper(All_lap_Data$SMOKERNONSMOKER)))
All_lap_Data$SMOKERNONSMOKER[All_lap_Data$SMOKERNONSMOKER == "YES"]   <- "SMOKER"
All_lap_Data$SMOKERNONSMOKER[All_lap_Data$SMOKERNONSMOKER == "NO"]    <- "NON-SMOKER"

### Clean More than 30 cigs column. anything that is a "no" becomes a "no" ###
colnames(All_lap_Data)[which(names(All_lap_Data) == "DOYOUSMOKEMORETHAN30CIGARETTESPERDAY")] <- "MORETHAN30"
All_lap_Data$MORETHAN30 <- gsub(" ","",gsub("[^[:alnum:] ]", "", toupper(All_lap_Data$MORETHAN30)))

All_lap_Data$MORETHAN30[All_lap_Data$MORETHAN30 %in% c("LESSTHAN30ADAY", "LESSTHAN30PERDAY", "NONSMOKER", "NOTAPPLICABLE")] <-  "NO"
All_lap_Data$MORETHAN30[All_lap_Data$MORETHAN30 %in% c("MORETHAN30ADAY", "MORETHAN30PERDAY")]                               <-  "YES"

### Checking if height is non-numeric or if it is less than 100cm. if the height is less than 100 then we add a 100 ###
All_lap_Data$HEIGHTINCM <- as.numeric(All_lap_Data$HEIGHTINCM)
All_lap_Data$HEIGHTINCM[All_lap_Data$HEIGHTINCM < 100 & !is.na(All_lap_Data$HEIGHTINCM)] <- All_lap_Data$HEIGHTINCM[All_lap_Data$HEIGHTINCM < 100 & !is.na(All_lap_Data$HEIGHTINCM)] + 100 
All_lap_Data$HEIGHTINCM[is.na(All_lap_Data$HEIGHTINCM)] <- median(All_lap_Data$HEIGHTINCM, na.rm = TRUE)

### PPB indicator (1 if PPB, 0 if not) ###
All_lap_Data$PPB <- gsub(" ", "", All_lap_Data$PPBTOCELLCAPTIVE)
All_lap_Data$PPB[All_lap_Data$PPB != ""]  <- 1
All_lap_Data$PPB[All_lap_Data$PPB == ""]  <- 0

# Find Payment Day
All_lap_Data$PREMIUMPAYERDEBITORDERDAY <- as.numeric(gsub("([0-9]+).*$", "\\1", All_lap_Data$PREMIUMPAYERDEBITORDERDAY))

# We need the number of beneficiaries and the relationship to the policyholder.
# Cleaning all relationship columns
temp     <-  select(All_lap_Data, contains("RELATIONSHIPOFBENEFICIARY"))
temp     <-  toupper(gsub(" ", "", as.matrix(temp)))
All_lap_Data[colnames(All_lap_Data) %in% colnames(temp)] <- temp 

# finding the names of all the columns with "relationshipofbeneficiary" to count the number of beneficiaries
All_lap_Data$NUMBEROFBENEFICIARIES <- 0
temp[temp != ""] <- 1
temp[temp == ""] <- 0
temp <- apply(temp, 2, as.numeric)
All_lap_Data$NUMBEROFBENEFICIARIES <- rowSums(temp)

# Number of credit providers and cleaning provider names.
temp     <-  select(All_lap_Data, contains("CREDITPROVIDER"))
temp     <-  toupper(gsub(" ", "", as.matrix(temp)))
All_lap_Data[colnames(All_lap_Data) %in% colnames(temp)] <- temp

# count the number of credit providers
All_lap_Data$NOCREDITPROVIDERS          <-  0
temp[temp != ""]                        <-  1
temp[temp == ""]                        <-  0
temp                                    <-  apply(temp, 2, as.numeric)
All_lap_Data$NOCREDITPROVIDERS          <-  rowSums(temp)

rm(temp)

# Clean email info
All_lap_Data$EMAILADDRESS <- sub(".*\\@", "", All_lap_Data$EMAILADDRESS)
All_lap_Data$DOMAIN       <- sub("\\..*$", "", All_lap_Data$EMAILADDRESS)
All_lap_Data$EXTENTION    <- gsub("[^[:alpha:]]", "", toupper(gsub("^.*?\\.", "", All_lap_Data$EMAILADDRESS)))
All_lap_Data$EMAILDOTS    <- as.numeric(countLetter(All_lap_Data$EMAILADDRESS, "."))
All_lap_Data              <- subset(All_lap_Data, select = -EMAILADDRESS)

# Clean phone number info
All_lap_Data$PHONEW                             <-  gsub(" ", "", All_lap_Data$PHONEW)
All_lap_Data$PHONEW[All_lap_Data$PHONEW == ""]  <-  "0000000000"
All_lap_Data$PHONEW                             <-  ifelse(as.character(substr(All_lap_Data$PHONEW, 1, 1)) == "0", 
                                                           All_lap_Data$PHONEW, 
                                                           paste("0", All_lap_Data$PHONEW, sep = ""))

All_lap_Data$PHONEAREA     <-  substr(All_lap_Data$PHONEW, 1, 3)
All_lap_Data$PHONESUBAREA  <-  paste(substr(All_lap_Data$PHONEW, 1, 3), substr(All_lap_Data$PHONEW, 4, 6), sep = "")

All_lap_Data              <- subset(All_lap_Data, select = -PHONEW)

# Get Race Info
Race_Data$RACE     <-  gsub("[^[:alpha:] ]", "", toupper(Race_Data$RACE))
Race_Data$SURNAME  <-  gsub("[^[:alpha:] ]", "", toupper(Race_Data$SURNAME))
Race_Data$CULTURE  <-  gsub("[^[:alpha:] ]", "", toupper(Race_Data$CULTURE))

All_lap_Data$POLICYHOLDERSURNAME <- gsub(" ","", gsub("[^[:alpha:] ]", "", toupper(All_lap_Data$POLICYHOLDERSURNAME)))

Race_Data <- subset(Race_Data, !duplicated(SURNAME))

All_lap_Data <- merge(All_lap_Data, Race_Data, by.x = "POLICYHOLDERSURNAME", by.y = "SURNAME", all.x = TRUE)

# Clean Agent Name
All_lap_Data$AGENTNAME <- gsub("[^[:alpha:] ]", "", All_lap_Data$AGENTNAME)

# Get ages
All_lap_Data$DOBOFLIFEINSURED <- DateConv(All_lap_Data$DOBOFLIFEINSURED)

# Age at commencement date
All_lap_Data$AGE_COM <- as.numeric(All_lap_Data$COMMENCEMENTDATEOFPOLICY - All_lap_Data$DOBOFLIFEINSURED)/365.25

# Age at end date (or active if no end)
All_lap_Data$AGE_END                              <- as.numeric(All_lap_Data$STATUSEFFECTIVEENDDATE - 
                                                                  All_lap_Data$DOBOFLIFEINSURED) / 365.25
All_lap_Data$AGE_END[is.na(All_lap_Data$AGE_END)] <- as.numeric(as.Date(fileXLSDate) - 
                                                                  All_lap_Data$DOBOFLIFEINSURED[is.na(All_lap_Data$AGE_END)]) / 365.25

# Variables to include
All_lap_Data  <-  subset(All_lap_Data, select = c(AFFINITYGROUP, TITLEOFPOLICYHOLDER, 
                                                  PREMIUMPAYERDEBITORDERDAY,
                                                  VOICELOGGED, AGENTNAME, DURATION, 
                                                  GENDER, LEVELOFINCOME, EDUCATION, SMOKERNONSMOKER, RACE, CULTURE,
                                                  AGE_COM, AGE_END,
                                                  PROVINCE, DOMAIN, EXTENTION, PHONEAREA, PHONESUBAREA,
                                                  NUMBEROFBENEFICIARIES, NOCREDITPROVIDERS,
                                                  VOICELOGGEDDAY, VOICELOGGEDMONTH,
                                                  CREDITPROTECTIONDEATHSUMASSURED,
                                                  CREDITPROTECTIONDISABILITYSUMASSURED,
                                                  CREDITPROTECTIONTEMPORARYDISABILITYMONTHLYBENEFIT,
                                                  TEMPORARYDISABILITYPERIOD,
                                                  RETRENCHMENTBENEFIT,
                                                  RAFBENEFIT,
                                                  CRITICALILLNESSCOVER,
                                                  YEAR1QUOTEDTOTALPREMIUM,
                                                  POSTALCODE, PREMIUMPAYERBRANCHCODE,
                                                  PPB,
                                                  STATUS))

Options <- c("CREDITPROTECTIONDEATHSUMASSURED", "CREDITPROTECTIONDISABILITYSUMASSURED", 
             "CREDITPROTECTIONTEMPORARYDISABILITYMONTHLYBENEFIT", "TEMPORARYDISABILITYPERIOD", "RETRENCHMENTBENEFIT",
             "RAFBENEFIT", "CRITICALILLNESSCOVER", 
             "YEAR1QUOTEDTOTALPREMIUM", "AGE_COM", "AGE_END", "NUMBEROFBENEFICIARIES", "NOCREDITPROVIDERS",
             "VOICELOGGEDDAY", "VOICELOGGEDMONTH")

for (opt in Options) {
  All_lap_Data[[opt]]                              <-  as.numeric(All_lap_Data[[opt]])
  All_lap_Data[[opt]][is.na(All_lap_Data[[opt]])]  <-  0
}

All_lap_Data$PRODUCTS <- paste(ifelse(All_lap_Data$CREDITPROTECTIONDEATHSUMASSURED > 0, "A", ""),
                               ifelse(All_lap_Data$CREDITPROTECTIONDISABILITYSUMASSURED > 0, "B", ""),
                               ifelse(All_lap_Data$CREDITPROTECTIONTEMPORARYDISABILITYMONTHLYBENEFIT > 0, "C", ""),
                               ifelse(All_lap_Data$TEMPORARYDISABILITYPERIOD > 0, "D", ""),
                               ifelse(All_lap_Data$RETRENCHMENTBENEFIT > 0, "E", ""),
                               ifelse(All_lap_Data$RAFBENEFIT > 0, "F", ""),
                               ifelse(All_lap_Data$CRITICALILLNESSCOVER > 0, "G", ""),
                               sep = "")
 

All_lap_Data$AFFINITYGROUP              <-  gsub("[^[:alpha:]]", "", toupper(All_lap_Data$AFFINITYGROUP))
temp_tbl    <- data.frame(table(All_lap_Data$AFFINITYGROUP)) 
Keeper      <- as.character(temp_tbl$Var1[temp_tbl$Freq/nrow(All_lap_Data) > 0.025])
All_lap_Data$AFFINITYGROUP[!(All_lap_Data$AFFINITYGROUP %in% Keeper)] <- "OTHER"

All_lap_Data$TITLEOFPOLICYHOLDER        <-  gsub("[^[:alpha:]]", "", toupper(All_lap_Data$TITLEOFPOLICYHOLDER))

All_lap_Data$PREMIUMPAYERDEBITORDERDAY  <-  gsub("[^[:digit:]]", "", toupper(All_lap_Data$PREMIUMPAYERDEBITORDERDAY))
All_lap_Data$PREMIUMPAYERDEBITORDERDAY[is.na(All_lap_Data$PREMIUMPAYERDEBITORDERDAY)] <- ""
temp_tbl    <- data.frame(table(All_lap_Data$PREMIUMPAYERDEBITORDERDAY)) 
Keeper      <- as.character(temp_tbl$Var1[temp_tbl$Freq/nrow(All_lap_Data) > 0.025])
All_lap_Data$PREMIUMPAYERDEBITORDERDAY[!(All_lap_Data$PREMIUMPAYERDEBITORDERDAY %in% Keeper)] <- "OTHER"

All_lap_Data$AGENTNAME                  <-  gsub("[^[:alpha:]]", "", toupper(All_lap_Data$AGENTNAME))
All_lap_Data$GENDER                     <-  gsub("[^[:alpha:]]", "", toupper(All_lap_Data$GENDER))

All_lap_Data$LEVELOFINCOME              <-  gsub("[^[:alnum:]]", "", toupper(All_lap_Data$LEVELOFINCOME))
temp_tbl    <- data.frame(table(All_lap_Data$LEVELOFINCOME)) 
Keeper      <- as.character(temp_tbl$Var1[temp_tbl$Freq/nrow(All_lap_Data) > 0.025])
All_lap_Data$LEVELOFINCOME[!(All_lap_Data$LEVELOFINCOME %in% Keeper)] <- "OTHER"

All_lap_Data$EDUCATION                  <-  gsub("[^[:alnum:]]", "", toupper(All_lap_Data$EDUCATION))
All_lap_Data$SMOKERNONSMOKER            <-  gsub("[^[:alpha:]]", "", toupper(All_lap_Data$SMOKERNONSMOKER))

All_lap_Data$RACE                           <-  gsub("[^[:alpha:]]", "", toupper(All_lap_Data$RACE))
All_lap_Data$RACE[is.na(All_lap_Data$RACE)] <- ""

All_lap_Data$CULTURE                              <-  gsub("[^[:alpha:]]", "", toupper(All_lap_Data$CULTURE))
All_lap_Data$CULTURE[is.na(All_lap_Data$CULTURE)] <- ""

All_lap_Data$PROVINCE                   <-  gsub("[^[:alpha:]]", "", toupper(All_lap_Data$PROVINCE))

All_lap_Data$DOMAIN                     <-  gsub("[^[:alpha:]]", "", toupper(All_lap_Data$DOMAIN))
temp_tbl    <- data.frame(table(All_lap_Data$DOMAIN)) 
Keeper      <- as.character(temp_tbl$Var1[temp_tbl$Freq/nrow(All_lap_Data) > 0.0075])
All_lap_Data$DOMAIN[!(All_lap_Data$DOMAIN %in% Keeper)] <- "OTHER"

All_lap_Data$EXTENTION                                          <-  gsub("[^[:alpha:]]", "", toupper(All_lap_Data$EXTENTION))
Main_Ext <- c("", "ORG", "GOV", "COM", "ACZA", "NET", "COUK", "COZA")
All_lap_Data$EXTENTION[grep("ORG", All_lap_Data$EXTENTION)]     <-  "ORG"
All_lap_Data$EXTENTION[grep("GOV", All_lap_Data$EXTENTION)]     <-  "GOV"
All_lap_Data$EXTENTION[grep("COM", All_lap_Data$EXTENTION)]     <-  "COM"
All_lap_Data$EXTENTION[grep("ACZA", All_lap_Data$EXTENTION)]    <-  "ACZA"
All_lap_Data$EXTENTION[grep("NET", All_lap_Data$EXTENTION)]     <-  "NET"
All_lap_Data$EXTENTION[grep("COUK", All_lap_Data$EXTENTION)]    <-  "COUK"
All_lap_Data$EXTENTION[grep("COZA", All_lap_Data$EXTENTION)]    <-  "COZA"
All_lap_Data$EXTENTION[!(All_lap_Data$EXTENTION %in% Main_Ext)] <- "OTHER"

All_lap_Data$PHONEAREA                  <-  gsub("[^[:digit:]]", "", toupper(All_lap_Data$PHONEAREA))
temp_tbl    <- data.frame(table(All_lap_Data$PHONEAREA)) 
Keeper      <- as.character(temp_tbl$Var1[temp_tbl$Freq/nrow(All_lap_Data) > 0.015])
All_lap_Data$PHONEAREA[!(All_lap_Data$PHONEAREA %in% Keeper)] <- "OTHER"

All_lap_Data$PHONESUBAREA               <-  gsub("[^[:digit:]]", "", toupper(All_lap_Data$PHONESUBAREA))
temp_tbl    <- data.frame(table(All_lap_Data$PHONESUBAREA)) 
Keeper      <- as.character(temp_tbl$Var1[temp_tbl$Freq/nrow(All_lap_Data) > 0.0025])
All_lap_Data$PHONESUBAREA[!(All_lap_Data$PHONESUBAREA %in% Keeper)] <- "OTHER"

All_lap_Data$POSTALCODE                 <-  gsub("[^[:digit:]]", "", toupper(All_lap_Data$POSTALCODE))
temp_tbl    <- data.frame(table(All_lap_Data$POSTALCODE)) 
Keeper      <- as.character(temp_tbl$Var1[temp_tbl$Freq/nrow(All_lap_Data) > 0.0075])
All_lap_Data$POSTALCODE[!(All_lap_Data$POSTALCODE %in% Keeper)] <- "OTHER"


All_lap_Data$PREMIUMPAYERBRANCHCODE     <-  gsub("[^[:digit:]]", "", toupper(All_lap_Data$PREMIUMPAYERBRANCHCODE))
temp_tbl    <- data.frame(table(All_lap_Data$PREMIUMPAYERBRANCHCODE)) 
Keeper      <- as.character(temp_tbl$Var1[temp_tbl$Freq/nrow(All_lap_Data) > 0.0075])
All_lap_Data$PREMIUMPAYERBRANCHCODE[!(All_lap_Data$PREMIUMPAYERBRANCHCODE %in% Keeper)] <- "OTHER"

All_lap_Data$PPB                        <-  gsub("[^[:digit:]]", "", toupper(All_lap_Data$PPB))

All_lap_Data$STATUS                     <-  gsub("[^[:alpha:]]", "", toupper(All_lap_Data$STATUS))

All_lap_Data      <- All_lap_Data[All_lap_Data$VOICELOGGED <= firstDayMonth(seq(as.Date(fileXLSDate), length = 2, by = "-5 months")[2]), ]
All_lap_Data      <- All_lap_Data[All_lap_Data$AGENTNAME != "", ]
All_lap_Data$Year <- format(All_lap_Data$VOICELOGGED, format = "%Y")

All_lap_Data <- subset(All_lap_Data, select = -c(VOICELOGGED))

# NTU
NTUData        <- All_lap_Data
NTUData$STATUS <- as.numeric(ifelse(NTUData$STATUS == "NTU", 1, 0))

col_idx <- grep("STATUS", names(NTUData))
NTUData <- NTUData[, c(col_idx, (1:ncol(NTUData))[-col_idx])]

# LAPSE
LAPData        <- All_lap_Data[All_lap_Data$STATUS %in% c("LAP", "ACT"), ]
LAPData$STATUS <- as.numeric(ifelse(LAPData$STATUS == "LAP", 1, 0))

col_idx <- grep("STATUS", names(LAPData))
LAPData <- LAPData[, c(col_idx, (1:ncol(LAPData))[-col_idx])]

# ALL
ALLData        <- All_lap_Data
ALLData$STATUS <- as.numeric(ifelse(ALLData$STATUS == "ACT", 0, 1))

col_idx <- grep("STATUS", names(ALLData))
ALLData <- ALLData[, c(col_idx, (1:ncol(ALLData))[-col_idx])]

rm(All_lap_Data, col_idx, temp_tbl, Race_Data, opt, Options, Main_Ext, Keeper)

