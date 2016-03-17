# Loads City Data - to get province info

City_Post_Data <- fread(paste(Path, "/Data/City_Data/With_PostalCode.csv", sep = ""),
                        colClasses  =  "character",
                        header      =  TRUE,
                        skip        = 0)
City_Post_Data <- as.data.frame(City_Post_Data)

colnames(City_Post_Data)  <-  gsub(" ","", gsub("[^[:alnum:] ]", "", toupper(colnames(City_Post_Data))))

City_Data <- fread(paste(Path, "/Data/City_Data/SouthAfricanCities.csv", sep = ""),
                   colClasses  =  "character",
                   header      =  TRUE,
                   skip        = 0)
City_Data <- as.data.frame(City_Data)

colnames(City_Data)  <-  gsub(" ","", gsub("[^[:alnum:] ]", "", toupper(colnames(City_Data))))


#####################################################################################################
# Cleans Province info #
########################

# Append City with Accent City info
City_Data$ACCENTCITY   <-  gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(City_Data$ACCENTCITY)))
City_Data$CITY         <-  gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(City_Data$CITY)))
City_Data$PROVINCENAME <-  gsub(" ", "", gsub("[^[:alnum:] ]", "", toupper(City_Data$PROVINCENAME)))

ACC_temp <- City_Data[City_Data$CITY != City_Data$ACCENTCITY,]
ACC_temp <- subset(ACC_temp, select = - CITY)

colnames(ACC_temp)[colnames(ACC_temp) == "ACCENTCITY"] <- "CITY"

City_Data <- subset(City_Data, select = -ACCENTCITY)

City_Data <- rbind(City_Data, ACC_temp)

City_Data <- subset(City_Data, select = -c(LATITUDE, LONGITUDE, PROVINCEID))

City_Data <- subset(City_Data, !duplicated(CITY))

# Loads Surname - Race info

Race_Data <- fread(paste(Path, "/Data/Race_Data/Race_Data.csv", sep = ""),
                   colClasses  =  "character",
                   header      =  TRUE,
                   skip        = 0)
Race_Data <- as.data.frame(Race_Data)

colnames(Race_Data)  <-  gsub(" ","", gsub("[^[:alnum:] ]", "", toupper(colnames(Race_Data))))

Race_Data$RACE     <-  gsub("[^[:alpha:] ]", "", toupper(Race_Data$RACE))
Race_Data$SURNAME  <-  gsub(" ","", gsub("[^[:alpha:] ]", "", toupper(Race_Data$SURNAME)))
Race_Data$CULTURE  <-  gsub("[^[:alpha:] ]", "", toupper(Race_Data$CULTURE))

Race_Data <- subset(Race_Data, !duplicated(SURNAME))

####################### 
###### Load New #######
####################### 
######## Lead #########
####################### 
######## Data #########
####################### 

# Start  
lead_File_List      <-  list.files(paste(Path, "/Data/Lead_Data", sep = ""))
num_lead_file       <-  length(lead_File_List)     #  Number of files in folder 

sig <- grep("SIGNIO", lead_File_List)
bar <- grep("DPMarketingReport", lead_File_List)

SIG_DAT <- readWorksheet(loadWorkbook(paste(Path, "/Data/Lead_Data/", lead_File_List[sig], sep = "")), 1)
colnames(SIG_DAT) <- gsub(" ","", gsub("[^[:alnum:] ]", "", gsub("X.","", toupper(colnames(SIG_DAT)))))
SIG_DAT[] <- lapply(SIG_DAT, as.character)

fileXLSDate <- file.mtime(paste(Path, "/Data/Lead_Data/", lead_File_List[sig], sep = ""))

BAR_DAT <- readWorksheet(loadWorkbook(paste(Path, "/Data/Lead_Data/", lead_File_List[bar], sep = "")), 1)
colnames(BAR_DAT) <- gsub(" ","", gsub("[^[:alnum:] ]", "", gsub("X.","", toupper(colnames(BAR_DAT)))))
BAR_DAT[] <- lapply(BAR_DAT, as.character)

BAR_Names <- read_excel(paste(Path, "/Data/Lead_Col_Names/BARLOW.xlsx", sep = ""),
                        sheet = 1,
                        col_names = TRUE)

SIG_Names <- read_excel(paste(Path, "/Data/Lead_Col_Names/SIGNIO.xlsx", sep = ""),
                        sheet = 1,
                        col_names = TRUE)

DB_Names <- read_excel(paste(Path, "/Data/Lead_Col_Names/DBNAMES.xlsx", sep = ""),
                        sheet = 1,
                        col_names = TRUE)

BAR_DAT <- BAR_DAT[, colnames(BAR_DAT) %in% BAR_Names$Original]
colnames(BAR_DAT) <- BAR_Names$New[match(colnames(BAR_DAT), BAR_Names$Original)]

SIG_DAT <- SIG_DAT[, colnames(SIG_DAT) %in% SIG_Names$Original]
colnames(SIG_DAT) <- SIG_Names$New[match(colnames(SIG_DAT), SIG_Names$Original)]

#################################################################################################################################################

BAR_DAT$CLIENTIDNUMBER <- gsub(" ","", toupper(BAR_DAT$CLIENTIDNUMBER))

clID <- strsplit(gsub(" ","", toupper(BAR_DAT$CLIENTIDNUMBER)), ":")

n <- max(sapply(clID, length))
l <- lapply(clID, function(X) c(X, rep(NA, n - length(X))))

clID <- data.frame(t(do.call(cbind, l)))

BAR_DAT$CLIENTCATEGORY                                         <-  ifelse(grepl("COMPANYREG", BAR_DAT$CLIENTIDNUMBER), "COMMERCIAL", "PRIVATE")
BAR_DAT$CLIENTIDTYPE                                           <-  ifelse(grepl("PASSPORT",   BAR_DAT$CLIENTIDNUMBER), "OTHER ID",    "RSA ID")
BAR_DAT$CLIENTIDTYPE[BAR_DAT$CLIENTCATEGORY == "COMMERCIAL" ]  <-  "OTHERID"
BAR_DAT$CLIENTIDNUMBER                                         <-  as.character(clID$X2)

#################################################################################################################################################

postadr <- strsplit(gsub(" ","", toupper(BAR_DAT$POSTALADDRESS)), ",")

n <- max(sapply(postadr, length))
l <- lapply(postadr, function(X) c(X, rep(NA, n - length(X))))

postadr <- data.frame(t(do.call(cbind, l)))
colnames(postadr) <- paste("CLIENTPOSTALADDRESS", seq(1:ncol(postadr)), sep = "")

codepos <- ncol(postadr) - rowSums(is.na(postadr))

Pos_Code <- as.data.frame(as.numeric(postadr[cbind(1:nrow(postadr), codepos)]))
colnames(Pos_Code) <- "CLIENTPOSTALADDRESSPOSTALCODE"

postadr2 <- data.frame(lapply(postadr, as.character), stringsAsFactors = FALSE)
postadr2[is.na(postadr2)] <- 111
postadr2 <- data.frame(lapply(postadr2, as.numeric), stringsAsFactors = FALSE)
postadr2 <- ifelse(is.na(postadr2), TRUE, FALSE)
postadr3 <- as.matrix(postadr)
postadr  <- as.data.frame(ifelse(postadr2, postadr3, NA))

BAR_DAT <- subset(BAR_DAT, select = -POSTALADDRESS)

BAR_DAT <- cbind(BAR_DAT, postadr, Pos_Code)

BAR_DAT$AFFINITY <- "BARLOW"
BAR_DAT$SOURCE   <- "BARLOW"

#################################################################################################################################################

resadr <- trim(SIG_DAT$CLIENTRESIDENTIALADDRESS)
res_Pos_Code <- substr(resadr, nchar(resadr) - 3, nchar(resadr))
resadr <- trim(substr(resadr, 1, nchar(resadr) - 4))

posadr <- trim(SIG_DAT$CLIENTPOSTALADDRESS)
pos_Pos_Code <- substr(posadr, nchar(posadr) - 3, nchar(posadr))
posadr <- trim(substr(posadr, 1, nchar(posadr) - 4))

SIG_DAT$CLIENTPOSTALADDRESS1 <- posadr
SIG_DAT$CLIENTPOSTALADDRESSPOSTALCODE <- as.numeric(pos_Pos_Code)

SIG_DAT$CLIENTRESIDENTIALADDRESS1 <- resadr
SIG_DAT$CLIENTRESIDENTIALADDRESSPOSTALCODE <- as.numeric(res_Pos_Code)

SIG_DAT <- subset(SIG_DAT, select = -c(CLIENTRESIDENTIALADDRESS, CLIENTPOSTALADDRESS))

SIG_DAT$TRANSACTIONNUMBER <- paste("SIG", as.character(as.Date(fileXLSDate)), seq(1:nrow(SIG_DAT)), sep = "_")

SIG_DAT$SOURCE    <-  "SIGNIA"
SIG_DAT$AFFINITY  <-  SIG_DAT$MERCHANTNAME

#################################################################################################################################################

remove <- c(sig, bar)

counter <- seq(1:num_lead_file)

num_lead_file <- num_lead_file - 2

lead_File_List <- lead_File_List[!(counter %in% remove)]

for(leadfile in 1:num_lead_file){
  
  # Check to see if CSV should be created
  file_name <- lead_File_List[leadfile]

  lead_Data <- read_excel(paste(Path, "/Data/Lead_Data/", file_name, sep = ""),
                          sheet = "CONSOLIDATED",
                          col_names = TRUE,
                          skip = 8)

  lead_Data <- as.data.frame(lead_Data)
  
  colnames(lead_Data)  <-  gsub(" ","", gsub("[^[:alnum:] ]", "", toupper(colnames(lead_Data))))
  
  lead_Data <- lead_Data[lead_Data$TRANSACTIONID != "", ]
  
  lead_Data$TAKEN <- 0
  
  lead_Data[] <- lapply(lead_Data, as.character)
  
  if (length(grep("TAKEN UP", file_name)) == 0) { lead_Data$TAKEN <- 1 }
  
  if(leadfile == 1) {
    
    All_lead_Data <- lead_Data
    
  } else{
    
    common_cols <- intersect(colnames(All_lead_Data), colnames(lead_Data)) # Combine only the common columns (in case of missmatches)
    
    All_lead_Data <- rbind(
      subset(All_lead_Data,  select = common_cols), 
      subset(lead_Data,      select = common_cols)
    )
    
  }
  
  print(lead_File_List[leadfile])
  
} 

All_lead_Data$CLIENTWORKTELEPHONENUMBER <- paste(All_lead_Data$CLIENTWORKTELEPHONECODE, All_lead_Data$CLIENTWORKTELEPHONENUMBER, sep = "")
All_lead_Data$CLIENTWORKTELEPHONENUMBER <- gsub("NA", "", All_lead_Data$CLIENTWORKTELEPHONENUMBER)
All_lead_Data$CLIENTHOMETELEPHONENUMBER <- paste(All_lead_Data$CLIENTHOMETELEPHONECODE, All_lead_Data$CLIENTHOMETELEPHONENUMBER, sep = "")
All_lead_Data$CLIENTHOMETELEPHONENUMBER <- gsub("NA", "", All_lead_Data$CLIENTHOMETELEPHONENUMBER)

SIR_Names <- read_excel(paste(Path, "/Data/Lead_Col_Names/SIRITI.xlsx", sep = ""),
                        sheet = 1,
                        col_names = TRUE)

All_lead_Data <- All_lead_Data[, colnames(All_lead_Data) %in% SIR_Names$Original]
colnames(All_lead_Data) <- SIR_Names$New[colnames(All_lead_Data) %in% SIR_Names$Original]

All_lead_Data$SOURCE <- "SIRITI"

All_lead_Data <- smartbind(All_lead_Data, SIG_DAT, BAR_DAT)

All_lead_Data <- All_lead_Data[!is.na(All_lead_Data$TRANSACTIONNUMBER), ]

#################################################################################################################################################

DB_DAT <- dbReadTable(mydb, "AccessLife_Sales_File_Lead_Data")

colnames(DB_DAT)  <- gsub(" ","", gsub("[^[:alnum:] ]", "", gsub("X.","", toupper(colnames(DB_DAT)))))
DB_Names$Original <- gsub(" ","", gsub("[^[:alnum:] ]", "", gsub("X.","", toupper(DB_Names$Original))))

DB_DAT <- DB_DAT[, colnames(DB_DAT) %in% DB_Names$Original]
colnames(DB_DAT) <- DB_Names$New[match(colnames(DB_DAT), DB_Names$Original)]

#################################################################################################################################################

Stat_DAT <- dbReadTable(mydb, "ALV1_Lead_Stats")

Stat_DAT <- Stat_DAT[!is.na(Stat_DAT$Lead.Number), ]

Stat_DAT$Lead.Pick.Up.Date <- DateConv(Stat_DAT$Lead.Pick.Up.Date)
Stat_DAT$Lead.Pick.up.Time <- format(as.POSIXct(Stat_DAT$Lead.Pick.up.Time, format = "%H:%M:%S"), "%H:%M:%S")

Stat_DAT <- Stat_DAT[!is.na(Stat_DAT$Lead.Pick.Up.Date), ]
Stat_DAT <- Stat_DAT[Stat_DAT$Lead.Pick.Up.Date > as.Date("2000-12-31"), ]

Stat_DAT <- Stat_DAT[order(Stat_DAT$Lead.Pick.Up.Date, Stat_DAT$Lead.Pick.up.Time, decreasing = TRUE), ]

Stat_DAT <- Stat_DAT[!duplicated(Stat_DAT$Lead.Number), ]

Stat_DAT <- subset(Stat_DAT, select = c(Lead.Number, Reason.Status))
colnames(Stat_DAT) <- c("LEADNUMBER", "STATUS2")

DB_DAT <- merge(DB_DAT, Stat_DAT, by.x = "LEADNUMBER", by.y = "LEADNUMBER", all.x = TRUE)

#################################################################################################################################################

# Clean up

rm(ACC_temp, lead_File_List, num_lead_file, sig, bar, SIG_DAT, BAR_DAT, BAR_Names, SIG_Names, clID, n, l,
   postadr, postadr2, postadr3, codepos, Pos_Code, resadr, res_Pos_Code, posadr, pos_Pos_Code, remove, counter,
   num_lead_file, lead_File_List, fileXLSDate, common_cols, file_name, leadfile, lead_Data, SIR_Names,
   DB_Names, Stat_DAT)
















