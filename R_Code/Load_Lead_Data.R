
# Loads some additional data (Race/City/...) ------------------------------

source(paste(Path, "/R_Code/Load_Other_Data.R", sep = ""))

# Define File Lists -------------------------------------------------------

lead_File_List      <-  list.files(paste(Path, "/Data/Lead_Data", sep = ""))
num_lead_file       <-  length(lead_File_List)     #  Number of files in folder 

# Loads and cleans Barlow Data --------------------------------------------

# Loads Data
bar <- grep("MARKETINGREPORT", gsub(" ", "", toupper(lead_File_List)))

BAR_DAT <- readWorksheet(loadWorkbook(paste(Path, "/Data/Lead_Data/", lead_File_List[bar], sep = "")), 
                         sheet = 1,
                         startRow = 11)

colnames(BAR_DAT) <- gsub(" ","", gsub("[^[:alnum:] ]", "", gsub("X.","", toupper(colnames(BAR_DAT)))))
BAR_DAT[] <- lapply(BAR_DAT, as.character)

BAR_Names <- readWorksheet(loadWorkbook(paste(Path, "/Data/Lead_Col_Names/BARLOW.xlsx", sep = "")), 
                         sheet = 1)

BAR_DAT <- BAR_DAT[, colnames(BAR_DAT) %in% BAR_Names$Original]
colnames(BAR_DAT) <- BAR_Names$New[match(colnames(BAR_DAT), BAR_Names$Original)]

if(ncol(BAR_DAT) != nrow(BAR_Names)) {
  stop("Barlow column missmatch")
}

# Cleans the Data

# To be adjusted If we know Private vs Commercial (for now only commercial)
BAR_DAT$CLIENTCATEGORY <- "PRIVATE"

# Cleaning of ID number no longer needed - since the format is now a pure ID number an no longer in a complex different format. 
# BAR_DAT$CLIENTIDNUMBER <- gsub(" ","", toupper(BAR_DAT$CLIENTIDNUMBER))
# 
# clID <- strsplit(gsub(" ","", toupper(BAR_DAT$CLIENTIDNUMBER)), ":")
# 
# n <- max(sapply(clID, length))
# l <- lapply(clID, function(X) c(X, rep(NA, n - length(X))))
# 
# clID <- data.frame(t(do.call(cbind, l)))
# 
# BAR_DAT$CLIENTCATEGORY                                         <-  ifelse(grepl("COMPANYREG", BAR_DAT$CLIENTIDNUMBER), "COMMERCIAL", "PRIVATE")
# BAR_DAT$CLIENTIDTYPE                                           <-  ifelse(grepl("PASSPORT",   BAR_DAT$CLIENTIDNUMBER), "OTHER ID",    "RSA ID")
# BAR_DAT$CLIENTIDTYPE[BAR_DAT$CLIENTCATEGORY == "COMMERCIAL" ]  <-  "OTHERID"
# BAR_DAT$CLIENTIDNUMBER                                         <-  as.character(clID$X2)

# 

postadr <- strsplit(BAR_DAT$POSTALADDRESS, ",")

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

# Loads and Cleans Signio Data --------------------------------------------

sig <- grep("SIGNIO", gsub(" ", "", toupper(lead_File_List)))

SIG_DAT <- readWorksheet(loadWorkbook(paste(Path, "/Data/Lead_Data/", lead_File_List[sig], sep = "")), 
                         sheet = 1,
                         startRow = 1)

colnames(SIG_DAT) <- gsub(" ","", gsub("[^[:alnum:] ]", "", gsub("X.","", toupper(colnames(SIG_DAT)))))
SIG_DAT[] <- lapply(SIG_DAT, as.character)

fileXLSDate <- file.mtime(paste(Path, "/Data/Lead_Data/", lead_File_List[sig], sep = ""))

SIG_Names <- read_excel(paste(Path, "/Data/Lead_Col_Names/SIGNIO.xlsx", sep = ""),
                        sheet = 1,
                        col_names = TRUE)

SIG_DAT <- SIG_DAT[, colnames(SIG_DAT) %in% SIG_Names$Original]
colnames(SIG_DAT) <- SIG_Names$New[match(colnames(SIG_DAT), SIG_Names$Original)]

if(ncol(SIG_DAT) != nrow(SIG_Names)) {
  stop("Signio column missmatch")
}


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

SIG_DAT$SOURCE         <-  "SIGNIO"
SIG_DAT$CLIENTCATEGORY <-  "PRIVATE"

# Load and clean All other data -------------------------------------------

# Load data
remove <- c(sig, bar)

counter <- seq(1:num_lead_file)

num_lead_file <- num_lead_file - 2 # For loop - not to loop over all files (which includes Barlow and Signio)

lead_File_List <- lead_File_List[!(counter %in% remove)]

for(leadfile in 1:num_lead_file){
  
  # Check to see if CSV should be created
  file_name <- lead_File_List[leadfile]
  
  Sheets <- getSheets(loadWorkbook(paste(Path, "/Data/Lead_Data/", file_name, sep = "")))
  Sheet  <- Sheets[grep("CONS", gsub(" ", "", toupper(Sheets)))] # Find sheet name "CONSOLIDATED" - sometimes it is spelled wrong
  
  lead_Data <- read_excel(paste(Path, "/Data/Lead_Data/", file_name, sep = ""),
                          sheet = Sheet,
                          col_names = TRUE,
                          skip = 8)
  
  lead_Data <- as.data.frame(lead_Data)
  
  colnames(lead_Data)  <-  gsub(" ","", gsub("[^[:alnum:] ]", "", toupper(colnames(lead_Data))))
  
  lead_Data <- lead_Data[(lead_Data$TRANSACTIONID != "" & !is.na(lead_Data$TRANSACTIONID)), ]
  
  lead_Data$TAKEN <- 1
  
  lead_Data[] <- lapply(lead_Data, as.character)
  
  if (length(grep("TAKENUP", gsub(" ", "", file_name))) == 0) { lead_Data$TAKEN <- 0 }
  
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

# Cleans Data

All_lead_Data$SOURCE <- "SIRITI"

All_lead_Data$CLIENTWORKTELEPHONENUMBER <- paste(All_lead_Data$CLIENTWORKTELEPHONECODE, All_lead_Data$CLIENTWORKTELEPHONENUMBER, sep = "")
All_lead_Data$CLIENTWORKTELEPHONENUMBER <- gsub("NA", "", All_lead_Data$CLIENTWORKTELEPHONENUMBER)
All_lead_Data$CLIENTHOMETELEPHONENUMBER <- paste(All_lead_Data$CLIENTHOMETELEPHONECODE, All_lead_Data$CLIENTHOMETELEPHONENUMBER, sep = "")
All_lead_Data$CLIENTHOMETELEPHONENUMBER <- gsub("NA", "", All_lead_Data$CLIENTHOMETELEPHONENUMBER)

SIR_Names <- readWorksheet(loadWorkbook(paste(Path, "/Data/Lead_Col_Names/SIRITI.xlsx", sep = "")), 
                           sheet = 1)

All_lead_Data <- All_lead_Data[, colnames(All_lead_Data) %in% SIR_Names$Original]
colnames(All_lead_Data) <- SIR_Names$New[colnames(All_lead_Data) %in% SIR_Names$Original]

All_lead_Data$SOURCE <- "SIRITI"

All_lead_Data$TAKEN <- as.numeric(All_lead_Data$TAKEN)

# Bind with other data
All_lead_Data <- smartbind(All_lead_Data, SIG_DAT, BAR_DAT)

All_lead_Data <- All_lead_Data[!is.na(All_lead_Data$TRANSACTIONNUMBER), ]

write.csv(All_lead_Data, "Data_Out.csv")

# Clean up

rm(lead_File_List, num_lead_file, sig, bar, SIG_DAT, BAR_DAT, BAR_Names, SIG_Names, clID, n, l,
   postadr, postadr2, postadr3, codepos, Pos_Code, resadr, res_Pos_Code, posadr, pos_Pos_Code, remove, counter,
   num_lead_file, lead_File_List, fileXLSDate, common_cols, file_name, leadfile, lead_Data, SIR_Names)














