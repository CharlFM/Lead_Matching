# Start

# Loads City/Race/ etc data - For Allocation and Modelling
source(paste(Path, "/R_Code/Load_Other_Data.R", sep = ""))

DB_Names <- read_excel(paste(Path, "/Data/Lead_Col_Names/DBNAMES.xlsx", sep = ""),
                       sheet = 1,
                       col_names = TRUE)

DB_DAT <- dbReadTable(mydb, "AccessLife_Sales_File_Lead_Data")

colnames(DB_DAT)  <- gsub(" ","", gsub("[^[:alnum:] ]", "", gsub("X.","", toupper(colnames(DB_DAT)))))
DB_Names$Original <- gsub(" ","", gsub("[^[:alnum:] ]", "", gsub("X.","", toupper(DB_Names$Original))))

DB_DAT <- DB_DAT[, colnames(DB_DAT) %in% DB_Names$Original]
colnames(DB_DAT) <- DB_Names$New[match(colnames(DB_DAT), DB_Names$Original)]

#################################################################################################################################################

Stat_DAT <- dbReadTable(mydb, "ALV1_Lead_Stats")
colnames(Stat_DAT)  <- gsub(" ","", gsub("[^[:alnum:] ]", "", gsub("X.","", toupper(colnames(Stat_DAT)))))

Stat_DAT <- Stat_DAT[!is.na(Stat_DAT$LEADNUMBER), ]

Stat_DAT$LEADPICKUPDATE <- DateConv(Stat_DAT$LEADPICKUPDATE)

###################################################################################################

Stat_DAT$TimeAMPM                                  <- format(as.POSIXct(Stat_DAT$LEADPICKUPTIME, format = "%I:%M:%OS %p"), "%H:%M:%S")
Stat_DAT$LEADPICKUPTIME[!is.na(Stat_DAT$TimeAMPM)] <- Stat_DAT$TimeAMPM[!is.na(Stat_DAT$TimeAMPM)]
Stat_DAT$LEADPICKUPTIME                            <- format(as.POSIXct(Stat_DAT$LEADPICKUPTIME, format = "%H:%M:%S"), "%H:%M:%S")

###################################################################################################

Stat_DAT <- Stat_DAT[!is.na(Stat_DAT$LEADPICKUPDATE), ]
Stat_DAT <- Stat_DAT[Stat_DAT$LEADPICKUPDATE > as.Date("2000-12-31"), ]

Stat_DAT1 <- Stat_DAT[order(Stat_DAT$LEADPICKUPDATE, Stat_DAT$LEADPICKUPTIME, decreasing = TRUE), ]
Stat_DAT2 <- Stat_DAT[order(Stat_DAT$LEADPICKUPDATE, Stat_DAT$LEADPICKUPTIME, decreasing = FALSE), ]

rm(Stat_DAT)

Stat_DAT1 <- Stat_DAT1[!duplicated(Stat_DAT1$LEADNUMBER), ]
Stat_DAT2 <- Stat_DAT2[!duplicated(Stat_DAT2$LEADNUMBER), ]

Stat_DAT1 <- subset(Stat_DAT1, select = c(LEADNUMBER, REASONSTATUS))
colnames(Stat_DAT1) <- c("LEADNUMBER", "STATUS2")

Stat_DAT2 <- subset(Stat_DAT2, select = c(LEADNUMBER, LEADPICKUPDATE, LEADPICKUPTIME))
colnames(Stat_DAT2) <- c("LEADNUMBER", "LEADPICKUPDATE", "LEADPICKUPTIME")

Stat_DAT <- merge(Stat_DAT1, Stat_DAT2, by.x = "LEADNUMBER", by.y = "LEADNUMBER")

DB_DAT <- merge(DB_DAT, Stat_DAT, by.x = "LEADNUMBER", by.y = "LEADNUMBER", all.x = TRUE)

#################################################################################################################################################

# Clean up

rm(DB_Names, Stat_DAT1, Stat_DAT2, Stat_DAT)

dbDisconnectAll()

save.image(file = paste(Path, "/Offline_WS.RData", sep = ""))
save.image(file = paste(Path, "/Model_Backup/Offline_WS ", gsub("-", "_", gsub(":", "_", Sys.time())), ".RData", sep = ""))











