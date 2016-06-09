# Allocation Rules:
# 1) Ranked based on combination of number of sales and total premium size sold in the past 5 working days
# 2) If a certain lead is 3 (or more) times more likely to be sold by another agent -> override 1)


# Get the previous 10 working days only -----------------------------------
past31 <- seq(as.Date(today), by = "-1 day", length.out = 31)

past31 <- data.frame(Days      = past31,
                     Day_Names = weekdays(past31))

weekend <- c("Saturday", "Sunday")

past31 <- past31[!(past31$Day_Names %in% weekend), ]

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

past31 <- merge(past31, Enrico_data, by.x = "Days", by.y = "Clean_Date", all.x = TRUE)

past31 <- past31[is.na(past31$PUBHOLIDAY),]

past10 <- past31$Days[rev(order(past31$Days))][2:11]

rm(Enrico_data, past31)


# Get Agent Info ----------------------------------------------------------
allocQuery <- paste("SELECT AccessLife_Sales_File_Premiums.`Lead Number`, AccessLife_Sales_File_Premiums.`Quoted Premium 1`, ",
                    "AccessLife_Sales_File_Lead_Data.`First Allocation Date`, AccessLife_Sales_File_Lead_Data.`Status` ",
                    "FROM AccessLife_Sales_File_Lead_Data ",
                    "LEFT JOIN AccessLife_Sales_File_Premiums ON AccessLife_Sales_File_Lead_Data.`AutoNumber` = AccessLife_Sales_File_Premiums.`Lead Number` ",
                    "WHERE `First Allocation Date` BETWEEN '", min(past10), "' AND '", max(past10), "'",
                    sep = "") 

alloc_Dat            <-  dbGetQuery(mydb, allocQuery)
colnames(alloc_Dat)  <-  gsub(" ", "", colnames(alloc_Dat))

alloc_Dat$SaleCount <- 0
alloc_Dat$SaleCount[alloc_Dat$Status %in% c("SALE", "Provisional Sale")] <- 1

alloc_Result <- 1








newQuery <- paste("SELECT AccessLife_Sales_File_Premiums.`Lead Number`, AccessLife_Sales_File_Premiums.`Quoted Premium 1`, ",
                  "AccessLife_Sales_File_Lead_Data.`First Allocation Date`, AccessLife_Sales_File_Lead_Data.`Status` ",
                  "FROM AccessLife_Sales_File_Lead_Data ",
                  "LEFT JOIN AccessLife_Sales_File_Premiums ON AccessLife_Sales_File_Lead_Data.`AutoNumber` = AccessLife_Sales_File_Premiums.`Lead Number` ",
                  "WHERE `First Allocation Date` BETWEEN '", min(past10), "' AND '", max(past10), "'",
                  sep = "") 




















