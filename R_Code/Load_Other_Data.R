# Which Agents are currently active

  #Get active agents from DB
  # Opens DB connection
  source(paste(Path, "/R_Code/OpenDB.R", sep = ""))
  
  Active_Agents_Data <- dbReadTable(my_new_db, "agents")
  
  dbDisconnectAll()
  
  #Convert to data frame and make into column of names
  Active_Agents_Data <- as.data.frame(Active_Agents_Data)
  Active_Agents_Data <- Active_Agents_Data[Active_Agents_Data$Active == "YES",]
  Active_Agents_Data <- paste(toupper(Active_Agents_Data$AgentName), toupper(Active_Agents_Data$AgentSurname), sep = "")
  Active_Agents_Data <- gsub(" ","", gsub("[^[:alnum:] ]", "", Active_Agents_Data))


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

# Clean up

rm(ACC_temp)















