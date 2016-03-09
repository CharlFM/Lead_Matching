#########################

# NTU
AgentsNTU <- NTUData %>% 
               group_by(AGENTNAME, Year) %>%
               summarise(Tot_Pols  =  n(), 
                         NTUs      =  sum(STATUS))

AgentsNTU$NTU_Rate <- AgentsNTU$NTUs / AgentsNTU$Tot_Pols

AgentsNTU_RecentYear  <-  AgentsNTU[AgentsNTU$Year == max(AgentsNTU$Year), ]
AgentsNTU_RecentYear  <-  AgentsNTU_RecentYear[with(AgentsNTU_RecentYear, order(NTU_Rate)), ]

# LAPSE
AgentsLAP <- LAPData %>% 
               group_by(AGENTNAME, Year) %>%
               summarise(Tot_Pols  =  n(), 
                         LAPs      =  sum(STATUS), 
                         Dur       =  mean(DURATION))

AgentsLAP$LAP_Rate    <-  AgentsLAP$LAPs/AgentsLAP$Tot_Pols

AgentsLAP_RecentYear  <-  AgentsLAP[AgentsLAP$Year == max(AgentsLAP$Year), ]
AgentsLAP_RecentYear  <-  AgentsLAP_RecentYear[with(AgentsLAP_RecentYear, order(LAP_Rate)), ]

# COMBINED
AgentsALL <- ALLData %>% 
               group_by(AGENTNAME, Year) %>%
               summarise(Tot_Pols  =  n(), 
                         LAPs      =  sum(STATUS), 
                         Dur       =  mean(DURATION))

AgentsALL$LAP_Rate    <-  AgentsALL$LAPs/AgentsALL$Tot_Pols

AgentsALL_RecentYear  <-  AgentsALL[AgentsALL$Year == max(AgentsALL$Year), ]
AgentsALL_RecentYear  <-  AgentsALL_RecentYear[with(AgentsALL_RecentYear, order(LAP_Rate)), ]

NTUData <- subset(NTUData, select = -c(DURATION))
LAPData <- subset(LAPData, select = -c(DURATION))
ALLData <- subset(ALLData, select = -c(DURATION))

# NEXT
# NTU
ncount <- ceiling(nrow(AgentsNTU_RecentYear) * 0.25)

TopNTU <- AgentsNTU_RecentYear$AGENTNAME[((nrow(AgentsNTU_RecentYear) - ncount) : nrow(AgentsNTU_RecentYear))              ]
MidNTU <- AgentsNTU_RecentYear$AGENTNAME[(ncount + 1)                           : (nrow(AgentsNTU_RecentYear) - ncount - 1)]
LowNTU <- AgentsNTU_RecentYear$AGENTNAME[1                                      : ncount                                   ]

# LAP
ncount <- ceiling(nrow(AgentsLAP_RecentYear) * 0.25)

TopLAP <- AgentsLAP_RecentYear$AGENTNAME[((nrow(AgentsLAP_RecentYear) - ncount) : nrow(AgentsLAP_RecentYear))]
MidLAP <- AgentsLAP_RecentYear$AGENTNAME[(ncount + 1)                           : (nrow(AgentsLAP_RecentYear) - ncount - 1)]
LowLAP <- AgentsLAP_RecentYear$AGENTNAME[1                                      : ncount]

# ALL
ncount <- ceiling(nrow(AgentsALL_RecentYear) * 0.25)

TopALL <- AgentsALL_RecentYear$AGENTNAME[((nrow(AgentsALL_RecentYear) - ncount) : nrow(AgentsALL_RecentYear))]
MidALL <- AgentsALL_RecentYear$AGENTNAME[(ncount + 1)                           : (nrow(AgentsALL_RecentYear) - ncount - 1)]
LowALL <- AgentsALL_RecentYear$AGENTNAME[1                                      : ncount]

rm(AgentsNTU_RecentYear, AgentsLAP_RecentYear, AgentsALL_RecentYear, AgentsLAP, AgentsNTU, AgentsALL, ncount)









