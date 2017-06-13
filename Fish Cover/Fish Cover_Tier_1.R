# Calculate CHaMP Fish Cover Metrics
# Required: 
#    FishCover.csv file (exported from "All Measurements database, Fish Cover table
#    ChannelUnitSummary.csv (exported from "Program Metrics database, Channel Unit Summary table
# Matt Nahorniak, 1/18/2017



#Load the fish cover data (exported from "All Measurements" database, Fish Cover Table.
FC_data = read.csv("FishCover.csv", header=T)
# Need VisidID_CU to merge data with channel unit data
FC_data$VisitID_CU = paste(FC_data$VisitID, FC_data$ChannelUnitID)


# Load the channel unit data from the channel unit summary tab of the "Program Metrics" database
CU_data = read.csv("ChannelUnitSummary.csv", header=T)
# Need VisidID_CU to merge data with fish cover data
CU_data$VisitID_CU = paste(CU_data$VisitID, CU_data$ChUnitID)

nrow(FC_data)
nrow(CU_data)
# Merge the data
M_data=merge(CU_data, FC_data)
levels(M_data$Tier1)
nrow(M_data)

head(M_data)
# Remove data if AreaTotal is NA
M_data = M_data[is.na(M_data$AreaTotal)==F,]

# Come up with a list of VisitIDs
VisitIDs = levels(factor(M_data$VisitID))

# Pre-populate results
FishCovLW = rep(NA, length(VisitIDs))
FishCovTVeg = rep(NA, length(VisitIDs)) 
FishCovUcut = rep(NA, length(VisitIDs))
FishCovArt = rep(NA, length(VisitIDs))
FishCovAqVeg = rep(NA, length(VisitIDs))
FishCovNone = rep(NA, length(VisitIDs))
FishCovTotal = rep(NA, length(VisitIDs))

FishCovLW_FNT = rep(NA, length(VisitIDs))
FishCovTVeg_FNT = rep(NA, length(VisitIDs)) 
FishCovUcut_FNT = rep(NA, length(VisitIDs))
FishCovArt_FNT = rep(NA, length(VisitIDs))
FishCovAqVeg_FNT = rep(NA, length(VisitIDs))
FishCovNone_FNT = rep(NA, length(VisitIDs))
FishCovTotal_FNT = rep(NA, length(VisitIDs))

FishCovLW_FT = rep(NA, length(VisitIDs))
FishCovTVeg_FT = rep(NA, length(VisitIDs)) 
FishCovUcut_FT = rep(NA, length(VisitIDs))
FishCovArt_FT = rep(NA, length(VisitIDs))
FishCovAqVeg_FT = rep(NA, length(VisitIDs))
FishCovNone_FT = rep(NA, length(VisitIDs))
FishCovTotal_FT = rep(NA, length(VisitIDs))

FishCovLW_SlowPool = rep(NA, length(VisitIDs))
FishCovTVeg_SlowPool = rep(NA, length(VisitIDs)) 
FishCovUcut_SlowPool = rep(NA, length(VisitIDs))
FishCovArt_SlowPool = rep(NA, length(VisitIDs))
FishCovAqVeg_SlowPool = rep(NA, length(VisitIDs))
FishCovNone_SlowPool = rep(NA, length(VisitIDs))
FishCovTotal_SlowPool = rep(NA, length(VisitIDs))

FishCovLW_SSC = rep(NA, length(VisitIDs))
FishCovTVeg_SSC = rep(NA, length(VisitIDs)) 
FishCovUcut_SSC = rep(NA, length(VisitIDs))
FishCovArt_SSC = rep(NA, length(VisitIDs))
FishCovAqVeg_SSC = rep(NA, length(VisitIDs))
FishCovNone_SSC = rep(NA, length(VisitIDs))
FishCovTotal_SSC = rep(NA, length(VisitIDs))

Valid = rep(NA, length(VisitIDs))



i=1
# Cycle through visit IDs
for (i in 1:length(VisitIDs)){
VisitID=VisitIDs[i]

# subset data to just a single VisitID
data = M_data[M_data$VisitID == VisitID,]
data

########################################
### Here - subset to Tier1 level


########################################

data$SumFishCover
data = data[data$SumFishCover >89.99999,]

head(data)
# Figure out fraction area in each channel unit
Area_Pct_by_CU = data$AreaTotal / sum(data$AreaTotal, na.rm=T)
Area_Pct_by_CU_FNT= data$AreaTotal[data$Tier1=="Fast-NonTurbulent/Glide"] / sum(data$AreaTotal, na.rm=T)
Area_Pct_by_CU_FT= data$AreaTotal[data$Tier1=="Fast-Turbulent"] / sum(data$AreaTotal, na.rm=T)
Area_Pct_by_CU_SlowPool= data$AreaTotal[data$Tier1=="Slow/Pool"] / sum(data$AreaTotal, na.rm=T)
Area_Pct_by_CU_SSC= data$AreaTotal[data$Tier1=="Small Side Channel"] / sum(data$AreaTotal, na.rm=T)



# Fund minimu of SumFishCover.  Criteria is that it must be greater than
# or equal to 90% for a valid metric
MinSFC = min(data$SumFishCover, na.rm=T)
MinSFC

# Assign metric as valid or invalid
Valid[i]="Yes"
if (MinSFC < 90) {Valid[i] = "No"}

#Calculate each metric.  

############################

FishCovLW_by_CU= Area_Pct_by_CU*data$WoodyDebrisFC
FishCovLW[i] = sum(FishCovLW_by_CU, na.rm=T)

FishCovLW_by_CU_FNT= Area_Pct_by_CU_FNT*data$WoodyDebrisFC[data$Tier1=="Fast-NonTurbulent/Glide"]
FishCovLW_FNT[i] = sum(FishCovLW_by_CU_FNT, na.rm=T)

FishCovLW_by_CU_FT= Area_Pct_by_CU_FT*data$WoodyDebrisFC[data$Tier1=="Fast-Turbulent"]
FishCovLW_FT[i] = sum(FishCovLW_by_CU_FT, na.rm=T)

FishCovLW_by_CU_SlowPool= Area_Pct_by_CU_SlowPool*data$WoodyDebrisFC[data$Tier1=="Slow/Pool"]
FishCovLW_SlowPool[i] = sum(FishCovLW_by_CU_SlowPool, na.rm=T)

FishCovLW_by_CU_SSC= Area_Pct_by_CU_SSC*data$WoodyDebrisFC[data$Tier1=="Small Side Channel"]
FishCovLW_SSC[i] = sum(FishCovLW_by_CU_SSC, na.rm=T)


##########################

FishCovTVeg_by_CU = Area_Pct_by_CU*data$OverhangingVegetationFC
FishCovTVeg[i] = sum(FishCovTVeg_by_CU, na.rm=T)

FishCovTVeg_by_CU_FNT= Area_Pct_by_CU_FNT*data$OverhangingVegetationFC[data$Tier1=="Fast-NonTurbulent/Glide"]
FishCovTVeg_FNT[i] = sum(FishCovTVeg_by_CU_FNT, na.rm=T)

FishCovTVeg_by_CU_FT= Area_Pct_by_CU_FT*data$OverhangingVegetationFC[data$Tier1=="Fast-Turbulent"]
FishCovTVeg_FT[i] = sum(FishCovTVeg_by_CU_FT, na.rm=T)

FishCovTVeg_by_CU_SlowPool= Area_Pct_by_CU_SlowPool*data$OverhangingVegetationFC[data$Tier1=="Slow/Pool"]
FishCovTVeg_SlowPool[i] = sum(FishCovTVeg_by_CU_SlowPool, na.rm=T)

FishCovTVeg_by_CU_SSC= Area_Pct_by_CU_SSC*data$OverhangingVegetationFC[data$Tier1=="Small Side Channel"]
FishCovTVeg_SSC[i] = sum(FishCovTVeg_by_CU_SSC, na.rm=T)



##########################
FishCovUcut_by_CU = Area_Pct_by_CU*data$UndercutBanksFC
FishCovUcut[i] = sum(FishCovUcut_by_CU, na.rm=T)

FishCovUcut_by_CU_FNT= Area_Pct_by_CU_FNT*data$UndercutBanksFC[data$Tier1=="Fast-NonTurbulent/Glide"]
FishCovUcut_FNT[i] = sum(FishCovUcut_by_CU_FNT, na.rm=T)

FishCovUcut_by_CU_FT= Area_Pct_by_CU_FT*data$UndercutBanksFC[data$Tier1=="Fast-Turbulent"]
FishCovUcut_FT[i] = sum(FishCovUcut_by_CU_FT, na.rm=T)

FishCovUcut_by_CU_SlowPool= Area_Pct_by_CU_SlowPool*data$UndercutBanksFC[data$Tier1=="Slow/Pool"]
FishCovUcut_SlowPool[i] = sum(FishCovUcut_by_CU_SlowPool, na.rm=T)

FishCovUcut_by_CU_SSC= Area_Pct_by_CU_SSC*data$UndercutBanksFC[data$Tier1=="Small Side Channel"]
FishCovUcut_SSC[i] = sum(FishCovUcut_by_CU_SSC, na.rm=T)



###########################
FishCovArt_by_CU = Area_Pct_by_CU*data$ArtificialFC
FishCovArt[i] = sum(FishCovArt_by_CU, na.rm=T)

FishCovArt_by_CU_FNT= Area_Pct_by_CU_FNT*data$ArtificialFC[data$Tier1=="Fast-NonTurbulent/Glide"]
FishCovArt_FNT[i] = sum(FishCovArt_by_CU_FNT, na.rm=T)

FishCovArt_by_CU_FT= Area_Pct_by_CU_FT*data$ArtificialFC[data$Tier1=="Fast-Turbulent"]
FishCovArt_FT[i] = sum(FishCovArt_by_CU_FT, na.rm=T)

FishCovArt_by_CU_SlowPool= Area_Pct_by_CU_SlowPool*data$ArtificialFC[data$Tier1=="Slow/Pool"]
FishCovArt_SlowPool[i] = sum(FishCovArt_by_CU_SlowPool, na.rm=T)

FishCovArt_by_CU_SSC= Area_Pct_by_CU_SSC*data$ArtificialFC[data$Tier1=="Small Side Channel"]
FishCovArt_SSC[i] = sum(FishCovArt_by_CU_SSC, na.rm=T)



###########################
FishCovAqVeg_by_CU = Area_Pct_by_CU*data$AquaticVegetationFC
FishCovAqVeg[i] = sum(FishCovAqVeg_by_CU, na.rm=T)

FishCovAqVeg_by_CU_FNT= Area_Pct_by_CU_FNT*data$AquaticVegetationFC[data$Tier1=="Fast-NonTurbulent/Glide"]
FishCovAqVeg_FNT[i] = sum(FishCovAqVeg_by_CU_FNT, na.rm=T)

FishCovAqVeg_by_CU_FT= Area_Pct_by_CU_FT*data$AquaticVegetationFC[data$Tier1=="Fast-Turbulent"]
FishCovAqVeg_FT[i] = sum(FishCovAqVeg_by_CU_FT, na.rm=T)

FishCovAqVeg_by_CU_SlowPool= Area_Pct_by_CU_SlowPool*data$AquaticVegetationFC[data$Tier1=="Slow/Pool"]
FishCovAqVeg_SlowPool[i] = sum(FishCovAqVeg_by_CU_SlowPool, na.rm=T)

FishCovAqVeg_by_CU_SSC= Area_Pct_by_CU_SSC*data$AquaticVegetationFC[data$Tier1=="Small Side Channel"]
FishCovAqVeg_SSC[i] = sum(FishCovAqVeg_by_CU_SSC, na.rm=T)




###########################
FishCovNone_by_CU = Area_Pct_by_CU*data$TotalNoFC
FishCovNone[i] = sum(FishCovNone_by_CU, na.rm=T)

FishCovNone_by_CU_FNT= Area_Pct_by_CU_FNT*data$TotalNoFC[data$Tier1=="Fast-NonTurbulent/Glide"]
FishCovNone_FNT[i] = sum(FishCovNone_by_CU_FNT, na.rm=T)

FishCovNone_by_CU_FT= Area_Pct_by_CU_FT*data$TotalNoFC[data$Tier1=="Fast-Turbulent"]
FishCovNone_FT[i] = sum(FishCovNone_by_CU_FT, na.rm=T)

FishCovNone_by_CU_SlowPool= Area_Pct_by_CU_SlowPool*data$TotalNoFC[data$Tier1=="Slow/Pool"]
FishCovNone_SlowPool[i] = sum(FishCovNone_by_CU_SlowPool, na.rm=T)

FishCovNone_by_CU_SSC= Area_Pct_by_CU_SSC*data$TotalNoFC[data$Tier1=="Small Side Channel"]
FishCovNone_SSC[i] = sum(FishCovNone_by_CU_SSC, na.rm=T)

###########################
FishCovTotal[i] = sum(FishCovLW[i], FishCovTVeg[i],FishCovUcut[i], FishCovArt[i], FishCovAqVeg[i])
FishCovTotal_FNT[i] = sum(FishCovLW_FNT[i], FishCovTVeg_FNT[i],FishCovUcut_FNT[i], FishCovArt_FNT[i], FishCovAqVeg_FNT[i])
FishCovTotal_FT[i] = sum(FishCovLW_FT[i], FishCovTVeg_FT[i],FishCovUcut_FT[i], FishCovArt_FT[i], FishCovAqVeg_FT[i])
FishCovTotal_SlowPool[i] = sum(FishCovLW_SlowPool[i], FishCovTVeg_SlowPool[i],FishCovUcut_SlowPool[i], FishCovArt_SlowPool[i], FishCovAqVeg_SlowPool[i])
FishCovTotal_SSC[i] = sum(FishCovLW_SSC[i], FishCovTVeg_SSC[i],FishCovUcut_SSC[i], FishCovArt_SSC[i], FishCovAqVeg_SSC[i])



############################
print(paste(i,"of",length(VisitIDs)))
}



results = data.frame("VisitID"=VisitIDs, 
   FishCovLW, FishCovTVeg, FishCovUcut, FishCovArt, 
   FishCovAqVeg, FishCovNone, FishCovTotal,

FishCovLW_FNT, FishCovTVeg_FNT, FishCovUcut_FNT, FishCovArt_FNT, 
   FishCovAqVeg_FNT, FishCovNone_FNT, FishCovTotal_FNT,

FishCovLW_FT, FishCovTVeg_FT, FishCovUcut_FT, FishCovArt_FT, 
   FishCovAqVeg_FT, FishCovNone_FT, FishCovTotal_FT,

FishCovLW_SlowPool, FishCovTVeg_SlowPool, FishCovUcut_SlowPool, FishCovArt_SlowPool, 
   FishCovAqVeg_SlowPool, FishCovNone_SlowPool, FishCovTotal_SlowPool,

FishCovLW_SSC, FishCovTVeg_SSC, FishCovUcut_SSC, FishCovArt_SSC, 
   FishCovAqVeg_SSC, FishCovNone_SSC, FishCovTotal_SSC,

Valid)





# Write the results to a .csv file
write.csv(results,"FishCover_SiteMetrics_Tier1.csv",row.names=F)


################################################################################


### Make validation plots
valid = read.csv("MetricVisitInformation.csv", header=T)
names(valid)
idx = match(
results$VisitID
, valid$VisitID)
idx

valid$FishCovUcut[idx]

plot(results$FishCovLW, valid$FishCovLW[idx], xlab="R-Script", ylab="cm.org", 
     main = "FishCovLW R-script vs cm.org Validation")
plot(results$FishCovTVeg, valid$FishCovTVeg[idx], xlab="R-Script", ylab="cm.org", 
     main = "FishCovTVeg R-script vs cm.org Validation")
#plot(results$FishCovUcut, valid$FishCovUcut[idx], xlab="R-Script", ylab="cm.org", 
#     main = "FishCovUcut R-script vs cm.org Validation")
plot(results$FishCovArt, valid$FishCovArt[idx], xlab="R-Script", ylab="cm.org", 
     main = "FishCovArt R-script vs cm.org Validation")
plot(results$FishCovAqVeg, valid$FishCovAqVeg[idx], xlab="R-Script", ylab="cm.org", 
     main = "FishCovAqVeg R-script vs cm.org Validation")
plot(results$FishCovNone, valid$FishCovNone[idx], xlab="R-Script", ylab="cm.org", 
     main = "FishCovNone R-script vs cm.org Validation")
plot(results$FishCovTotal, valid$FishCovTotal[idx], xlab="R-Script", ylab="cm.org", 
     main = "FishCovTotal R-script vs cm.org Validation")


nrow(valid)