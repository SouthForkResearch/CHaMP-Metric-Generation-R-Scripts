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
nrow(M_data)

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
Valid = rep(NA, length(VisitIDs))


VisitID=1

# Cycle through visit IDs
for (i in 1:length(VisitIDs)){
VisitID=VisitIDs[i]

# subset data to just a single VisitID
data = M_data[M_data$VisitID == VisitID,]
data
# Figure out fraction area in each channel unit
Area_Pct_by_CU = data$AreaTotal / sum(data$AreaTotal, na.rm=T)
Area_Pct_by_CU

# Fund minimu of SumFishCover.  Criteria is that it must be greater than
# or equal to 90% for a valid metric
MinSFC = min(data$SumFishCover, na.rm=T)

# Assign metric as valid or invalid
Valid[i]="Yes"
if (MinSFC < 90) {Valid[i] = "No"}

#Calculate each metric.  
FishCovLW_by_CU= Area_Pct_by_CU*data$WoodyDebrisFC
FishCovLW[i] = sum(FishCovLW_by_CU, na.rm=T)

FishCovTVeg_by_CU = Area_Pct_by_CU*data$OverhangingVegetationFC
FishCovTVeg[i] = sum(FishCovTVeg_by_CU, na.rm=T)

FishCovUcut_by_CU = Area_Pct_by_CU*data$UndercutBanksFC
FishCovUcut[i] = sum(FishCovUcut_by_CU, na.rm=T)

FishCovArt_by_CU = Area_Pct_by_CU*data$ArtificialFC
FishCovArt[i] = sum(FishCovArt_by_CU, na.rm=T)

FishCovAqVeg_by_CU = Area_Pct_by_CU*data$AquaticVegetationFC
FishCovAqVeg[i] = sum(FishCovAqVeg_by_CU, na.rm=T)

FishCovNone_by_CU = Area_Pct_by_CU*data$TotalNoFC
FishCovNone[i] = sum(FishCovNone_by_CU, na.rm=T)

FishCovTotal[i] = sum(FishCovLW[i], FishCovTVeg[i],
                      FishCovUcut[i], FishCovArt[i],
                      FishCovAqVeg[i])
print(paste(i,"of",length(VisitIDs)))
}


# Write the results to a .csv file
write.csv(data.frame("VisitID"=VisitIDs, 
   FishCovLW, FishCovTVeg, FishCovUcut, FishCovArt, 
   FishCovAqVeg, FishCovNone, FishCovTotal,Valid),"FishCover_SiteMetrics.csv",row.names=F)
