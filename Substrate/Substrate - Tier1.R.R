# Calculate CHaMP Substrate Metrics
# Required: 
#    SubstrateCover.csv file (exported from "All Measurements database, Fish Cover table
#    ChannelUnitSummary.csv (exported from "Program Metrics database, Channel Unit Summary table
# Matt Nahorniak, 1/18/2017


dir()
#Load the fish cover data (exported from "All Measurements" database, Substrate Cover Table.
SS_data = read.csv("SubstrateCover.csv", header=T)
# Need VisidID_CU to merge data with channel unit data
SS_data$VisitID_CU = paste(SS_data$VisitID, SS_data$ChannelUnitID)

# Load the channel unit data from the channel unit summary tab of the "Program Metrics" database
CU_data = read.csv("ChannelUnitSummary.csv", header=T)
# Need VisidID_CU to merge data with fish cover data
CU_data$VisitID_CU = paste(CU_data$VisitID, CU_data$ChUnitID)

nrow(SS_data)
nrow(CU_data)
# Merge the data
M_data=merge(CU_data, SS_data)
nrow(M_data)

# Remove data if AreaTotal is NA
M_data = M_data[is.na(M_data$AreaTotal)==F,]

# Come up with a list of VisitIDs
VisitIDs = levels(factor(M_data$VisitID))


# Pre-populate results
SubEstBldr = rep(NA, length(VisitIDs))
SubEstCbl = rep(NA, length(VisitIDs)) 
SubEstGrvl = rep(NA, length(VisitIDs))
SubEstSandFines = rep(NA, length(VisitIDs))

SubEstBldr_FNT = rep(NA, length(VisitIDs))
SubEstCbl_FNT = rep(NA, length(VisitIDs)) 
SubEstGrvl_FNT = rep(NA, length(VisitIDs))
SubEstSandFines_FNT = rep(NA, length(VisitIDs))

SubEstBldr_FT = rep(NA, length(VisitIDs))
SubEstCbl_FT = rep(NA, length(VisitIDs)) 
SubEstGrvl_FT = rep(NA, length(VisitIDs))
SubEstSandFines_FT = rep(NA, length(VisitIDs))

SubEstBldr_SlowPool = rep(NA, length(VisitIDs))
SubEstCbl_SlowPool = rep(NA, length(VisitIDs)) 
SubEstGrvl_SlowPool = rep(NA, length(VisitIDs))
SubEstSandFines_SlowPool = rep(NA, length(VisitIDs))

SubEstBldr_SSC = rep(NA, length(VisitIDs))
SubEstCbl_SSC = rep(NA, length(VisitIDs)) 
SubEstGrvl_SSC = rep(NA, length(VisitIDs))
SubEstSandFines_SSC = rep(NA, length(VisitIDs))


Valid = rep(NA, length(VisitIDs))


VisitID=1

i=1
# Cycle through visit IDs
for (i in 1:length(VisitIDs)){
VisitID=VisitIDs[i]

# subset data to just a single VisitID
data = M_data[M_data$VisitID == VisitID,]

# eliminate data from channel units with SumSubstrateCover less than 90$
data = data[data$SumSubstrateCover > 89.9999,]
# Figure out fraction area in each channel unit

Area_Pct_by_CU = data$AreaTotal / sum(data$AreaTotal, na.rm=T)
Area_Pct_by_CU
data$Tier1

# Fund minimu of SumFishCover.  Criteria is that it must be greater than
# or equal to 90% for a valid metric
MinSSC = min(data$SumSubstrateCover, na.rm=T)

## Assign metric as valid or invalid
Valid[i]="Yes"
if (MinSSC < 90) {Valid[i] = "No"}


#Calculate each metric.  
Bldr_by_CU= Area_Pct_by_CU*data$BouldersGT256
SubEstBldr[i] = sum(Bldr_by_CU, na.rm=T)
SubEstBldr_FNT[i] = sum(Bldr_by_CU[data$Tier1 == "Fast-NonTurbulent/Glide"], na.rm=T)
SubEstBldr_FT[i] = sum(Bldr_by_CU[data$Tier1 == "Fast-Turbulent"], na.rm=T)
SubEstBldr_SlowPool[i] = sum(Bldr_by_CU[data$Tier1 == "Slow/Pool"], na.rm=T)
SubEstBldr_SSC[i] = sum(Bldr_by_CU[data$Tier1 == "Small Side Channel"], na.rm=T)



Cbl_by_CU = Area_Pct_by_CU*data$Cobbles65255
SubEstCbl[i] = sum(Cbl_by_CU, na.rm=T)
SubEstCbl_FNT[i] = sum(Cbl_by_CU[data$Tier1 == "Fast-NonTurbulent/Glide"], na.rm=T)
SubEstCbl_FT[i] = sum(Cbl_by_CU[data$Tier1 == "Fast-Turbulent"], na.rm=T)
SubEstCbl_SlowPool[i] = sum(Cbl_by_CU[data$Tier1 == "Slow/Pool"], na.rm=T)
SubEstCbl_SSC[i] = sum(Cbl_by_CU[data$Tier1 == "Small Side Channel"], na.rm=T)



CoarseGrvl_by_CU = Area_Pct_by_CU*data$CoarseGravel1764
FineGrvl_by_CU =  Area_Pct_by_CU*data$FineGravel316
SubEstGrvl[i] = sum(CoarseGrvl_by_CU, na.rm=T)+sum(FineGrvl_by_CU, na.rm=T)
SubEstGrvl_FNT[i] = sum(CoarseGrvl_by_CU[data$Tier1 == "Fast-NonTurbulent/Glide"], na.rm=T)+sum(FineGrvl_by_CU[data$Tier1 == "Fast-NonTurbulent/Glide"], na.rm=T)
SubEstGrvl_FT[i] = sum(CoarseGrvl_by_CU[data$Tier1 == "Fast-Turbulent"], na.rm=T)+sum(FineGrvl_by_CU[data$Tier1 == "Fast-Turbulent"], na.rm=T)
SubEstGrvl_SlowPool[i] = sum(CoarseGrvl_by_CU[data$Tier1 == "Slow/Pool"], na.rm=T)+sum(FineGrvl_by_CU[data$Tier1 == "Slow/Pool"], na.rm=T)
SubEstGrvl_SSC[i] = sum(CoarseGrvl_by_CU[data$Tier1 == "Small Side Channel"], na.rm=T)+sum(FineGrvl_by_CU[data$Tier1 == "Small Side Channel"], na.rm=T)



Sand_by_CU = Area_Pct_by_CU*data$Sand0062
Fines_by_CU = Area_Pct_by_CU*data$FinesLT006
SubEstSandFines[i] = sum(Sand_by_CU, na.rm=T)+sum(Fines_by_CU, na.rm=T)
SubEstSandFines_FNT[i] = sum(Sand_by_CU[data$Tier1 == "Fast-NonTurbulent/Glide"], na.rm=T)+sum(Fines_by_CU[data$Tier1 == "Fast-NonTurbulent/Glide"], na.rm=T)
SubEstSandFines_FT[i] = sum(Sand_by_CU[data$Tier1 == "Fast-Turbulent"], na.rm=T)+sum(Fines_by_CU[data$Tier1 == "Fast-Turbulent"], na.rm=T)
SubEstSandFines_SlowPool[i] = sum(Sand_by_CU[data$Tier1 == "Slow/Pool"], na.rm=T)+sum(Fines_by_CU[data$Tier1 == "Slow/Pool"], na.rm=T)
SubEstSandFines_SSC[i] = sum(Sand_by_CU[data$Tier1 == "Small Side Channel"], na.rm=T)+sum(Fines_by_CU[data$Tier1 == "Small Side Channel"], na.rm=T)




print(paste(i,"of",length(VisitIDs)))
}


results = data.frame("VisitID"=VisitIDs, Valid,
   SubEstBldr, SubEstCbl, SubEstGrvl, SubEstSandFines,
   SubEstBldr_FNT, SubEstCbl_FNT, SubEstGrvl_FNT, SubEstSandFines_FNT,
   SubEstBldr_FT, SubEstCbl_FT, SubEstGrvl_FT, SubEstSandFines_FT,
   SubEstBldr_SlowPool, SubEstCbl_SlowPool, SubEstGrvl_SlowPool, SubEstSandFines_SlowPool,
   SubEstBldr_SSC, SubEstCbl_SSC, SubEstGrvl_SSC, SubEstSandFines_SSC
)



# Write the results to a .csv file
write.csv(results,"Substrate_SiteMetrics_Tier1.csv",,row.names=F)





###################################################################################
#  Validation
####################################################################################

MVI = read.csv("MetricVisitInformation.csv", header=T)

idx = match(results$VisitID, MVI$VisitID)
idx

windows(record=T)
plot(results$SubEstBldr, MVI$SubEstBldr[idx], xlab="R-Script",
ylab="cm.org", main="Substrate Script Validation: 
SubEstBldr: R-script vs CM.org value")

plot(results$SubEstCbl, MVI$SubEstCbl[idx], xlab="R-Script",
ylab="cm.org", main="Substrate Script Validation: 
SubEstCbl: R-script vs CM.org value")

plot(results$SubEstGrvl, MVI$SubEstGrvl[idx], xlab="R-Script",
ylab="cm.org", main="Substrate Script Validation: 
SubEstGrvl: R-script vs CM.org value")

plot(results$SubEstSandFines, MVI$SubEstSandFines[idx], xlab="R-Script",
ylab="cm.org", main="Substrate Script Validation: 
SubEstSandFines: R-script vs CM.org value")

