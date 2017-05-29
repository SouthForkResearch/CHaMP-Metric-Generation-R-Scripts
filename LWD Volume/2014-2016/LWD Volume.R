
# Script to Calculate Large Woody Debris Volume
# Specific to 2014-2016 (and beyond) data tables
# Matt Nahorniak
# 5/26/2017

# Modifies from LWFrequency Script....

# REad the data file
LWD = read.csv("LargeWoodPiece.csv", header=T)
CU = read.csv("ChannelUnit.csv",header=T)
nrow(LWD)
nrow(CU)

VCU_LWD=paste(LWD$VisitID,"_", LWD$ChannelUnitID,sep="")
VCU_CU = paste(CU$VisitID,"_", CU$ChannelUnitID,sep="")
idx = match(VCU_LWD, VCU_CU)

LWD$Tier1 = CU$Tier1[idx]
LWD$Tier1

# Split into wet and dry components
LWD_Dry = LWD[LWD$LargeWoodType == "Dry",]
LWD_Wet = LWD[LWD$LargeWoodType == "Wet",]

# Read the metricVisitINformation file which contains the needed site length
MVI = read.csv("MetricVisitInformation.csv", header=T)

# Create list of all VisitIDs
VisitIDs=levels(factor(LWD$VisitID))

# Initialize results arrays.
LWVol_Bf = rep(NA, length(VisitIDs))
LWVol_Wet =  rep(NA, length(VisitIDs))
LWVol_BfFstNT =  rep(NA, length(VisitIDs))
LWVol_WetFstNT =  rep(NA, length(VisitIDs))
LWVol_BfFstTurb=  rep(NA, length(VisitIDs))
LWVol_WetFstTurb =  rep(NA, length(VisitIDs))
LWVol_BfSlow =  rep(NA, length(VisitIDs))
LWVol_WetSlow =  rep(NA, length(VisitIDs))

i=1
# Loop through each VisitID
for (i in 1:length(VisitIDs)){
print(paste(i, "of", length(VisitIDs)))

# Get results just for the VisitID in question
LWD_ID_Dry = LWD_Dry[LWD_Dry$VisitID == VisitIDs[i],]
LWD_ID_Wet = LWD_Wet[LWD_Wet$VisitID == VisitIDs[i],]

LWVol_Dry = pi*sum((0.5* LWD_ID_Dry$Diameter)^2 * LWD_ID_Dry$Length)
LWVol_Wet[i] = pi*sum((0.5* LWD_ID_Wet$Diameter)^2 * LWD_ID_Wet$Length)
LWVol_Bf[i] = LWVol_Wet[i] + LWVol_Dry

LWD_ID_Dry$Tier1

LWVol_DryFstNT = pi*sum((0.5* LWD_ID_Dry$Diameter[LWD_ID_Dry$Tier1=="Fast-NonTurbulent/Glide"])^2 * 
     LWD_ID_Dry$Length[LWD_ID_Dry$Tier1=="Fast-NonTurbulent/Glide"])
LWVol_WetFstNT[i] = pi*sum((0.5* LWD_ID_Wet$Diameter[LWD_ID_Wet$Tier1=="Fast-NonTurbulent/Glide"])^2 * 
     LWD_ID_Wet$Length[LWD_ID_Wet$Tier1=="Fast-NonTurbulent/Glide"])
LWVol_BfFstNT[i] = LWVol_DryFstNT + LWVol_WetFstNT[i]


LWVol_DryFstTurb = pi*sum((0.5* LWD_ID_Dry$Diameter[LWD_ID_Dry$Tier1=="Fast-Turbulent"])^2 * 
     LWD_ID_Dry$Length[LWD_ID_Dry$Tier1=="Fast-Turbulent"])
LWVol_WetFstTurb[i] = pi*sum((0.5* LWD_ID_Wet$Diameter[LWD_ID_Wet$Tier1=="Fast-Turbulent"])^2 * 
     LWD_ID_Wet$Length[LWD_ID_Wet$Tier1=="Fast-Turbulent"])
LWVol_BfFstTurb[i] = LWVol_DryFstTurb + LWVol_WetFstTurb[i]


LWVol_DrySlow = pi*sum((0.5* LWD_ID_Dry$Diameter[LWD_ID_Dry$Tier1=="Slow/Pool"])^2 * 
     LWD_ID_Dry$Length[LWD_ID_Dry$Tier1=="Slow/Pool"])
LWVol_WetSlow[i] = pi*sum((0.5* LWD_ID_Wet$Diameter[LWD_ID_Wet$Tier1=="Slow/Pool"])^2 * 
     LWD_ID_Wet$Length[LWD_ID_Wet$Tier1=="Slow/Pool"])
LWVol_BfSlow[i] = LWVol_DrySlow + LWVol_WetSlow[i]




}




results=data.frame(LWVol_Wet, LWVol_Bf, LWVol_WetFstNT, 
        LWVol_BfFstNT,LWVol_WetFstTurb, LWVol_BfFstTurb,LWVol_WetSlow,LWVol_BfSlow)

write.csv(results, "LWD Volume.csv")
head(results)

MVI = read.csv("MetricVisitInformation.csv", header=T)
idx = match(VisitIDs, MVI$VisitID)
idx

LWVol_BfFstNT
plot(results$LWVol_Wet,MVI$LWVol_Wet[idx],
xlab="computed value", ylab="cm.org value",
main = "LWVol_Wetted: Computed vs cm.org Value")

plot(results$LWVol_Bf,MVI$LWVol_Bf[idx])
plot(results$LWVol_BfFstNT,MVI$LWVol_BfFstNT[idx])
plot(results$LWVol_WetFstNT,MVI$LWVol_WetFstNT[idx])

plot(results$LWVol_BfFstTurb,MVI$LWVol_BfFstTurb[idx],
xlab="computed value", ylab="cm.org value",
main = "LWVol_Bankfull: Computed vs cm.org Value")



plot(results$LWVol_WetFstTurb,MVI$LWVol_WetFstTurb[idx])

plot(results$LWVol_BfSlow,MVI$LWVol_BfSlow[idx])
plot(results$LWVol_WetSlow,MVI$LWVol_WetSlow[idx])
