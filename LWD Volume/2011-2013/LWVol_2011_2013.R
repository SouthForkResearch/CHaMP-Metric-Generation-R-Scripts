# Script to Calculate Large Wood Frequencies (LWDFreq_Wet and LWDFreq_Bf)
# Specific to 2011-2013 data tables
# Matt Nahorniak
# 1/23/2017


# Read the raw data
LWD = read.csv("LargeWoodyDebris.csv", header=T)

# Split into wet and dry parts for the different metrics.
LWD_Dry = LWD[LWD$LargeWoodType == "Dry",]
LWD_Wet = LWD[(LWD$LargeWoodType == "Wet",]


# Get VisitID's
VisitIDs=levels(factor(LWD$VisitID))

# Initialize results
LWVol_Wet = rep(NA, length(VisitIDs))
LWVol_Bf = rep(NA, length(VisitIDs))


# Loop through all VisitIDs

i = match(1954,VisitIDs)
i=1
for (i in 1:length(VisitIDs)){
print(paste(i, "of", length(VisitIDs)))

# subset data to only visitID in question
LWD_ID_Dry = LWD_Dry[LWD_Dry$VisitID == VisitIDs[i],]
LWD_ID_Wet = LWD_Wet[LWD_Wet$VisitID == VisitIDs[i],]
LWD_ID_Dry$VisitID

LWVol_Dry[is.na(LWVol_Dry)] =0
LWVol_Dry=sum(
LWD_ID_Dry$SmallSmall *  0.02035+  
LWD_ID_Dry$SmallMedium * 0.04878+
LWD_ID_Dry$SmallLarge * 0.10758+
LWD_ID_Dry$MediumSmall * 0.05981+
LWD_ID_Dry$MediumMedium * 0.15101+
LWD_ID_Dry$MediumLarge * 0.40012+
LWD_ID_Dry$LargeSmall * 0.22887+
LWD_ID_Dry$LargeMedium * 0.57739+
LWD_ID_Dry$LargeLarge * 1.72582+
LWD_ID_Dry$SmallMidLarge * 0.10470+
LWD_ID_Dry$SmallExtraLarge * 0.23794+
LWD_ID_Dry$MediumMidLarge * 0.33875+
LWD_ID_Dry$MediumExtraLarge * 0.82393+
LWD_ID_Dry$MidLargeSmall * 0.21187+
LWD_ID_Dry$MidLargeMedium * 0.51680+
LWD_ID_Dry$MidLargeMidLarge * 1.12232+
LWD_ID_Dry$MidLargeExtraLarge * 2.71169+
LWD_ID_Dry$ExtraLargeSmall * 0.84320+
LWD_ID_Dry$ExtraLargeMedium * 1.89000+
LWD_ID_Dry$ExtraLargeMidLarge * 3.82249+
LWD_ID_Dry$ExtraLargeExtraLarge * 10.54683
, na.rm=T
)

LWD_ID_Wet[is.na(LWD_ID_Wet)]=0
LWVol_Wet[i]=
sum(
LWD_ID_Wet$SmallSmall*  0.02035+  
LWD_ID_Wet$SmallMedium* 0.04878+
LWD_ID_Wet$SmallLarge* 0.10758+
LWD_ID_Wet$MediumSmall* 0.05981+
LWD_ID_Wet$MediumMedium* 0.15101+
LWD_ID_Wet$MediumLarge* 0.40012+
LWD_ID_Wet$LargeSmall* 0.22887+
LWD_ID_Wet$LargeMedium* 0.57739+
LWD_ID_Wet$LargeLarge* 1.72582+
LWD_ID_Wet$SmallMidLarge* 0.10470+
LWD_ID_Wet$SmallExtraLarge* 0.23794+
LWD_ID_Wet$MediumMidLarge* 0.33875+
LWD_ID_Wet$MediumExtraLarge* 0.82393+
LWD_ID_Wet$MidLargeSmall* 0.21187+
LWD_ID_Wet$MidLargeMedium* 0.51680+
LWD_ID_Wet$MidLargeMidLarge* 1.12232+
LWD_ID_Wet$MidLargeExtraLarge* 2.71169+
LWD_ID_Wet$ExtraLargeSmall* 0.84320+
LWD_ID_Wet$ExtraLargeMedium* 1.89000+
LWD_ID_Wet$ExtraLargeMidLarge* 3.82249+
LWD_ID_Wet$ExtraLargeExtraLarge* 10.54683
, na.rm=T
)

LWVol_Bf[i]=LWVol_Wet[i]+LWVol_Dry

}

results = data.frame("VisitID"=VisitIDs, LWVol_Bf, LWVol_Wet)
write.csv(results, "LWFreq.csv", row.names=F)

#########################################################
# Validation
##########################################################
# read the data from cm.org
MVI = read.csv("MetricVisitInformation.csv", header=T)

idx = match(results$VisitID, MVI$VisitID)
idx

write.csv(data.frame(results$LWVol_Wet== MVI$LWVol_Wet[idx],MVI[idx,]), "test.csv")

#data.frame(MVI$VisitID[idx],results$LWVol_Wet, MVI$LWVol_Wet[idx])
#data.frame(MVI$VisitID[idx],results$LWVol_Wet, MVI$LWVol_Wet[idx],results$LWVol_Wet== MVI$LWVol_Wet[idx])


plot(results$LWVol_Wet, MVI$LWVol_Wet[idx], xlab="R-Script",
ylab="cm.org", main="LWVol Script 2011-2013 Validation: 
LWVol_Wet: R-script vs CM.org value")

plot(results$LWVol_Bf, MVI$LWVol_Bf[idx], xlab="R-Script",
ylab="cm.org", main="LWVol_Bf Script 2011-2013 Validation: 
LWVol_Bf: R-script vs CM.org value")

 0.23069  -          0.25104

