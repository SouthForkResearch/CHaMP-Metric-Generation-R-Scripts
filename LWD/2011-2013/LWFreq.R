# Script to Calculate Large Wood Frequencies (LWDFreq_Wet and LWDFreq_Bf)
# Specific to 2011-2013 data tables
# Matt Nahorniak
# 1/23/2017


# Read the raw data
LWD = read.csv("LargeWoodyDebris.csv", header=T)

# Split into wet and dry parts for the different metrics.
LWD_Dry = LWD[LWD$LargeWoodType == "Dry",]
LWD_Wet = LWD[LWD$LargeWoodType == "Wet",]

# read this to get site length
MVI = read.csv("MetricVisitInformation.csv", header=T)

# Get VisitID's
VisitIDs=levels(factor(LWD$VisitID))

# Initialize results
LWFreq_Wet = rep(NA, length(VisitIDs))
LWFreq_Bf = rep(NA, length(VisitIDs))


# Loop through all VisitIDs

for (i in 1:length(VisitIDs)){
print(paste(i, "of", length(VisitIDs)))

# subset data to only visitID in question
LWD_ID_Dry = LWD_Dry[LWD_Dry$VisitID == VisitIDs[i],]
LWD_ID_Wet = LWD_Wet[LWD_Wet$VisitID == VisitIDs[i],]
LWD_ID_Dry$VisitID

# Pull total number of LW pieces from the total column.
LWD_Dry_sum = sum(LWD_ID_Dry$SumLWDCount)
LWD_Wet_sum = sum(LWD_ID_Wet$SumLWDCount)

## This is what's in documentation.  It misses some columns.
#LWD_Dry_sum = sum(c(LWD_ID_Dry$SmallSmall,LWD_ID_Dry$SmallMedium,
#LWD_ID_Dry$SmallLarge,LWD_ID_Dry$MediumSmall,LWD_ID_Dry$MediumMedium,
#LWD_ID_Dry$MediumLarge,LWD_ID_Dry$LargeSmall,LWD_ID_Dry$LargeMedium,
#LWD_ID_Dry$LargeLarge))

#LWD_Wet_sum = sum(c(LWD_ID_WEt$SmallSmall,LWD_ID_Wet$SmallMedium,
#LWD_ID_Wet$SmallLarge,LWD_ID_Wet$MediumSmall,LWD_ID_Wet$MediumMedium,
#LWD_ID_Wet$MediumLarge,LWD_ID_Wet$LargeSmall,LWD_ID_Wet$LargeMedium,
#LWD_ID_Wet$LargeLarge))

idx = match(VisitIDs[i], MVI$VisitID)
# Here's what is says in the documentation
#SiteLengthBankfull = MVI$Lgth_Bf[idx]
# Here's what works
SiteLengthWetted = MVI$Lgth_Wet[idx]


# Documentation says to round off, actual results are not rounded off.
LWFreq_Wet[i]=100*LWD_Wet_sum/SiteLengthWetted
LWFreq_Bf[i]=100*((LWD_Dry_sum + LWD_Wet_sum)/SiteLengthWetted)

}

LWFreq_Wet[is.na(LWFreq_Wet)] = 0
LWFreq_Bf[is.na(LWFreq_Bf)] = 0

results = data.frame("VisitID"=VisitIDs, LWFreq_Wet, LWFreq_Bf)
write.csv(results, "LWFreq.csv", row.names=F)


idx = match(results$VisitID, MVI$VisitID)
idx
plot(results$LWFreq_Wet, MVI$LWFreq_Wet[idx], xlab="R-Script",
ylab="cm.org", main="LWFreq_Wet Script 2011-2013 Validation: 
LWFreq_Wet: R-script vs CM.org value")

plot(results$LWFreq_Bf, MVI$LWFreq_Bf[idx], xlab="R-Script",
ylab="cm.org", main="LWFreq_Bf Script 2011-2013 Validation: 
LWFreq_Bf: R-script vs CM.org value")
