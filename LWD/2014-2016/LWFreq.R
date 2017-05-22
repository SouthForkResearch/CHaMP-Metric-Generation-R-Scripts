# Script to Calculate Large Wood Frequencies (LWDFreq_Wet and LWDFreq_Bf)
# Specific to 2014-2016 (and beyond) data tables
# Matt Nahorniak
# 1/23/2017

# REad the data file
LWD = read.csv("LargeWoodPiece.csv", header=T)

# Split into wet and dry components
LWD_Dry = LWD[LWD$LargeWoodType == "Dry",]
LWD_Wet = LWD[LWD$LargeWoodType == "Wet",]

# Read the metricVisitINformation file which contains the needed site length
MVI = read.csv("MetricVisitInformation.csv", header=T)

# Create list of all VisitIDs
VisitIDs=levels(factor(LWD$VisitID))

# Initialize results arrays.
LWFreq_Wet = rep(NA, length(VisitIDs))
LWFreq_Bf = rep(NA, length(VisitIDs))


i=1
# Loop through each VisitID
for (i in 1:length(VisitIDs)){
print(paste(i, "of", length(VisitIDs)))

# Get results just for the VisitID in question
LWD_ID_Dry = LWD_Dry[LWD_Dry$VisitID == VisitIDs[i],]
LWD_ID_Wet = LWD_Wet[LWD_Wet$VisitID == VisitIDs[i],]

# There is one row of data for each VisitID, so the number of rows equals
# the number of pieces of large wood.
LWD_Dry_sum = nrow(LWD_ID_Dry)
LWD_Wet_sum = nrow(LWD_ID_Wet)

# Find the corresponding row on metricvisitinformtion (MVI) to extract site length
idx = match(VisitIDs[i], MVI$VisitID)

# Here's what is says in the documentation - use bankfull site length!
SiteLengthBankfull = MVI$Lgth_Bf[idx]
# Here's what's needed to match results in cm.org - Wetted site length used
SiteLengthBankfull = MVI$Lgth_Wet[idx]

# Documentation says "round" but cm.org results aren't rounded
#LWFreq_Wet[i]=round(100*LWD_Wet_sum/SiteLengthBankfull,3)
#LWFreq_Bf[i]=round(100*((LWD_Dry_sum + LWD_Wet_sum)/SiteLengthBankfull),3)

# Results aren't rounded
LWFreq_Wet[i]=100*LWD_Wet_sum/SiteLengthBankfull
LWFreq_Bf[i]=100*((LWD_Dry_sum + LWD_Wet_sum)/SiteLengthBankfull)

}

# NA values (no rows of data) should be zero LW
LWFreq_Wet[is.na(LWFreq_Wet)] = 0
LWFreq_Bf[is.na(LWFreq_Bf)] = 0

# Write results file.
results = data.frame("VisitID"=VisitIDs, LWFreq_Wet, LWFreq_Bf)
write.csv(results, "LWFreq.csv", row.names=F)


idx = match(results$VisitID, MVI$VisitID)
idx
plot(results$LWFreq_Wet, MVI$LWFreq_Wet[idx], xlab="R-Script",
ylab="cm.org", main="LWFreq_Wet Script 2014-2016 Validation: 
LWFreq_Wet: R-script vs CM.org value")

plot(results$LWFreq_Bf, MVI$LWFreq_Bf[idx], xlab="R-Script",
ylab="cm.org", main="LWFreq_Bf Script 2014-2016 Validation: 
LWFreq_Bf: R-script vs CM.org value")
