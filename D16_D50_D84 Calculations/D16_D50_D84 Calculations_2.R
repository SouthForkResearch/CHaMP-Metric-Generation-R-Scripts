
# Read the data from CV
#all.data = read.csv("SizeClassSubstrateNotInPools_20160718.txt", header=T)
all.data = read.csv("SubstrateSizeClass_All_20160718.txt", header=T)
all.data = all.data[all.data$VisitYear != 2011,]

all.data  = all.data[all.data$SubstrateSizeClass != ">4000mm",]
all.data  = all.data[all.data$SubstrateSizeClass !="1448 - 2048mm" ,]
all.data  = all.data[all.data$SubstrateSizeClass !="2048 - 2896mm" ,]
all.data  = all.data[all.data$SubstrateSizeClass != "2896 - 4000mm"  ,]
all.data  = all.data[all.data$SubstrateSizeClass != "512 - 724mm",]
all.data  = all.data[all.data$SubstrateSizeClass !=  "724 - 1024mm",]
all.data  = all.data[all.data$SubstrateSizeClass != "Bedrock",]




levels(all.data$SubstrateSizeClass)

# enumerate all VisitID's, then we'll walk through them one at a time.
VisitIDs = levels(factor(all.data$VisitID))
n = length(VisitIDs)

all.data[is.na(all.data$SubstrateMeasurement) == F,]


# Initialize results dataframe
results = data.frame("VisitID" = rep(0, n), "D16"=rep(0,n), "D50"=rep(0,n), "D84"=rep(0,n))


# loop through visit IDs, calculating D16, D50, D84 for each
v=1
for (v in 1:length(VisitIDs)) {
print(VisitIDs[v])
	# Subset the data to just this visidID
	data = all.data[all.data$VisitID == VisitIDs[v],]
nrow(data)

	# Filter to exclude pebble records where the cross-section is associated with channel units of Tier1Type = “Slow/Pool”
	data = data[data$Tier1 != "Slow/Pool",]
      data = data[data$SubstrateSizeClass != "",]
	data = data[data$SubstrateSizeClass != "Bedrock",]


	# Pull out the "upper bound" of range for later calculation
	names(data)
	a=regexpr("-", data$SubstrateSizeClass)
	b= regexpr("mm", data$SubstrateSizeClass)
	lower.bound = substr(data$SubstrateSizeClass, 1, a-2)
	upper.bound = substr(data$SubstrateSizeClass, a+2 ,b-1)
    
	# Oddball cases
		# for "bedrock" set upper bound to 2000, lower bound to 10000
		lower.bound[data$SubstrateSizeClass=="Bedrock"] = "10000"
		upper.bound[data$SubstrateSizeClass=="Bedrock"] = "20000"

		# for ">510" 
		lower.bound[data$SubstrateSizeClass=="> 512mm"]=data$SubstrateMeasurement[data$SubstrateSizeClass=="> 512mm"]
		upper.bound[data$SubstrateSizeClass=="> 512mm"]=data$SubstrateMeasurement[data$SubstrateSizeClass=="> 512mm"]+.00001

		# for ">4000mm"
		lower.bound[data$SubstrateSizeClass==">4000mm"] = "4000"
		upper.bound[data$SubstrateSizeClass==">4000mm"] = "4001"

	lower.bound = as.numeric(lower.bound)
	upper.bound = as.numeric(upper.bound)
	data.frame(data$SubstrateSizeClass, lower.bound, upper.bound)

ln.lower.bound = log(100*sort(lower.bound))
ln.upper.bound = log(100*sort(upper.bound))


SubstrateSizeClass = data$SubstrateSizeClass[order(lower.bound)]
ord.data = data.frame(SubstrateSizeClass, ln.lower.bound, ln.upper.bound)

# initialized
ord.data$ln.interp = rep(0, nrow(ord.data))

# Assume values within each size class, assuming even (equally spaced) distribution on ln scale
#First figure out size classes
classes = levels(factor(ln.lower.bound))



#Do the interpolation within each size class.
for (c in 1:length(classes)){
# subset to just the data in this size class
c.data = ord.data[factor(ord.data$ln.lower.bound)==classes[c],]

# if there's only one value, use the mean of upper and lower bound (on ln scale)
if (nrow(c.data)==1) {
ord.data$ln.interp[factor(ord.data$ln.lower.bound)==classes[c]] = 0.5*(c.data$ln.lower.bound + c.data$ln.upper.bound)
} else {

# otherwise set values to sequence from bottom to top value, evenly spaced.
n = nrow(c.data)
ln.interp = seq(c.data$ln.lower.bound[1], c.data$ln.upper.bound[1], 
             by = ((c.data$ln.upper.bound[1]-c.data$ln.lower.bound[1])/(n)))

ord.data$ln.interp[factor(ord.data$ln.lower.bound)==classes[c]]=ln.interp

}
}


	# Interpolate to get values at 16th, 50th, and 84th percentiles and back transform
Pct = 1:nrow(ord.data)/nrow(ord.data)
A = ord.data$ln.interp
exp(A)
	# D16
		idx = trunc(.16*nrow(ord.data))
            idx
		logD16 = A[idx] + (A[idx+1]- A[idx]) * ((.16-Pct[idx])/(Pct[idx+1]-Pct[idx]))
		D16 = exp(logD16)/100
            D16
	# D50
		idx = trunc(.5*nrow(ord.data))
		logD50 = A[idx] + (A[idx+1]- A[idx]) * ((.5-Pct[idx])/(Pct[idx+1]-Pct[idx]))
		D50 = exp(logD50)/100

	# D84
		idx = trunc(.84*nrow(ord.data))
		logD84 = A[idx] + (A[idx+1]- A[idx]) * ((.84-Pct[idx])/(Pct[idx+1]-Pct[idx]))
		D84 = exp(logD84)/100

	# Assign results to dataframe	
		results$VisitID[v] = data$VisitID[1]
		results$D16[v] = D16
		results$D50[v] = D50
		results$D84[v] = D84

} # End loop through visit IDs


results

nrow(results)
cm.org = read.csv("metricvisitinformation.csv", header=T)
idx=match(results$VisitID, cm.org$VisitID)
cm.org$D16
results$D16_cm.org = cm.org$SubD16[idx]
results$D50_cm.org = cm.org$SubD50[idx]
results$D84_cm.org = cm.org$SubD84[idx]

results$D16_cm.org
plot(results$D16, results$D16_cm.org)
plot(results$D50, results$D50_cm.org)
plot(results$D84, results$D84_cm.org)

mod16=lm(results$D16 ~ results$D16_cm.org)
summary(mod16)
#R-squared = .9805


mod50=lm(results$D50 ~ results$D50_cm.org)
summary(mod50)
#R-squared = .9471

mod84=lm(results$D84 ~ results$D84_cm.org)
summary(mod84)
#R-squared = .915



# Write results to a file
write.csv(results, "D16_D50_D84.csv", row.names=F)
