dir()
Ucut = read.csv("UndercutBank.csv")
Metrics = read.csv("MetricVisitInformation.csv", header=T)


VisitID
SiteName

Visits = levels(factor(Ucut$VisitID))
Visits

Ucut_Lgth = rep(NA, length(Visits))
Ucut_Area = rep(NA, length(Visits))
UcutLgth_Pct = rep(NA, length(Visits))
UcutArea_Pct = rep(NA, length(Visits))
SiteName = rep(NA, length(Visits))

k=3

for (k in 1:length(Visits)){
print(paste(k,"of",length(Visits)))

Visit = Visits[k]
data = Ucut[Ucut$VisitID == Visit,]
Ucut_Lgth[k] = sum(data$EstimatedLength)
Ucut_Area[k] = sum(data$AverageWidth * data$EstimatedLength)
#Ucut_Area[k] = sum(data$EstimatedUndercutArea)

Ucut_Area
idx = match(Visit, Metrics$VisitID)

UcutLgth_Pct[k] = Ucut_Lgth[k] / Metrics$Lgth_Wet[idx]*100
UcutArea_Pct[k] = Ucut_Area[k] / Metrics$Area_Wet[idx]*100
SiteName[k] = Metrics$SiteName[idx]

}

#saved=Ucut_Area
data.frame(saved, Ucut_Area)

results = data.frame(SiteName, "VisitID"=Visits, Ucut_Lgth, UcutLgth_Pct,
                                                 Ucut_Area, UcutArea_Pct)

results
write.csv(results, "Undercut.csv")



######################3
# Validation
idx = match(Visits, Metrics$VisitID)

plot(results$UcutLgth_Pct, Metrics$UcutLgth_Pct[idx],main="UcutLgth_Pct Validation:
Calculated vs CM.org",xlab="Calculated UcutLgth_Pct", ylab="cm.org UcutLgth_Pct")

# interesting that Ucut_Area matches exactly w/ cm.org, but UcutArea_Pct doesn't, since I'm
# I'm getting Area_Wet from cm.org.  I'm guessing the Area_Wet used in the calculation
# of UcutArea_Pct isn't exactly the same as the rported Area_Wet.

plot(results$UcutArea_Pct, Metrics$UcutArea_Pct[idx],main="UcutArea_Pct Validation:
Calculated vs CM.org",xlab="Calculated UcutArea_Pct", ylab="cm.org UcutArea_Pct")

plot(results$Ucut_Area, Metrics$Ucut_Area[idx],main="UcutArea Validation:
Calculated vs CM.org",xlab="Calculated Ucut_Area", ylab="cm.org Ucut_Area")





