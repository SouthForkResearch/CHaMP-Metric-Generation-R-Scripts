dir()

data = read.csv("RiparianStructure.csv", header=T)

VisitIDs = levels(factor(data$VisitID))
VisitIDs[1]
RipCovBigTree = rep(NA, length(VisitIDs))
RipCovConif = rep(NA, length(VisitIDs))
RipCovGrnd = rep(NA, length(VisitIDs))
RipCovNonWood = rep(NA, length(VisitIDs))
RipCovUstory = rep(NA, length(VisitIDs))
RipCovWood = rep(NA, length(VisitIDs))
RipCovCanNone = rep(NA, length(VisitIDs))



# Loop through VisitIDs
for (i in 1:length(VisitIDs)){

print(paste("i=",i,"of",length(VisitIDs)))

# Subset of data specific to this VisitID
Vdata = data[data$VisitID == VisitIDs[i],]


###################################
RipCovBigTree[i] = mean(c(Vdata$LBCanopyBigTreesGT30CmDBH, Vdata$RBCanopyBigTreesGT30CmDBH))
RipCovBigTree[i]



###################################
# RipCovConif
# Method for Year = 2011; although this doesn't agree with results.cm.org
if (Vdata$VisitYear[1]==2011) {
RipCovConif[i]=
mean(c((0.5*(Vdata$LBCanopyVegetationType=="Mixed")+ 1*(Vdata$LBCanopyVegetationType=="Coniferous")),
       (0.5*(Vdata$LBUnderstoryVegetationType=="Mixed")+ 1*(Vdata$LBUnderstoryVegetationType=="Coniferous")),
       (0.5*(Vdata$RBCanopyVegetationType=="Mixed")+ 1*(Vdata$RBCanopyVegetationType=="Coniferous")),
       (0.5*(Vdata$RBUnderstoryVegetationType=="Mixed")+ 1*(Vdata$RBUnderstoryVegetationType=="Coniferous"))
))*100
} else {
# Here's how it's done if Year > 2011
LB=tapply((Vdata$LBCanopyWoodyConiferous+Vdata$LBUnderstoryWoodyConiferous),Vdata$TransectID,mean)
RB=tapply((Vdata$RBCanopyWoodyConiferous+Vdata$RBUnderstoryWoodyConiferous),Vdata$TransectID,mean)
RipCovConif[i]=mean(c(LB,RB))
}

RipCovConif[i]


##########################
#RipCovGrnd
RipCovGrnd[i] = mean(
 c(Vdata$LBGroundcoverWoodyShrubs+Vdata$LBGroundcoverNonWoodyShrubs,
   Vdata$RBGroundcoverWoodyShrubs+Vdata$RBGroundcoverNonWoodyShrubs))

RipCovGrnd[i]



##########################
#RipCovNonWood
RipCovNonWood[i]=mean(
c(Vdata$LBUnderstoryNonWoodyShrubs + Vdata$LBGroundcoverNonWoodyShrubs,
 Vdata$RBUnderstoryNonWoodyShrubs + Vdata$RBGroundcoverNonWoodyShrubs))

RipCovNonWood[i]

######################
RipCovUstory[i]=mean(	
  c(Vdata$LBUnderstoryWoodyShrubs+Vdata$LBUnderstoryNonWoodyShrubs,
    Vdata$RBUnderstoryWoodyShrubs+Vdata$RBUnderstoryNonWoodyShrubs))
RipCovUstory[i]

######################
RipCovWood[i]=mean(
c(Vdata$LBCanopyBigTreesGT30CmDBH+Vdata$LBCanopySmallTreesLT30CmDBH+Vdata$LBUnderstoryWoodyShrubs+Vdata$LBGroundcoverWoodyShrubs,
  Vdata$RBCanopyBigTreesGT30CmDBH+Vdata$RBCanopySmallTreesLT30CmDBH+Vdata$RBUnderstoryWoodyShrubs+Vdata$RBGroundcoverWoodyShrubs))
RipCovWood[i]

################################
# Doesn't work for 2011 data.  No documentation on how this was calculated then.
RipCovCanNone[i]= 100-mean(
c(Vdata$LBCanopyWoodyConiferous+Vdata$LBCanopyWoodyDeciduous+
  Vdata$LBCanopyWoodyBroadleafEvergreen+Vdata$LBCanopyStandingDeadVegetation,
  Vdata$RBCanopyWoodyConiferous+Vdata$RBCanopyWoodyDeciduous+
  Vdata$RBCanopyWoodyBroadleafEvergreen+Vdata$RBCanopyStandingDeadVegetation)
)
RipCovCanNone[i]

} # end of cycle through VisitIDs

results = data.frame("VisitID"=VisitIDs,RipCovBigTree,RipCovConif,RipCovGrnd,RipCovNonWood,
RipCovUstory,RipCovWood,RipCovCanNone)

write.csv(results, "RiparianCover.csv", row.names=F)