#################### Teresa's Roadside Data #########################

scen1dbf = read.dbf("C:/Users/tgrant/Documents/Monarchs GIS/Shapefiles/ToxSims/scen2woBuffs_export.dbf")
Terdbf = read.dbf("C:/Users/tgrant/Documents/Monarch Butterflies/Projection+Tox Model/Teresa data/Teresa_data.dbf")

hist(Terdbf$Count_)
hist(Terdbf$Sum_N_Eggs)

plot(Terdbf$Count_, Terdbf$Sum_N_Eggs)
abline(lm(Terdbf$Sum_N_Eggs ~ Terdbf$Count_))

lr = lm(Terdbf$Sum_N_Eggs ~ Terdbf$Count_)
summary(lr)


#run 8 - baseline probEggs
run8EZ = read.csv("CumEggsPerZone.2017.Oct.06.12_14_34.txt")
sum(run8EZ$Eggs)
#run 9 is calibrated probEggs
run9EZ = read.csv("CumEggsPerZone.2017.Oct.06.17_47_47.txt")
sum(run9EZ$Eggs)


Denresults = data.frame(matrix(nrow=11332, ncol=5))
colnames(Denresults) = c("PolygonID", "HabType", "CumEggs","PolygonAreaLL","ProbEggs")

dens = run9EZ
nrow(dens) #should be 11,332*10=113,320

#loop through dens to create object with densities for each polygon - 1 min
system.time(
  for(i in 1:11332)
  {
    densi = filter(dens, ID == i) #combine all 20 instances for each polygon
    Denresults[i,1] = densi[1,3] #polygon ID number
    Denresults[i,2] = as.character(densi[1,4]) #habitat type
    Denresults[i,3] = sum(densi$Eggs) #sum across the 20 instances to get total eggs for per polygon
    Denresults[i,4] = densi[1,6] #polygon area - sometimes lat/long, depends on shapefile
    Denresults[i,5] = densi[1,5] #probEggs
  }
)

Denresults8 = Denresults
Denresults9 = Denresults

#select records from Denresults that match OBJECT_IDs from shapefile
Tpolyrecords = Denresults8[is.element(Denresults8$PolygonID, TIDs),]
Tpolyrecords9 = Denresults9[is.element(Denresults9$PolygonID, TIDs),]

#objectID_1 from shapefile dbf which should be equal to ID# from denresults
Tpolys = read.dbf("C:/Users/tgrant/Documents/Monarchs GIS/Shapefiles/ToxSims/TeresaPolys.dbf")
TIDs = c(Tpolys$OBJECTID_1)

#dataframe for mean and SD eggs per habitat type from the simulation run 8
SimEggs = data.frame(matrix(nrow = 5, ncol = 2))
colnames(SimEggs) = c("Mean","SD")
rownames(SimEggs) = c("MWROW0","MWROW1-5","MWROW5-20","MWROW20-60","MWROW60-100")
SimEggs[1,1] = mean(filter(Tpolyrecords, HabType == "MWROW0")$CumEggs)
SimEggs[2,1] = mean(filter(Tpolyrecords, HabType == "MWROW1-5")$CumEggs)
SimEggs[3,1] = mean(filter(Tpolyrecords, HabType == "MWROW5-20")$CumEggs)
SimEggs[4,1] = mean(filter(Tpolyrecords, HabType == "MWROW20-60")$CumEggs)
SimEggs[5,1] = mean(filter(Tpolyrecords, HabType == "MWROW60-100")$CumEggs)
SimEggs[1,2] = sd(filter(Tpolyrecords, HabType == "MWROW0")$CumEggs)
SimEggs[2,2] = sd(filter(Tpolyrecords, HabType == "MWROW1-5")$CumEggs)
SimEggs[3,2] = sd(filter(Tpolyrecords, HabType == "MWROW5-20")$CumEggs)
SimEggs[4,2] = sd(filter(Tpolyrecords, HabType == "MWROW20-60")$CumEggs)
SimEggs[5,2] = sd(filter(Tpolyrecords, HabType == "MWROW60-100")$CumEggs)
write.csv(SimEggs, "SimEggs.csv")

#dataframe for mean and SD eggs per habitat type from the simulation run 9
SimEggs9 = data.frame(matrix(nrow = 5, ncol = 2))
colnames(SimEggs9) = c("Mean","SD")
rownames(SimEggs9) = c("MWROW0","MWROW1-5","MWROW5-20","MWROW20-60","MWROW60-100")
SimEggs9[1,1] = mean(filter(Tpolyrecords9, HabType == "MWROW0")$CumEggs)
SimEggs9[2,1] = mean(filter(Tpolyrecords9, HabType == "MWROW1-5")$CumEggs)
SimEggs9[3,1] = mean(filter(Tpolyrecords9, HabType == "MWROW5-20")$CumEggs)
SimEggs9[4,1] = mean(filter(Tpolyrecords9, HabType == "MWROW20-60")$CumEggs)
SimEggs9[5,1] = mean(filter(Tpolyrecords9, HabType == "MWROW60-100")$CumEggs)
SimEggs9[1,2] = sd(filter(Tpolyrecords9, HabType == "MWROW0")$CumEggs)
SimEggs9[2,2] = sd(filter(Tpolyrecords9, HabType == "MWROW1-5")$CumEggs)
SimEggs9[3,2] = sd(filter(Tpolyrecords9, HabType == "MWROW5-20")$CumEggs)
SimEggs9[4,2] = sd(filter(Tpolyrecords9, HabType == "MWROW20-60")$CumEggs)
SimEggs9[5,2] = sd(filter(Tpolyrecords9, HabType == "MWROW60-100")$CumEggs)
write.csv(SimEggs9, "SimEggs9.csv")


#graph simulated vs real eggs per polygon

CompareByPolygon = Tpolys[,c(1,7)]
CompareByPolygon = cbind(CompareByPolygon,Tpolyrecords[,c(1,3)])
plot(CompareByPolygon$Sum_N_Eggs,CompareByPolygon$CumEggs8, xlab = "Real Eggs Laid", ylab = "Simulated Eggs Laid", ylim = c(0,120), 
     main = "Simulated vs. Real Eggs Laid")
abline(0,1)
# in general, too many eggs in low MW density plots, and not enough eggs in high density plots
lr8 = lm(CompareByPolygon$CumEggs8 ~ CompareByPolygon$Sum_N_Eggs)
summary(lr8) #R^2 = 0.2368

# add run 9
CompareByPolygon = cbind(CompareByPolygon,Tpolyrecords9[,c(1,3)])
colnames(CompareByPolygon) = c(colnames(CompareByPolygon)[1:2],"PolygonID8","CumEggs8","PolygonID9","CumEggs9")
plot(CompareByPolygon$Sum_N_Eggs,CompareByPolygon$CumEggs9, xlab = "Real Eggs Laid", ylab = "Simulated Eggs Laid", ylim = c(0,120),
     main = "Simulated vs. Real Eggs Laid")
abline(0,1)
lr9 = lm(CompareByPolygon$CumEggs9 ~ CompareByPolygon$Sum_N_Eggs)
summary(lr9) #R^2 = 0.04167



