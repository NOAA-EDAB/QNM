# Summary plots of QNM project
# Created 9/27/2018, Robert Wildermuth

# Plot comparing scenarios for FCM and QNM models
load("C:/Users/rwildermuth/Dropbox/PhD_UMass/QNMproject/GB/msOutput/QNMresults.RData")
load("C:/Users/rwildermuth/Dropbox/PhD_UMass/QNMproject/GB/msOutput/FCMresults.RData")
resQNM <- data.frame(Concept=rownames(QNMresults$BTresults), 
                     QNM_CC=(QNMresults$BTresults[,"+"]+0.00001)-(QNMresults$BTresults[,"-"]+0.00001),
                     QNM_GF=(QNMresults$GFresults[,"+"]+0.00001)-(QNMresults$GFresults[,"-"]+0.00001),
                     QNM_CF=(QNMresults$BTandGFres[,"+"]+0.00001)-(QNMresults$BTandGFres[,"-"]+0.00001))
resFCM <- FCMresults$CC[, c("Concept","Percent_change")]
levels(resFCM$Concept) <- levels(resQNM$Concept)
names(resFCM)[2] <- "FCM_CC"
resFCM$FCM_GF <- FCMresults$GF[, "Percent_change"]
resFCM$FCM_CF <- FCMresults$CF[, "Percent_change"]
FCMvQNM <- merge(resFCM, resQNM)
plot(FCMvQNM$FCM_CC, FCMvQNM$QNM_CC, xlim = c(-40,40), main = "Georges Bank", 
     pch=1, col="red", xlab = "FCM %change", ylab = "QNM pos-neg")
# plot(scale(FCMvQNM$Percent_change, center = TRUE, scale = FALSE), 
#      scale(FCMvQNM$QNM_CC, center = TRUE, scale = FALSE), 
#      pch=1, col="red", xlab = "FCM %change", ylab = "QNM pos-neg")
points(FCMvQNM$FCM_GF, FCMvQNM$QNM_GF, pch=2, col="blue")
points(FCMvQNM$FCM_CF, FCMvQNM$QNM_CF, pch=3, col="black")
abline(h=0, col="grey");abline(v=0, col="grey")
legend("topright", legend = c("Climate change", "Groundfish fishing", "Both"), col = c("red", "blue", "black"), pch=1:3, title = "Scenario")

# Plot comparing scenarios for QNM and BBN models
incTemp <- read.delim("C:/Users/rwildermuth/Dropbox/PhD_UMass/QNMproject/GB/GB_static_IncSSTandBT.txt", 
                      stringsAsFactors = FALSE)
# tides=3 is higher tide states
resBBN <- incTemp[incTemp$X %in% c("tides", "tides1", "tides_lag1"), c(1,4)]
names(resBBN)[2] <- "BBN_CC"
# high nearshore habitat states are in first column
resBBNhab <- incTemp[incTemp$X %in% c("habNear"), c(1,2)]
names(resBBNhab)[2] <- "BBN_CC"
resBBNrest <- incTemp[!incTemp$X %in% c("tides", "tides1", "tides_lag1", "", "habNear"), c(1,3)]
names(resBBNrest)[2] <- "BBN_CC"
resBBN <- rbind(resBBN,resBBNhab,resBBNrest)
lookup <- data.frame(X=c("AT", "benthos", "BS", "BT", "commGF", "commPel", "commSF", "copepod", "detBac", "employment", "inverts", 
         "forage", "GZ", "ground", "habNear", "habPel", "habDem", "gfMAB", "precip", "PP", "profits", "PS", 
         "recParticip", "seafood", "pctLSW", "strat", "SSS", "SST", "tides", "winds"), 
         Concept=c("AirTemperature", "Benthos", "BottomSalinity", "BottomTemperature",            
         "CommercialGroundfishFishery", "CommercialPelagicFishery", "CommercialShellfishFishery", "CopepodsMicronekton",          
         "DetritusBacteria", "Employment", "FishedInvertebrates", "ForageFish",                   
         "GelatinousZooplankton", "Groundfish", "HabitatNearshore", "HabitatPelagic", 
         "HabitatSeafloorDemersal", "MidAtlanticGroundfish", "Precipitation", "PrimaryProduction",            
         "Profits", "ProtectedSpecies", "RecreationalGroundfishFishery", "Seafood",                      
         "SourceWaterProportions", "Stratification", "SurfaceSalinity",              
         "SurfaceTemperature", "TidalForcing", "Winds"))
resBBN <- merge(lookup, resBBN)

test1 <- merge(resBBN, FCMvQNM)
plot(test1$FCM_CC, test1$BBN_CC, xlim = c(-40,40), ylim = c(0,1), main = "Georges Bank", 
     pch=1, col="red", xlab = "FCM %change", ylab = "BBN p(High)")
abline(h=0.5, col="grey"); abline(v=0, col="grey")

plot(test1$QNM_CC, test1$BBN_CC, ylim = c(0,1), main = "Georges Bank", 
     pch=1, col="red", xlab = "QNM pos-neg", ylab = "BBN p(High)")
abline(h=0.5, col="grey"); abline(v=0, col="grey")
text(x=test1$QNM_CC, y=as.numeric(test1$BBN_CC), labels = levels(test1$X), pos=3)
