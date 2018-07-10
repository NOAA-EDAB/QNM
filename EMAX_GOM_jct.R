#EMAX
#SML

#User parameters
if(Sys.info()['sysname']=="Windows"){
    data.dir <- "C:/Users/Sean.Lucey/Desktop/Rpath_code/data"
    out.dir  <- "C:/Users/Sean.Lucey/Desktop/Rpath_code/outputs"
}
if(Sys.info()['sysname']=="Linux"){
    data.dir <- "/home/slucey/slucey/Rpath_code/data"
    out.dir  <- "/home/slucey/slucey/Rpath_code/outputs"
}


#-------------------------------------------------------------------------------
#Required packages
library(data.table); library(Rpath)

#-------------------------------------------------------------------------------
#User created functions

#-------------------------------------------------------------------------------
setwd("~/JCT work/WGNARS/ecosim to qpress/GOMmodel")
#Georges Bank
groups <- c("Phytoplankton", "Bacteria", "Microzooplankton", 
            "Small copepods", "Large copepods", "Gelatinous zooplankton", "Micronekton", 
            "Macrobenthos polychaetes", "Macrobenthos crustaceans", 
            "Macrobenthos molluscs", "Macrobenthos other", "Megabenthos filterers", 
            "Megabenthos", "Shrimps", "Larval juv fish all", 
            "Small Pelagics commercial", "Small Pelagics other", "Small Pelagics squid", 
            "Small Pelagics anadromous", "Medium Pelagics piscivores and other", 
            "Demersals benthivores", "Demersals omnivores", "Demersals piscivores", 
            "Sharks pelagics", "HMS", "Pinnipeds", "Baleen Whales", "Odontocetes", 
            "Sea Birds", "Discards", "Detritus", "Fishery")

types <- c(1, rep(0, 28), rep(2, 2), 3)

GOM.params <- create.rpath.params(groups, types)

GOM.params$model$Biomass <- c(22.126, 5.484, 4.885, 10.403, 11.955, 1.283, 4.874, 
                              18.942, 4.04, 9.866, 24.936, 2.879, 3.505, 0.396, 0.207, 
                              5.714, 1.275, 0.29, 0.153, 0.0229, 2.981, 0.4, 4.006, 
                              0.00296, 0.00587, 0.063, 0.602, 0.0336, 0.0035, 0.442, 
                              100, NA)

GOM.params$model$PB <- c(163.143, 91.25, 72, 30.918, 35, 35, 14.25, 2.55, 3.3, 2.24, 
                         2.04, 0.864, 1.68, 2, 15, 0.52, 0.44, 1.4, 0.437, 0.649, 
                         0.459, 0.54, 0.55, 0.15, 0.5, 0.0673, 0.042, 0.04, 0.275, 
                         55.84456, 29.90675, NA)


GOM.params$model$QB <- c(NA, 380.208, 242.424, 127.75, 109.5, 146, 36.5, 17.5, 21, 
                         13.72, 11.777, 10, 11.03, 5, 45, 1.882, 2, 2, 2, 1.428, 
                         0.9, 0.9, 1.014, 0.623, 2.362, 4.85, 2.3, 8.5, 5.362, 
                         rep(NA, 3))

GOM.params$model$BioAcc <- c(rep(0, 31), NA)

GOM.params$model$Unassim <- c(0, 0.2, 0.1, 0.25, 0.25, 0.35, 0.25, 0.5, 0.5, 0.6, 
                              0.5, 0.7, 0.3, 0.3, 0.15, 0.15, 0.35, 0.15, 0.15, 0.15, 
                              0.3, 0.35, 0.15, 0.15, 0.15, 0.2, 0.2, 0.2, 0.15, 
                              0, 0, NA)

GOM.params$model$Discards <- c(rep(0, 31), 1)

GOM.params$model$'Detritus' <- c(rep(1, 29), 0, 0, 0)

GOM.params$model$Fishery <- c(rep(0, 11), 0.0917, 0.345, 0.0622, 0, 0.874, 0.0000151,
                              0.0138, 0.00561, 0.0101, 0.142, 0.00718, 0.301, 0.00024,
                              0.00182, rep(0, 6), NA)

GOM.params$model$Fishery.disc <- c(rep(0, 5), 6.36E-07, 0, 0.000135, 0.0000183, 
                                   0.000189, 0.000724, 0.0302, 0.104, 0.0195, 2.65E-09,
                                   0.131, 0.000156, 0.000562, 0.0167, 0.00303, 0.043,
                                   0.00228, 0.0904, 0.0000721, 0.000545, 0.00115,
                                   0.000405, 0.000139, 0.0000588, 0, 0, NA)

#Diet - import from Sarah's C code
DC.groups <- groups[which(!groups %in% c('Discards', 'Detritus', 'Fishery'))]

DC.c.code <-as.data.table(matrix(c(
  0,0,0.4305193,0.2117425,0.7240587,0.6317893,0.08602893,0.1824,0.1241,0.1351713,0.4000163,0.2008283,0.7909901,0,0.07548419,0.0589119,0.01121766,0.1656693,0,0.0117771,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0.1971663,0,0,0.02000673,0,0.2954761,0.1374624,0.1951299,0.2008283,0.1188099,0.1418395,0.4473137,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0.09702543,0.1133734,0.04287973,0.05001682,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0.032,0.1097328,0.3301111,0.2208001,0,0.005453273,0,0,0,0,0,0.4305102,0.1193151,0.08079781,0,0.05613042,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0.1276366,0.3601212,0.3664,0,0.02749248,0,0,0,0,0,0.2492427,0.4344292,0.676395,0.1006037,0.9020878,0,0,0,0,0.03340645,0,0,0.153928,0,0.03427451,0,0,0,
  0,0,0,0,0,0.02392604,0.04867304,0,0,0,0,0,0,0,0,0,0.01923198,0.02177701,0,0,0.01960942,0.008961361,0.08314439,0.04308448,0,0.09437622,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0.0464,0.05909523,0.05383944,0.03414774,0.06733654,0,0,0.1509684,0.06797528,0.2172146,0.04431918,0.5030184,0.02034226,0,0.06147739,0.0328972,0.02203885,0,0,0.00313569,0.5024855,0.000274618,0.1405256,0,0,0,
  0,0,0,0,0,0,0,0,0.02672455,0.1351713,0,0.06561421,0,0.07919377,0,0.02096442,0.01121766,0,0,0.001553198,0,0.1471426,0.1663658,0.01101942,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0.000327961,0,0,0.004904852,0.03310977,0,0.009938519,0,0.066255,0.008152554,0.01274636,0.0545647,0.000721625,0.1509055,0.002170789,0.01226363,0.1844322,0.1663658,0.01101942,0.01113548,0,0.005412052,0,0.002591398,0,0,0,0,
  0,0,0,0,0,0,0,0,0.000639306,0.02196813,0.004286418,0.02239663,0,0.2375813,0,0.005,0.00803651,0.00072132,0,0,0,0.1229548,0.1663658,0.03305828,0,0,0.005412052,0,0.002591398,0,0,0,0,
  0,0,0,0,0,0.001748159,0,0,0.02106318,0.1346288,0.022337,0.0363167,0,0.2375813,0.1048391,0.02096442,0.01121766,0.002212085,0.01676727,0,0.01226363,0.1844322,0.1109106,0.1091925,0.01113548,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0.001053159,0.004799868,0.002984548,0.001145771,0,0.009741648,0,0,0,0,0,0,0,0.05240696,0.04041656,0.01101942,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0.001404212,0.004421631,0.000735662,0.006267365,0,0.04154278,0,0,0,0,0,0,0.1941741,0.09574349,0.1109106,0.02203885,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.001258382,0,0,0,0.02361985,0,0.01720621,0.01696089,0.003024275,0.1207828,0,0,0.00313569,0,0.000274618,0.02157306,0,0,0,
  0,0,0,0,0,0,0.004059562,0,0,0,0,0,0,0,0,0.0747728,0.1080974,0.00738653,0.1676727,0.005938409,0.01226363,0,0.01127904,0.01101942,0,0,0,0,0,0.10,0,0,0,
  0,0,0,0,0,0,0.000358531,0,0,0,0,0,0,0,0,0,0,0,0.0075355,0,0.3638209,0.04829028,0.04365378,0.2704768,0.2338451,0.1801901,0.1359761,0.1550972,0.2288119,0.3118983,0,0,0,
  0,0,0,0,0,0,0.000454902,0,0,0,0,0,0,0,0,0,0,0,0.007557068,0,0.03679087,0.002015652,0.001879841,0.0560989,0.05567741,0.7006319,0.256553,0.08101361,0.06583849,0.2684838,0,0,0,
  0,0,0,0,0,0,9.18E-05,0,0,0,0,0,0,0,0,0,0,0,0.02131235,0,0.02591145,0.003626702,0.001691515,0.04413503,0.1781677,0.02480174,0.06566829,0.03142953,0.3652223,0.03182262,0,0,0,
  0,0,0,0,0,0,4.37E-05,0,0,0,0,0,0,0,0,0,0,0,0.001007609,0,0.01925174,0,0,0.001604244,0.02227096,0,0,0,0,0.02017987,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.000721882,0,0,0,0.031213,0,0,0,0,0.000353094,0,0,0,
  0,0,0,0,0,0,0,0,0,0.001607866,0,0.00181822,0,0.002898938,0,0,0.001819155,0,0,0,0.1216143,0.03728956,0.01221896,0.01101942,0.05567741,0,0.251599,0.01532696,0.1114401,0.02,0,0,0,
  0,0,0,0,0,0,0,0,0,0.000314047,0,0.000115925,0,0.000863633,0,0,0.001819155,0,0,0,0.04249413,0.006046957,0.0093992,0.004007064,0.08908386,0,0.02449874,0.0110239,0.04565654,0.01322618,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0.001225253,0,0.001656536,0,0,0.001819155,0,0,0,0.1216143,0.004031305,0.005639521,0.218385,0.07794838,0,0.2486094,0.04969525,0.1772986,0.02856211,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.03340645,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.01113548,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.03340645,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.01113548,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.02227096,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.03340645,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.0091007,0,0,0,
  0,0,0.5694807,0.4940658,0.130568,0.06195949,0.1000336,0.184,0.465539226,0.304559621,0.340362397,0.386167839,0.0902,0.18084539,0.21198369,0.0589119,0,0,0,0,0,0.02418782,0.03383712,0,0.05567741,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 32, 33, byrow = T))

DC.c.code[, V1  := NULL]
DC.c.code[, V31 := NULL]
DC.c.code[, V32 := NULL]
DC.c.code[, V33 := NULL]

GOM.params$diet <- cbind(GOM.params$diet[, Group], DC.c.code)

setnames(GOM.params$diet, c(paste('V', 1:30, sep = '')), c('Group', DC.groups))

check.rpath.params(GOM.params)
# save(GOM.params, file = file.path(data.dir, 'GOM_EMAX_params.R'))

#Run model
GOM <- rpath(GOM.params, 'Gulf of Maine')


#Groups to aggregate to compare with WSS
#Medium pelagics to Demersal- piscivores
#small pelagics- anadromous and small pelagics- other
#Small copepods and Large copepods as Mesozooplankton
#Rename Micronekton as Macrozooplankton
#merge larval fish to macrozooplankton
#Output for QPress
qpress.prey <- GOM.params$diet
pred.cols <- names(qpress.prey)[which(names(qpress.prey) != 'Group')]
# for(ipred in seq_along(pred.cols)){
#   setnames(qpress.prey, pred.cols[ipred], 'V1')
#   qpress.prey[V1 < .1, V1 := 0]
#   setnames(qpress.prey, 'V1', pred.cols[ipred])
# }
qpress.prey <- qpress.prey[Group != 'Import']


#Ensure that the matrices are square
detritus.DC <- data.table(Group = qpress.prey[, Group], Detritus = 0, Discards = 0)
qpress.prey <- merge(qpress.prey, detritus.DC, by = 'Group')
groupnames<-as.character(qpress.prey$Group)
qpress.prey<- qpress.prey[,-1]
setcolorder(qpress.prey,groupnames)

#Need to make write functions able to output to RData
x <- copy(GOM)
ngroup <- x$NUM_LIVING + x$NUM_DEAD
out <- data.frame(Group    = x$Group[1:ngroup],
                  type     = x$type [1:ngroup],
                  PB       = x$PB   [1:ngroup])
#Calculate M0
M0  <- c(x$PB[1:x$NUM_LIVING] * (1 - x$EE[1:x$NUM_LIVING]), 
         x$EE[(x$NUM_LIVING + 1):ngroup])
out <- cbind(out, M0)
#Calculate F mortality
totcatch <- x$Catch + x$Discards
Fmort    <- as.data.frame(totcatch / x$BB[row(as.matrix(totcatch))])
setnames(Fmort, paste('V',  1:x$NUM_GEARS,                     sep = ''), 
         paste('F.', x$Group[(ngroup +1):x$NUM_GROUPS], sep = ''))
out  <- cbind(out, Fmort[1:ngroup, ])
#Calculate M2
bio  <- x$BB[1:x$NUM_LIVING]
BQB  <- bio * x$QB[1:x$NUM_LIVING]
diet <- as.data.frame(x$DC)
nodetrdiet <- diet[1:x$NUM_LIVING, ]
detrdiet   <- diet[(x$NUM_LIVING +1):ngroup, ]
newcons    <- nodetrdiet * BQB[col(as.matrix(nodetrdiet))]
predM      <- newcons / bio[row(as.matrix(newcons))]
detcons    <- detrdiet * BQB[col(as.matrix(detrdiet))]
predM      <- rbind(predM, detcons)
setnames(predM, paste('V',  1:x$NUM_LIVING,    sep = ''), 
         paste('M2.', x$Group[1:x$NUM_LIVING], sep = ''))
qpress.pred <- as.data.table(cbind(out, predM))

M2.cols <- names(qpress.pred)[which(!names(qpress.pred) %in% c('Group', 'type', 
                                                               'PB', 'M0', 'Fmort[1:ngroup, ]'))]
qpress.pred[, mort.sum := rowSums(.SD), .SDcols = M2.cols]
qpress.pred <- qpress.pred[, .SD / mort.sum, .SDcols = M2.cols, by = Group]
qpress.pred[is.na(qpress.pred)] <- 0

# for(ipred in seq_along(M2.cols)){
#   setnames(qpress.pred, M2.cols[ipred], 'V1')
#   qpress.pred[V1 < .1, V1 := 0]
#   setnames(qpress.pred, 'V1', M2.cols[ipred])
# }

#make qpress.pred negative
qpress.pred[, c('type', 'PB', 'M0', 'Fmort[1:ngroup, ]') := NULL]
qpress.pred <- qpress.pred[, .SD * -1, .SDcol = M2.cols, by = Group]


#Ensure that the matrices are square, transpose pred matrix
detritus.m2 <- data.table(qpress.pred[, 'Group'], M2.Detritus = 0, M2.Discards = 0)
qpress.pred <- merge(qpress.pred, detritus.m2, by = 'Group')
qpress.pred<-qpress.pred[, .SD, .SDcols = c(M2.cols, 'M2.Detritus', 'M2.Discards')]
prednames<-as.character(detritus.DC$Group)
colnames(qpress.pred)<-prednames
setcolorder(qpress.pred,groupnames)
qpress.pred<-t(qpress.pred)
colnames(qpress.pred)<-groupnames

#Merge pred and prey matrices 

qpress.GOM<-qpress.pred+qpress.prey

# qpress.GOM <- t(qpress.pred[, .SD, .SDcols = c(M2.cols, 'M2.Detritus', 'M2.Discards')]) + 
#   qpress.prey[, .SD, .SDcols = c(pred.cols, 'Detritus', 'Discards')]




#add/change  row and column names

FG<-as.vector(groupnames)
qpress.GOM<-cbind(FG, qpress.GOM)

save(qpress.GOM, file="qpress.GOM.RData")


#remove values below 10%, 20%, 30%, 40%, 50%

GOMqnm10<-qpress.GOM
GOMqnm10[GOMqnm10 >=-0.10 & GOMqnm10<= 0.10]<-0

GOMqnm20<-qpress.GOM
GOMqnm20[GOMqnm20 >=-0.20 & GOMqnm20<= 0.20]<-0

GOMqnm30<-qpress.GOM
GOMqnm30[GOMqnm20 >=-0.30 & GOMqnm20<= 0.30]<-0

GOMqnm40<-qpress.GOM
GOMqnm40[GOMqnm40 >=-0.40 & GOMqnm40<= 0.40]<-0

GOMqnm50<-qpress.GOM
GOMqnm50[GOMqnm50 >=-0.50 & GOMqnm20<= 0.50]<-0

setwd("~/JCT work/WGNARS/ecosim to qpress/GOMmodel")
save(GOMqnm10, file='GOMqnm10.RData')
save(GOMqnm20, file='GOMqnm20.RData')
save(GOMqnm30, file='GOMqnm30.RData')
save(GOMqnm40, file='GOMqnm40.RData')
save(GOMqnm50, file='GOMqnm50.RData')


