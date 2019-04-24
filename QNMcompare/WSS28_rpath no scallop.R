#Western Scotian Shelf Model (No stanza groups)
#SML

#User parameters


#-------------------------------------------------------------------------------
#Required packages
library(data.table); library(Rpath)
setwd("~/JCT work/WGNARS/ecosim to qpress/WSS28model")
#-------------------------------------------------------------------------------
#User created functions

#-------------------------------------------------------------------------------
#Western Scotian Shelf Groups - collapsed list (no multistanza groups)
groups <- c('Whales', 'Toothed cetaceans', 'Seals', 'Sea birds', 'Sharks', 
            'Large pelagics',  
            'D piscivores', 'L benthivores', 'Skates', 'Small pelagics', 'Other pelagic mesopelagic',
            'Squids', 'Megabenthos', 'Small crab other arthropoda', 'Shrimps', 
            'Bivalves', 'Other molluscs', 'Worms', 'Meiofauna','Gelatinous zoop', 
            'Macrozoop', 'Mesozoop', 'Microzoop', 'Microflora', 'Phytoplankton', 
            'Discards', 'Detritus', 'Fishery')

#Identify type (0 = consumer, 1 = producer, 2 = detritus, 3 = fleet)
types <- c(rep(0, 24), 1, 2, 2, 3)

#Create parameter list object
WSS28.params <- create.rpath.params(groups, types)

#Fill in model parameters
#Biomass, Production, consumption
biomass <- c(0.407075, 0.049754, 0.04383, 0.00617, 0.0260249,0.023573, 4.082286,  4.240253, 
             0.137757, 6.294171,  0.878285, 0.173893,
             0.458598,
             1.818481,
             1.302563,
             
             64.01978,
             2,
             7.345485,
             42.75382,
             0.5204,
             41.31,
             23.79555,
             5.800508,
             3.678861,
             33.664,
             0.063293,
             1,NA)

pb <- c(0.071,0.18,0.147007,0.25,0.18,0.4,0.5341109,0.3798308,0.2320817,0.6959251,0.7432774,4,
        0.8221899,2.759435,3,0.69,0.75,1.25,1.32583,15.51,3.04,29.2, 
        82.8, 104.938, 70.639, rep(NA, 3))

qb <- c(4.94,14.5,7.338632,87.6,4.78,4.24,2.868397,3.254083,2.447539,3.475426,3.532147,11.33333,
        5.481265,18.39623 ,rep(NA, 4), 8.83887,62.05, 19.5, 73, rep(NA, 6))

WSS28.params$model[, Biomass := biomass]
WSS28.params$model[, PB      := pb]
WSS28.params$model[, QB      := qb]

#Production to Consumption
WSS28.params$model[Group %in% c('megabenthos',  'Shrimps',
                                'Bivalves', 'Other molluscs', 'Worms' ),
                   ProdCons := 0.15]
#WSS28.params$model[Group == 'Mesozoop', ProdCons := 0.4]
WSS28.params$model[Group %in% c('Microzoop', 'Microflora'), ProdCons := 0.5]

#Biomass accumulation and unassimilated production
ba <- c(0.0032566,0,0.003702101,0,-0.001951868,0,-0.05736,0.044246, rep(0, 4), 0.0301295, 0,0,
        0.03234285,rep(0,11), NA)
# ba<-c(rep(0,28), NA)
WSS28.params$model[, BioAcc  := ba]
WSS28.params$model[, Unassim := c(rep(0.2, 20), rep(0.4, 3), 0.2, rep(0, 3), NA)]

#Detrital fate
WSS28.params$model[, Detritus := c(rep(1, 25), rep(0, 3))]
WSS28.params$model[, Discards := c(rep(0, 27), 1)]

#Landings/Discards
land <- c(rep(0, 4),0.00209077,
          0.005067847,
          0.3251713,
          0.182276,
          0.000799,
          0.999061,
          0.019708,
          0.001637,
          0.220424,0,
          0.000162,
        
          0.117151,
          0.00236,0,
          0.03026,rep(0,8),NA)
disc <- c(0,0.002995,0,2.4E-05,
          0.000575,
          0.000469,
          0.013477,
          0.002266,
          0.013094,
          0.000487,
          0.00056,
          1.9E-05,
          0.008776,
          0.000633,
          8E-06 ,0,4.1E-05,0,0.019869, rep(0, 8), NA)
WSS28.params$model[, Fishery      := land]
WSS28.params$model[, Fishery.disc := disc]

#Diet composition
DC.groups <- groups[which(!groups %in% c('Discards', 'Detritus', 'Fishery'))]
# DC.data <- as.data.table(matrix(c(0,	0,	0,	0	,0,	0	,0,	0,	0,	0,	0,0	,0	,0	,0	,0	,0	,0	,0,	0,0,0,0,0,0,
#                                   0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0	,0,	0	,0,	0	,0,	0	,0,	0	,0,	0	,0,	0	,0,	0,
#                                   0,	0	,0,	0	,0.006511993,	0	,0,	0	,0,	0	,0,	0,	0,	0	,0,	0	,0,	0,	0,	0	,0,	0,	0,	0,	0,
#                                   0,	0,	0,	0.002663992,	0,	0,	0,	0,	0	,0,	0	,0,	0	,0,	0	,0,	0	,0	,0,	0	,0,	0	,0,	0,	0,
#                                   0	,0,	0	,0,	0	,0,	0	,0,	0,	0,	0,	0,	0,	0,	0,	0,	0	,0,	0	,0,	0	,0,	0	,0,	0,
#                                   0,	0,	0,	0	,0,	0.002219998	,0,	0,	0,	0,	0,	0	,0,	0,	0,	0	,0,	0	,0,	0	,0,	0,	0,	0,	0,
#                                   0.004802,	0.2437588,	0.1276689,	0.0007209978,	0.1848318,	0.06917194,	0.05424953,	0.005541045	,0.003080484,	0	,0,	0.003657,	0,	0,	0,	0	,0	,0,	0,	0,0,	0,	0,	0	,0,
#                                   0.012545,	0.09793591,	0.2855617,	0.005076986,	0.3280216,	0.1511799,	0.04674805,	0.004132936,	0.05460171,	0,	0,	0.007678,	0.001092015,	0,	0,	0	,0,	0	,0	,0,	0,	0	,0,	0,	0,
#                                   0,	0	,0.006431994,	0,	0.0008079991,	0.002838997,	0.0004965155,	0	,0,	0	,0,	0.000187,	0,	0	,0,	0,	0,	0	,0,	0,	0,	0,	0,	0,	0,
#                                   0.173918,	0.3776346	,0.5207465,	0.3529089,	0.4223085,	0.6149244,	0.1726697,	0.00143368,	0.02304704,	0,	0,	0.015327,	0,	0,	0,	0,	0,	0,	0,	0	,0	,0	,0,	0,	0,
#                                   0.0194,	0.007396993,	0.05680194,	0.07901775,	0.04387695,	0.1276559,	0.02289399,	0.01478033,	0.03303853,	0,	0,	0.004734,	0	,0,	0,	0	,0	,0	,0,	0,	0	,0,	0	,0,	0,
#                                   0.011769,	0.2679567,	0.002056998,	0.03595289,	0.01354798,	0.01555398,	0.02838177,	0.001817405	,0.006008342,	0,	0,	0.037929,	0.002280137,	0,	0,	0,	0,	0	,0,	0,	0,	0	,0,	0,	0,
#                                   0,	0	,2.099998E-05,	0,	9.29999E-05,	7.299994E-05,	0.003771638,	0.001804974,	0.00659312,	0,	0,	0,	0.007052792,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                                   0,	0,	0,	0,	0,	0,	0.03455577,	0.07374978,	0.1577646,	0.02415398,	0.060237,	0.014737,	0.09158555,	0.009690308,	0.004533,	0,	0	,0.03769,	0.02,	0.0001840747,	0,	0,	0,	0,	0,
#                                   0,	0,	0.0007109993	,0,	0,	0.003309997,	0.07398107,	0.08974432,	0.07314944,	0.0622156,	0.03105934,	0.010014,	0.001267644,	0	,0	,0	,0	,0.020851,	0,	0,	0,	0,	0,	0,	0,
#                                   0,	0	,0	,0	,0	,0	,0.0008845589,	0.00388914,	0,	0,	0,	0,	0.004711973,	0,	0,	0,	0,	0,	0	,0	,0	,0	,0	,0	,0,
#                                   0,	0,	0,	0,	0,	0	,0.0006980912,	0.02864456,	0.0008348122,	0	,0,	0	,0.3265275	,0.01410418	,0	,0	,0,	0.332281,	0,	0.001184787,	0,	0	,0	,0	,0,
#                                   0,	0,	0,	0,	0,	0,	0.001044587,	0.005987847,	0.002030869	,0,	0	,0	,0.0419751,	0.01410418,	0	,0	,0	,0.027689	,0,	0.0006171872,	0	,0,	0,	0,	0,
#                                   0,	0,	0	,0	,0,	0,	0.002290674,	0.09854721,	0.1947723,	0.001041604,	0.0008129025,	0.013273,	0.1380028,	0.02820811,	0.015272,	0	,0	,0,	0.070001,	0.0006171872,	0,	0,	0,	0,	0,
#                                   0,	0,	0,	0,	0,	0.01216899,	0.004603415,	0.09481322,	0.001261869,	0.003441011,	0,	0,	0.281883,	0.08955499,	0	,0	,0,0.304589,	0.070001,	0.03959263,	0,	0,	0,	0,	0,
#                                   0,	0,	0,	0,	0,	5.899995E-05,	0.08581191,	0.008719152,	0.001671519,	0.0001719213,	0.001211964,	0.001571,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
#                                   0.726929,	0,	0,	0.315373,	0,	0,	0.4453562,	0.5160776,	0.3443648,	0.8572081,	0.3833565,	0.890893,	0,	0	,0.123605,	0	,0	,0	,0	,0.005774691,	0.161448,	0.055941,	0,	0	,0,
#                                   0.050637,	0	,0,	0,	0,	0,	0.01242108,	0.0372947,	0.09778056,	0.05176783,	0.5141716,	0,	0,	0,	0.244326,	0	,0	,0	,0	,0.005284418	,0.696038	,0.47728,	0.058572	,0,	0,
#                                   0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0.00386473,	0.095929,	0.11221,	0.161911,	0.099731,	0,
#                                   0,	0,	0,	0,	0,	0,	0,	0	,0	,0	,0	,0	,0	,0.006854988,	0.008578,	0.04999995,	0.095,	0,	0,	0.0102941,	0.005032,	0.015172,	0.076645,	0.202492,	0,
#                                   0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0.002426699,	0,	0,	0,	0.069225,	0.4222216,	0.882656,	0	,0,	0.08967555,	0.041553,	0.127274,	0.702872,	0.209335,	0,
#                                   0	,0.005316995,	0,	0.0005529983,	0,	0,	0,	0,	0,	0,	0,	0,	0.002525342,	0.001344179,	0,	0,	0,	0,	0,	0	,0	,0	,0,	0,	0,
#                                   0	,0	,0,	0	,0,	0,	0,	0,	0,	0	,0,	0,	0.06347585,	0.8361391,	0.534461,	0.5277785,	0.022344,	0.2769,	0.839998,	0.8429105,	0,	0.212123,	0,	0.488442,	1,
#                                   0,	0,	0,	0.2077324,	0,	0.000843999,	0.009141359,	0.01302205,	0,	0,	0.006723996,	0,	0.03762027,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0),
#   29,25, byrow = T))



#----------------------------
# code to copy paste data into Rpath, can do this or copy paste to CSV and import.
# read.excel <- function(header=TRUE,...) {
#   read.table("clipboard",sep="\t",header=F,...)
# }
# 
# dcdat<-read.excel()
# DC.data<-as.data.table(dcdat)
# save(DC.data, file='DC.data.noscallop.RData')
load('DC.data.noscallop.RData')
#-------------------------------------------

#Fix column names
# setnames(WSS28.params$diet, 1, 'Group')
# setnames(WSS28.params$diet, paste0('V', 1:25), DC.groups)
# setnames(WSS28.params$diet, c(paste('V', 1:27, sep = '')), c('Group', DC.groups)

#Merge DC matrix with diet parameter object
WSS28.params$diet <- cbind(WSS28.params$diet[, Group], DC.data)

#Fix column names
setnames(WSS28.params$diet, 1, 'Group')
# setnames(WSS28.params$diet, paste0('V', 1:25), DC.groups)


#Ecopath
WSS28 <- rpath(WSS28.params, 'Western Scotian Shelf')
save(WSS28.params, file = "WSS28.RData")

#QPress-------------------------------------------------------------------------

#Prey matrix
qpress.prey <- WSS28.params$diet
pred.cols <- names(qpress.prey)[which(names(qpress.prey) != 'Group')]
#This code for thresholds
# for(ipred in seq_along(pred.cols)){
#   setnames(qpress.prey, pred.cols[ipred], 'V1')
#   qpress.prey[V1 < .1, V1 := 0]
#   setnames(qpress.prey, 'V1', pred.cols[ipred])
# }
qpress.prey <- qpress.prey[Group != 'Import']

#Ensure that the matrices are square
detritus.DC <- data.table(Group = qpress.prey[, Group], Detritus = 0, Discards = 0)
qpress.prey <- merge(qpress.prey, detritus.DC, by = 'Group')

#line columns and rows up
groupnames <- as.character(qpress.prey$Group)
groupnames<-c('Group', groupnames)
setcolorder(qpress.prey, groupnames)

#Predator M2/F matrix
x <- copy(WSS28)
ngroup <- x$NUM_LIVING + x$NUM_DEAD
out <- data.frame(Group    = x$Group[1:ngroup],
                  type     = x$type [1:ngroup],
                  PB       = x$PB   [1:ngroup])
#Calculate F mortality
totcatch <- x$Catch + x$Discards
Fmort    <- as.data.frame(totcatch / x$BB[row(as.matrix(totcatch))])
setnames(Fmort, paste('V',  1:x$NUM_GEARS,                     sep = ''), 
         paste('F.', x$Group[(ngroup +1):x$NUM_GROUPS], sep = ''))
out  <- cbind(out, Fmort[1:ngroup, ])
setnames(out, 'Fmort[1:ngroup, ]', 'Fishery')

#Add Fishery to prey
fishery <- as.data.table(out)
fishery <- fishery[, list(Group, Fishery)]
qpress.prey <- merge(qpress.prey, fishery, by = 'Group') 
qpress.prey[, Group := NULL]
fishery.0 <- as.data.table(matrix(rep(0, nrow(fishery) + 1), 1, nrow(fishery) + 1))
qpress.prey <- rbindlist(list(qpress.prey, fishery.0))

# #Predator M2/F matrix
# x <- copy(WSS28.b)
# ngroup <- x$NUM_LIVING + x$NUM_DEAD
# out <- data.frame(Group    = x$Group[1:ngroup],
#                   type     = x$type [1:ngroup],
#                   PB       = x$PB   [1:ngroup])

#Calculate M0
M0  <- c(x$PB[1:x$NUM_LIVING] * (1 - x$EE[1:x$NUM_LIVING]), 
         x$EE[(x$NUM_LIVING + 1):ngroup])
out <- cbind(out, M0)



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
                                                               'PB', 'M0'))]
qpress.pred[, mort.sum := rowSums(.SD), .SDcols = M2.cols]
qpress.pred <- qpress.pred[, .SD / mort.sum, .SDcols = M2.cols, by = Group]
qpress.pred[is.na(qpress.pred)] <- 0

#Code for thresholds
# for(ipred in seq_along(M2.cols)){
#   setnames(qpress.pred, M2.cols[ipred], 'V1')
#   qpress.pred[V1 < .1, V1 := 0]
#   setnames(qpress.pred, 'V1', M2.cols[ipred])
# }

#make qpress.pred negative
qpress.pred <- qpress.pred[, .SD * -1, .SDcol = M2.cols, by = Group]

#Ensure that the matrices are square
detritus.m2 <- data.table(qpress.pred[, 'Group'], M2.Detritus = 0, M2.Discards = 0)
qpress.pred <- merge(qpress.pred, detritus.m2, by = 'Group')
qpress.pred <- qpress.pred[, .SD, .SDcols = c(M2.cols, 'M2.Detritus', 'M2.Discards')]
prednames <- as.character(c('Fishery', detritus.DC$Group))
colnames(qpress.pred) <- prednames
new.names<-colnames(qpress.prey)
setcolorder(qpress.pred, new.names)
qpress.pred <- t(qpress.pred)
qpress.pred <- cbind(qpress.pred, rep(0, nrow(qpress.pred)))
colnames(qpress.pred) <- c(new.names)


#Merge pred and prey matrices 
qpress.WSS28 <- qpress.pred + qpress.prey

#add/change  row and column names

FG<-as.vector(new.names)
qpress.WSS28<-cbind(FG, qpress.WSS28)

save(qpress.WSS28, file="qpress.WSS28.RData")


#remove values below 10%, 20%, 30%, 40%, 50%

WSS28qnm10<-qpress.WSS28
WSS28qnm10[WSS28qnm10 >=-0.10 & WSS28qnm10<= 0.10]<-0

WSS28qnm20<-qpress.WSS28
WSS28qnm20[WSS28qnm20 >=-0.20 & WSS28qnm20<= 0.20]<-0

WSS28qnm30<-qpress.WSS28
WSS28qnm30[WSS28qnm20 >=-0.30 & WSS28qnm20<= 0.30]<-0

WSS28qnm40<-qpress.WSS28
WSS28qnm40[WSS28qnm40 >=-0.40 & WSS28qnm40<= 0.40]<-0

WSS28qnm50<-qpress.WSS28
WSS28qnm50[WSS28qnm50 >=-0.50 & WSS28qnm20<= 0.50]<-0

setwd("~/JCT work/WGNARS/ecosim to qpress/WSS28model")
save(WSS28qnm10, file='WSS28qnm10.RData')
save(WSS28qnm20, file='WSS28qnm20.RData')
save(WSS28qnm30, file='WSS28qnm30.RData')
save(WSS28qnm40, file='WSS28qnm40.RData')
save(WSS28qnm50, file='WSS28qnm50.RData')



