#Gulf of Maine QNM results
#SML

#User parameters----------------------------------------------------------------
if(Sys.info()['sysname']=="Windows"){
  main.dir <- "C:/Users/Sean.Lucey/Desktop/QNM/QNMcompare"
}

if(Sys.info()['sysname']=="Linux"){
  main.dir  <- "/home/slucey/slucey/QNM/QNMcompare"
}

data.dir <- file.path(main.dir, 'data')
qpress.dir <- file.path(main.dir, 'Qpress_results')
out.dir <- file.path(main.dir, 'output')

#Required packages--------------------------------------------------------------
library(data.table)

#-------------------------------------------------------------------------------
#User created functions


#-------------------------------------------------------------------------------
#Load outputs
#Quantitative
load(file.path(data.dir, 'GOM_ecosense_PP_up_summary.RData'))
load(file.path(data.dir, 'GOM_ecosense_Piscivore_up_summary.RData'))
load(file.path(data.dir, 'GOM_ecosense_Piscivore_down_summary.RData'))

#Note pisc.up is wrong ... I perturbed whales instead of piscivores

#Add order column
pp.up[, Order := 1:28]
pisc.up[, Order := 1:28]
pisc.down[, Order := 1:28]

#Qualitative
pp.up.1 <- as.data.table(read.csv(file.path(qpress.dir, 'GOMresults1Phytoplankton_up.csv')))
pp.up.2 <- as.data.table(read.csv(file.path(qpress.dir, 'GOMresults2Phytoplankton_up.csv')))
pp.up.3 <- as.data.table(read.csv(file.path(qpress.dir, 'GOMresults3Phytoplankton_up.csv')))
pp.up.4 <- as.data.table(read.csv(file.path(qpress.dir, 'GOMresults4Phytoplankton_up.csv')))
pp.up.5 <- as.data.table(read.csv(file.path(qpress.dir, 'GOMresults5Phytoplankton_up.csv')))

pisc.up.1 <- as.data.table(read.csv(file.path(qpress.dir, 'GOMresults1D piscivores_up.csv')))
pisc.up.2 <- as.data.table(read.csv(file.path(qpress.dir, 'GOMresults2D piscivores_up.csv')))
pisc.up.3 <- as.data.table(read.csv(file.path(qpress.dir, 'GOMresults3D piscivores_up.csv')))
pisc.up.4 <- as.data.table(read.csv(file.path(qpress.dir, 'GOMresults4D piscivores_up.csv')))
pisc.up.5 <- as.data.table(read.csv(file.path(qpress.dir, 'GOMresults5D piscivores_up.csv')))

pisc.down.1 <- as.data.table(read.csv(file.path(qpress.dir, 'GOMresults1D piscivores_down.csv')))
pisc.down.2 <- as.data.table(read.csv(file.path(qpress.dir, 'GOMresults2D piscivores_down.csv')))
pisc.down.3 <- as.data.table(read.csv(file.path(qpress.dir, 'GOMresults3D piscivores_down.csv')))
pisc.down.4 <- as.data.table(read.csv(file.path(qpress.dir, 'GOMresults4D piscivores_down.csv')))
pisc.down.5 <- as.data.table(read.csv(file.path(qpress.dir, 'GOMresults5D piscivores_down.csv')))

#fix names
setnames(pp.up.1, c('X', 'X.', 'X..1'), c('Group', 'decrease', 'increase'))
setnames(pp.up.2, c('X', 'X.', 'X..1'), c('Group', 'decrease', 'increase'))
setnames(pp.up.3, c('X', 'X.', 'X..1'), c('Group', 'decrease', 'increase'))
setnames(pp.up.4, c('X', 'X.', 'X..1'), c('Group', 'decrease', 'increase'))
setnames(pp.up.5, c('X', 'X.', 'X..1'), c('Group', 'decrease', 'increase'))

setnames(pisc.up.1, c('X', 'X.', 'X..1'), c('Group', 'decrease', 'increase'))
setnames(pisc.up.2, c('X', 'X.', 'X..1'), c('Group', 'decrease', 'increase'))
setnames(pisc.up.3, c('X', 'X.', 'X..1'), c('Group', 'decrease', 'increase'))
setnames(pisc.up.4, c('X', 'X.', 'X..1'), c('Group', 'decrease', 'increase'))
setnames(pisc.up.5, c('X', 'X.', 'X..1'), c('Group', 'decrease', 'increase'))

setnames(pisc.down.1, c('X', 'X.', 'X..1'), c('Group', 'decrease', 'increase'))
setnames(pisc.down.2, c('X', 'X.', 'X..1'), c('Group', 'decrease', 'increase'))
setnames(pisc.down.3, c('X', 'X.', 'X..1'), c('Group', 'decrease', 'increase'))
setnames(pisc.down.4, c('X', 'X.', 'X..1'), c('Group', 'decrease', 'increase'))
setnames(pisc.down.5, c('X', 'X.', 'X..1'), c('Group', 'decrease', 'increase'))

#Calculate Bar size
pp.up[, Quant := (increase - decrease) / (increase + decrease)]
pp.up.1[, Link1 := (increase - decrease) / (increase + decrease + X0)]
pp.up.2[, Link2 := (increase - decrease) / (increase + decrease + X0)]
pp.up.3[, Link3 := (increase - decrease) / (increase + decrease + X0)]
pp.up.4[, Link4 := (increase - decrease) / (increase + decrease + X0)]
pp.up.5[, Link5 := (increase - decrease) / (increase + decrease + X0)]

pisc.up[, Quant := (increase - decrease) / (increase + decrease)]
pisc.up.1[, Link1 := (increase - decrease) / (increase + decrease + X0)]
pisc.up.2[, Link2 := (increase - decrease) / (increase + decrease + X0)]
pisc.up.3[, Link3 := (increase - decrease) / (increase + decrease + X0)]
pisc.up.4[, Link4 := (increase - decrease) / (increase + decrease + X0)]
pisc.up.5[, Link5 := (increase - decrease) / (increase + decrease + X0)]

pisc.down[, Quant := (increase - decrease) / (increase + decrease)]
pisc.down.1[, Link1 := (increase - decrease) / (increase + decrease + X0)]
pisc.down.2[, Link2 := (increase - decrease) / (increase + decrease + X0)]
pisc.down.3[, Link3 := (increase - decrease) / (increase + decrease + X0)]
pisc.down.4[, Link4 := (increase - decrease) / (increase + decrease + X0)]
pisc.down.5[, Link5 := (increase - decrease) / (increase + decrease + X0)]

#Merge files
pp.up.all <- merge(pp.up[, list(Order, Group, Quant)], pp.up.1[, list(Group, Link1)],
                   by = 'Group', all = T)
pp.up.all <- merge(pp.up.all, pp.up.2[, list(Group, Link2)], by = 'Group', all = T)
pp.up.all <- merge(pp.up.all, pp.up.3[, list(Group, Link3)], by = 'Group', all = T)
pp.up.all <- merge(pp.up.all, pp.up.4[, list(Group, Link4)], by = 'Group', all = T)
pp.up.all <- merge(pp.up.all, pp.up.5[, list(Group, Link5)], by = 'Group', all = T)
setkey(pp.up.all, Order)
pp.up.all[, Order := NULL]


pisc.up.all <- merge(pisc.up[, list(Order, Group, Quant)], pisc.up.1[, list(Group, Link1)],
                   by = 'Group', all = T)
pisc.up.all <- merge(pisc.up.all, pisc.up.2[, list(Group, Link2)], by = 'Group', 
                     all = T)
pisc.up.all <- merge(pisc.up.all, pisc.up.3[, list(Group, Link3)], by = 'Group', 
                     all = T)
pisc.up.all <- merge(pisc.up.all, pisc.up.4[, list(Group, Link4)], by = 'Group', 
                     all = T)
pisc.up.all <- merge(pisc.up.all, pisc.up.5[, list(Group, Link5)], by = 'Group', 
                     all = T)
setkey(pisc.up.all, Order)
pisc.up.all[, Order := NULL]

pisc.down.all <- merge(pisc.down[, list(Order, Group, Quant)], pisc.down.1[, list(Group, Link1)],
                   by = 'Group', all = T)
pisc.down.all <- merge(pisc.down.all, pisc.down.2[, list(Group, Link2)], by = 'Group', 
                       all = T)
pisc.down.all <- merge(pisc.down.all, pisc.down.3[, list(Group, Link3)], by = 'Group', 
                       all = T)
pisc.down.all <- merge(pisc.down.all, pisc.down.4[, list(Group, Link4)], by = 'Group', 
                       all = T)
pisc.down.all <- merge(pisc.down.all, pisc.down.5[, list(Group, Link5)], by = 'Group', 
                       all = T)
setkey(pisc.down.all, Order)
pisc.down.all[, Order := NULL]

#Fix labels for plotting
pp.up.all[Group == 'Small crab other arthropoda', Group := 'Other arthropoda']
pp.up.all[Group == 'Other pelagic mesopelagic',   Group := 'Other pelagic']

pisc.up.all[Group == 'Small crab other arthropoda', Group := 'Other arthropoda']
pisc.up.all[Group == 'Other pelagic mesopelagic',   Group := 'Other pelagic']

pisc.down.all[Group == 'Small crab other arthropoda', Group := 'Other arthropoda']
pisc.down.all[Group == 'Other pelagic mesopelagic',   Group := 'Other pelagic']

#Plot
png(filename = file.path(out.dir, 'GOM_Primary_up.png'), height = 1800, width = 2500,
    res = 200)
opar <- par(mar = c(10, 4, 2, 2))
barplot(as.matrix(t(pp.up.all[, list(Quant, Link1, Link2, Link3, Link4, Link5)])), 
        beside = T, col = c('black', '#ffffd4','#fed98e','#fe9929','#d95f0e','#993404'))
axis(1, at = seq(4, 193, by = 7), labels = pp.up.all[, Group], las = 3, cex.axis = 1.2)
mtext(2, text = 'Proportion', line = 2.5, cex = 2)
dev.off()

png(filename = file.path(out.dir, 'GOM_Piscivore_up.png'), height = 1800, width = 2500,
    res = 200)
opar <- par(mar = c(10, 4, 2, 2))
barplot(as.matrix(t(pisc.up.all[, list(Quant, Link1, Link2, Link3, Link4, Link5)])), 
        beside = T, col = c('black', '#ffffd4','#fed98e','#fe9929','#d95f0e','#993404'))
axis(1, at = seq(4, 193, by = 7), labels = pp.up.all[, Group], las = 3, cex.axis = 1.2)
mtext(2, text = 'Proportion', line = 2.5, cex = 2)
dev.off()

png(filename = file.path(out.dir, 'GOM_Piscivore_down.png'), height = 1800, width = 2500,
    res = 200)
opar <- par(mar = c(10, 4, 2, 2))
barplot(as.matrix(t(pisc.down.all[, list(Quant, Link1, Link2, Link3, Link4, Link5)])), 
        beside = T, col = c('black', '#ffffd4','#fed98e','#fe9929','#d95f0e','#993404'))
axis(1, at = seq(4, 193, by = 7), labels = pp.up.all[, Group], las = 3, cex.axis = 1.2)
mtext(2, text = 'Proportion', line = 2.5, cex = 2)
dev.off()


