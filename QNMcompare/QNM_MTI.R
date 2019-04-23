#Mixed trophic impacts work
#Latest version on GitHub NOAA-EDAB/QNM/QNMcompare

#User Parameters
if(Sys.info()['sysname']=="Windows"){
  qnm.dir    <- "C:\\Users\\Sean.Lucey\\Desktop\\QNM\\QNMcompare"
  qpress.dir <- file.path(qnm.dir, "Qpress_results")
  out.dir    <- file.path(qnm.dir, "Outputs")
}

if(Sys.info()['sysname']=="Linux"){
  data.dir <- "/home/slucey/slucey/Rpath_code/data"
  out.dir  <- "/home/slucey/slucey/Rpath_code/outputs"
}

library(Rpath); library(data.table)

#Load GOM model
load(file.path(qnm.dir, 'EMAX_GOM_28groups_renamed.RData'))

#Run Ecopath
GOM <- rpath(GOM.28b, "Gulf of Maine")

#Run MTI
gom.mti <- MTI(GOM, GOM.28b)

#Plot MTI results
n.pred <- ncol(gom.mti)
labs <- GOM.28b$model$Group

opar <- par()

for(igroup in 1:n.pred){
  if(igroup %in% seq(1, n.pred, 4)){
    png(file = file.path(out.dir, paste0('GOM_MTI_Groups_', igroup, '_',
                                             igroup + 3, '.png')),
                             height = 1500, width = 2000, res = 200)
    par(mfcol = c(4, 1), mar = c(0,0,0,0), oma = c(14, 14, 0, 0))
  }
  bars <- barplot(gom.mti[, igroup], ylim = c(-1, 1), axes = F)
  mtext(2, text = labs[igroup], las = T, line = -2, cex = 1)
  #Add pos/neg colors
  pos.value <- gom.mti[, igroup]
  neg.value <- gom.mti[, igroup]
  pos.value[which(pos.value < 0)] <- 0
  neg.value[which(neg.value > 0)] <- 0
  barplot(pos.value, ylim = c(-1, 1), add = T, col = 'blue', axes = F)
  barplot(neg.value, ylim = c(-1, 1), add = T, col = 'red', axes = F)
  if(igroup %in% seq(4, n.pred, 4)){
    text(cex = 1.5, x = bars - .25, y = -0.7, labs, xpd = NA, srt = 90, adj = 1)
    dev.off()
  } 
}

#Plot comparison plots
#Gulf of Maine
#Scenario 1 - Phytoplankton up
out.order <- GOM.28b$model$Group
gom.phyto.up <- data.table(Group = out.order, mti = gom.mti[, 1]) 

#Load QPress results
# out.order <- GOM.28b$model$Group
# for(iscene in 1:5){
#   qpress.results <- as.data.table(read.csv(file.path(qpress.dir, paste0(
#     'GOMresults', iscene, 'Phytoplankton_up.csv'))))
#   setnames(qpress.results, c('X', 'X.', 'X0', 'X..1'), c('Group', 'Neg', 'Neu', 'Pos'))
#   
#   #Convert to prop
#   qpress.results[Pos > Neg, Value := Pos / 1000]
#   qpress.results[Neg > Pos, Value := -1 * (Neg / 1000)]
#   qpress.results[, out.order := factor(Group, levels = out.order)]
#   setorder(qpress.results, out.order)
#   
#   scene.results <- qpress.results[, list(Value)]
#   setnames(scene.results, 'Value', paste0('Model', iscene))
#   
#   gom.phyto.up <- cbind(gom.phyto.up, scene.results)
# }

for(iscene in 1:5){
  qpress.results <- as.data.table(read.csv(file.path(qpress.dir, paste0(
    'GOMresults', iscene, 'Phytoplankton_up.csv'))))
  setnames(qpress.results, c('X', 'X.', 'X0', 'X..1'), c('Group', 'Neg', 'Neu', 'Pos'))
  
  #Convert to prop
  qpress.results[, Value := (Pos - Neg) / 1000]
  
  scene.results <- qpress.results[, list(Group, Value)]
  setnames(scene.results, 'Value', paste0('Model', iscene))
  
  gom.phyto.up <- merge(gom.phyto.up, scene.results, by = 'Group', 
                        all.x = T)
}

#Fix NAs
gom.phyto.up[is.na(gom.phyto.up)] <- 0

#Set order
gom.phyto.up[, out.order := factor(Group, levels = out.order)]
setorder(gom.phyto.up, out.order)

#Drop group column
gom.phyto.up[, c('Group', 'out.order') := NULL]

png(file = file.path(out.dir, 'GOM_Phyto_up_2.png'), 
    height = 1500, width = 2000, res = 200) 
par(mar = c(10, 6, 4, 2))
bars <- barplot(as.matrix(t(gom.phyto.up)), beside = T, 
                col = c('#7fc97f','#beaed4','#fdc086','#ffff99',
                        '#386cb0','#f0027f'))
text(cex = .8, x = colMeans(bars) - .25, y = -1.1, labs, 
     xpd = NA, srt = 90, adj = 1)
abline(v = bars[6, ] + 1, lty = 2, col = 'lightgrey')
legend(97, 1.2, legend = c('MTI', '10%', '20%', '30%', '40%', '50%'),
       fill = c('#7fc97f','#beaed4','#fdc086','#ffff99', '#386cb0',
               '#f0027f'), xpd = NA, xjust = .5, horiz = T)

dev.off()

