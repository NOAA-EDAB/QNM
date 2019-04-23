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
gom.mti.down <- MTI(GOM, GOM.28b, increase = F)

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
scenarios <- c('Phytoplankton_up',
               'Small crab other arthropoda_down',
               'Mesozoop_up',
               'Macrozoop_down',
               'Megabenthos_up',
               'Other pelagic mesopelagic_up',
               'Small pelagics_up',
               'D piscivores_up',
               'D piscivores_down',
               'Seals_up',
               'Fishery_up',
               'Fishery_down')
decrease <- c(F, T, F, T, rep(F, 4), T, F, F, T)
focal.group <- c(1, 8, 4, 6, 12, 16, 14, 17, 17, 22, 28, 28)

#Gulf of Maine
out.order <- GOM.28b$model$Group
for(iscene in 1:12){
  if(decrease[iscene]){
    scene.out <- data.table(Group = out.order, 
                            mti = gom.mti.down[, focal.group[iscene]])
  } else {
    scene.out <- data.table(Group = out.order, mti = gom.mti[, focal.group[iscene]])
  }
  
  #Load QPress results
  for(imod in 1:5){
    mod.name <- paste0('GOMresults', imod, scenarios[iscene], '.csv')
    qpress.results <- as.data.table(read.csv(file.path(qpress.dir, mod.name)))
    setnames(qpress.results, c('X', 'X.', 'X0', 'X..1'), c('Group', 'Neg', 'Neu', 'Pos'))
    
    #Convert to prop
    qpress.results[, Value := (Pos - Neg) / 1000]
    
    mod.results <- qpress.results[, list(Group, Value)]
    setnames(mod.results, 'Value', paste0('Model', imod))
    
    scene.out <- merge(scene.out, mod.results, by = 'Group', all.x = T)
  }
  
  #Fix NAs
  scene.out[is.na(scene.out)] <- 0
  
  #Set order
  scene.out[, out.order := factor(Group, levels = out.order)]
  setorder(scene.out, out.order)
  
  #Drop group column
  scene.out[, c('Group', 'out.order') := NULL]
  
  #Plot output
  png(file = file.path(out.dir, paste0('GOM_', scenarios[iscene], '.png')), 
      height = 1500, width = 2000, res = 200) 
  par(mar = c(10, 6, 4, 2))
  bars <- barplot(as.matrix(t(scene.out)), beside = T, 
                  col = c('#7fc97f','#beaed4','#fdc086','#ffff99',
                          '#386cb0','#f0027f'))
  text(cex = .8, x = colMeans(bars) - .25, y = -1.1, labs, 
       xpd = NA, srt = 90, adj = 1)
  abline(v = bars[6, ] + 1, lty = 2, col = 'lightgrey')
  legend(97, 1.2, legend = c('MTI', '10%', '20%', '30%', '40%', '50%'),
         fill = c('#7fc97f','#beaed4','#fdc086','#ffff99', '#386cb0',
                  '#f0027f'), xpd = NA, xjust = .5, horiz = T)
  
  dev.off()
}

    







