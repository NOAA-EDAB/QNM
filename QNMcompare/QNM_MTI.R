#Mixed trophic impacts work
#Latest version on GitHub NOAA-EDAB/QNM/QNMcompare

#User Parameters
if(Sys.info()['sysname']=="Windows"){
  data.dir <- "C:\\Users\\Sean.Lucey\\Desktop\\Rpath_code\\data"
  out.dir  <- "C:\\Users\\Sean.Lucey\\Desktop\\Rpath_code\\outputs"
}

if(Sys.info()['sysname']=="Linux"){
  data.dir <- "/home/slucey/slucey/Rpath_code/data"
  out.dir  <- "/home/slucey/slucey/Rpath_code/outputs"
}

library(Rpath); library(data.table)

#Load GOM model
load(file.path(data.dir, 'EMAX_GOM_28groups_renamed.RData'))

#Run Ecopath
GOM <- rpath(GOM.28b, "Gulf of Maine")

#Run MTI
gom.mti <- MTI(GOM, GOM.28b)

#Plot results
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

