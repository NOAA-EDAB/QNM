#Gulf of Maine Rpath Ecosense
#SML

#User parameters----------------------------------------------------------------
if(Sys.info()['sysname']=="Windows"){
  main.dir <- "C:/Users/Sean.Lucey/Desktop/QNM/QNMcompare"
}

if(Sys.info()['sysname']=="Linux"){
  main.dir  <- "/home/slucey/slucey/QNM/QNMcompare"
}

data.dir <- file.path(main.dir, 'data')

#Required packages--------------------------------------------------------------
library(data.table); library(Rpath)

#-------------------------------------------------------------------------------
#User created functions
source(file.path(main.dir, 'rsim_sense_master_Jun2019.r'))
       
#-------------------------------------------------------------------------------
#Load and balance model
load(file.path(main.dir, 'EMAX_GOM_28groups_renamed.RData'))

#load current biomass/landings
#load(file.path(data.dir, 'GB_biomass_current.RData'))
#load(file.path(data.dir, 'GB_landings_current.RData'))

#Run Rpath
GOM <- rpath(GOM.28b, 'Gulf of Maine')

#Add pedigree - for merged model drop small copepods, small pelagics - anadromous,
#Medium pelagics, and larval fish
#Add pedigree - converted from EMAX documentation
#7 = 0.1 ... 0 = 0.8
GOM.28b$pedigree[, B := c(0.3, 0.6, 0.6, 0.4, 0.5, 0.5, 0.4, 0.4, 0.4, 0.4, 0.4, 
                          0.5, 0.3, 0.3, 0.4, 0.4, 0.3, 0.3, 0.3, 0.5, 0.5, 0.4, 
                          0.4, 0.4, 0.4, 0.6, 0.6, 1)]
GOM.28b$pedigree[, PB := c(0.3,	0.6, 0.6,	0.5, 0.6, 0.6,	0.5, 0.5,	0.5, 0.5, 0.5,	
                           0.5, 0.5,	0.1,	0.3, 0.3,	0.3, 0.3, 0.3,	0.4, 0.2,	
                           0.5, 0.5,	0.5, 0.5, 0.7, 0.7, 1)]
GOM.28b$pedigree[, QB := c(1,	  0.6, 0.6, 0.5, 0.6, 0.6, 0.5, 0.5, 0.5, 0.5, 0.5,	
                           0.5, 0.5, 0.1, 0.3, 0.3, 0.1, 0.1, 0.1,	0.4, 0.4, 0.5, 
                           0.5, 0.5, 0.5, 0.7, 0.7, 1)]
GOM.28b$pedigree[, Diet := c(1,	  0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6,	
                             0.6, 0.6, 0.3, 0.4, 0.4, 0.3, 0.3, 0.3, 0.4, 0.4, 0.5, 
                             0.5, 0.5, 0.4, 0.7, 0.7, 1)]
GOM.28b$pedigree[, Fishery := c(1, 1, 1, 1, 0.8, 1, 0.8, 0.8, 0.8, 0.8, 0.4, 0.4, 
                                0.3, 0.3, 0.5, 0.3, 0.3, 0.3, 0.3, 0.5, 0.5, 0.5, 
                                0.5, 1, 1, 0.7, 0.7, 0.5)]

#Set up sense runs
all_years <- 2014:2063
scene <- rsim.scenario(GOM, GOM.28b, years = all_years)

# ----- Set up ecosense generator ----- ########################################
# load rsim_sense_master.r
scene$params$BURN_YEARS <- 50
NUM_RUNS <- 10000
parlist <- as.list(rep(NA, NUM_RUNS))
kept <- rep(NA, NUM_RUNS)

set.seed(123)
for (irun in 1:NUM_RUNS){
  GOMsense <- copy(scene) 
  # INSERT SENSE ROUTINE BELOW
  parlist[[irun]] <- GOMsense$params 		# Base ecosim params
  parlist[[irun]] <- rsim.sense.orig(GOMsense, GOM, GOM.28b)	# Replace the base params with Ecosense params  
  GOMsense$start_state$Biomass <- parlist[[irun]]$B_BaseRef
  parlist[[irun]]$BURN_YEARS <- 50			# Set Burn Years to 50
  GOMsense$params <- parlist[[irun]]
  GOMtest <- rsim.run(GOMsense, method = "RK4", years = all_years)
  failList <- which(is.na(GOMtest$end_state$BB))
  {if (length(failList)>0)
  {cat(irun,": fail in year ",GOMtest$crash_year,": ",failList,"\n"); kept[irun] <- F; flush.console()}
    else 
    {cat(irun,": success!\n"); kept[irun]<-T;  flush.console()}}
  parlist[[irun]]$BURN_YEARS <- 1
}

# KEPT tells you which ecosystems were kept
KEPT <- which(kept==T)
nkept <- length(KEPT)
nkept
# 1179 / 10000 = 11.8%
GOM.sense <- parlist[KEPT]
save(GOM.sense, file = file.path(data.dir, 'GOM_ecosense_valid.RData'))

#Run scenario-----
#Bottom up
scenario.out <- data.table(Group = c('Outside', GOM.28b$model$Group))
set.seed(123)
for(irun in 1:length(GOM.sense)){
  run.scene <- rsim.scenario(GOM, GOM.28b, years = all_years)
  run.scene$params <- GOM.sense[[irun]]
  run.base <- rsim.run(run.scene, years = all_years)
  
  run.scene$params$MzeroMort[2] <- run.scene$params$MzeroMort[2] - 
    run.scene$params$MzeroMort[2] * .10
  run.diff <- rsim.run(run.scene, years = all_years)
  #rsim.plot(run.base, run.scene$params$spname)
  
  #Save end / start biomass
  scenario.out <- scenario.out[, run.out := run.diff$end_state$BB / 
                                 run.base$end_state$BB][]
  setnames(scenario.out, 'run.out', paste0('run', irun))
  
  #Counter
  cat(irun,": processed\n")
  flush.console()
}

save(scenario.out, file = file.path(data.dir, 'GOM_ecosense_PP_up.RData'))

#summarize
spnames <- scenario.out[Group != 'Outside', Group]
scenario.out <- scenario.out[Group != 'Outside', ]
scenario.out[, Group := NULL]
pp.up <- data.table(Group = spnames)
for(isp in 1:length(spnames)){
  pp.up[isp, increase := length(which(scenario.out[isp, ] > 1))]
  pp.up[isp, decrease := length(which(scenario.out[isp, ] < 1))][]
  
  cat(isp,": processed\n")
  flush.console()
}

save(pp.up, file = file.path(data.dir, 'GOM_ecosense_PP_up_summary.RData'))

#Piscivore up
scenario.out <- data.table(Group = c('Outside', GOM.28b$model$Group))
set.seed(123)
for(irun in 1:length(GOM.sense)){
  run.scene <- rsim.scenario(GOM, GOM.28b, years = all_years)
  run.scene$params <- GOM.sense[[irun]]
  run.base <- rsim.run(run.scene, years = all_years)
  
  run.scene$params$MzeroMort[17] <- run.scene$params$MzeroMort[17] - 
    run.scene$params$MzeroMort[17] * .10
  run.diff <- rsim.run(run.scene, years = all_years)
  #rsim.plot(run.base, run.scene$params$spname)
  
  #Save end / start biomass
  scenario.out <- scenario.out[, run.out := run.diff$end_state$BB / 
                                 run.base$end_state$BB][]
  setnames(scenario.out, 'run.out', paste0('run', irun))
  #Counter
  cat(irun,": processed\n")
  flush.console()
}

save(scenario.out, file = file.path(data.dir, 'GOM_ecosense_Pisc_up.RData'))

#summarize
spnames <- scenario.out[Group != 'Outside', Group]
scenario.out <- scenario.out[Group != 'Outside', ]
scenario.out[, Group := NULL]
pisc.up <- data.table(Group = spnames)
for(isp in 1:length(spnames)){
  pisc.up[isp, increase := length(which(scenario.out[isp, ] > 1))]
  pisc.up[isp, decrease := length(which(scenario.out[isp, ] < 1))][]
  
  cat(isp,": processed\n")
  flush.console()
}

save(pisc.up, file = file.path(data.dir, 'GOM_ecosense_Piscivore_up_summary.RData'))

#Piscivore down
scenario.out <- data.table(Group = c('Outside', GOM.28b$model$Group))
set.seed(123)
for(irun in 1:length(GOM.sense)){
  run.scene <- rsim.scenario(GOM, GOM.28b, years = all_years)
  run.scene$params <- GOM.sense[[irun]]
  run.base <- rsim.run(run.scene, years = all_years)
  
  run.scene$params$MzeroMort[17] <- run.scene$params$MzeroMort[17] + 
    run.scene$params$MzeroMort[17] * .10
  run.diff <- rsim.run(run.scene, years = all_years)
  #rsim.plot(run.base, run.scene$params$spname)
  
  #Save end / start biomass
  scenario.out <- scenario.out[, run.out := run.diff$end_state$BB / 
                                 run.base$end_state$BB][]
  setnames(scenario.out, 'run.out', paste0('run', irun))
  #Counter
  cat(irun,": processed\n")
  flush.console()
}

save(scenario.out, file = file.path(data.dir, 'GOM_ecosense_Pisc_down.RData'))

#summarize
spnames <- scenario.out[Group != 'Outside', Group]
scenario.out <- scenario.out[Group != 'Outside', ]
scenario.out[, Group := NULL]
pisc.down <- data.table(Group = spnames)
for(isp in 1:length(spnames)){
  pisc.down[isp, increase := length(which(scenario.out[isp, ] > 1))]
  pisc.down[isp, decrease := length(which(scenario.out[isp, ] < 1))][]
  
  cat(isp,": processed\n")
  flush.console()
}

save(pisc.down, file = file.path(data.dir, 'GOM_ecosense_Piscivore_down_summary.RData'))
