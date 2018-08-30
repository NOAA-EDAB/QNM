#Fuzzy Cognizant Mapping using FCMapper
#Georeges Bank
if(Sys.info()['sysname']=="Windows"){
  data.dir <- "C:/Users/Sean.Lucey/Desktop/QNM/IEA_QNM"
  out.dir  <- "C:/Users/Sean.Lucey/Desktop/QNM/IEA_QNM"
}
if(Sys.info()['sysname']=="Linux"){
  data.dir <- "/home/slucey/slucey/Rpath_code/data"
  out.dir  <- "/home/slucey/slucey/Rpath_code/outputs"
}

#-------------------------------------------------------------------------------
#Required packages
library(data.table); library(FCMapper)

#-------------------------------------------------------------------------------
#User created functions

#-------------------------------------------------------------------------------
#Load adjaceny matrix developed using mental modeler
GB <- as.matrix(read.csv(file = file.path(data.dir, 'GB_QNM_adjacencyMat.csv'),
               row.names = 1))
GB[is.na(GB)] <- 0
concept.names <- row.names(GB)

#Diagnostics
check.matrix(GB)
matrix.indices(GB)
GBConceptIndices <- concept.indices(GB, concept.names)

#Run scenarios
nochanges.scenario(GB, concept.names, 100)
NoChangesResults <- nochanges.scenario(GB, concept.names, 100)

graph.fcm(GB, concept.sizes = NoChangesResults$Equilibrium_value,
          concept.names)

#Climate scenario
GB.CC <- changes.scenario(GB, concept.names, iter=25,
                          set.concepts = c("Surface Temperature", "Bottom Temperature"),
                          set.values = 1)
summary(GB.CC)

#Compare scenarios
CC.compare <- comp.scenarios(GB.CC, NoChangesResults)
comp.maps(concept.names, concept.names)
