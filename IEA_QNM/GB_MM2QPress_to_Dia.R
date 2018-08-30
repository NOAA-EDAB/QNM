# Code to create correctly formatted Dia file of Georges Bank model
# Created: 7/12/2018, Robert Wildermuth

library(data.table)
library(QPress)

# Taken from: https://github.com/NOAA-EDAB/QNM/blob/master/looping_qpress_Rpath.R
#function to create signed digraph from Mental Modeler 
MM2Qpress <- function(data){
  
  mental.sheet <- as.data.table(data)
  names(mental.sheet)[1] <- 'Box'
  n <- nrow(mental.sheet)
  box.names <- names(mental.sheet)[which(names(mental.sheet) != 'Box')]
  model <- c()
  for(i in 1:n){
    pos <- which(mental.sheet[i, .SD, .SDcol = box.names] > 0)
    if(length(pos) > 0){
      pos.interaction <- paste(mental.sheet[i, Box], '->', 
                               names(mental.sheet[i, pos + 1, with = F]), 
                               sep = '')
      model <- append(model, pos.interaction)
    }
    
    neg <- which(mental.sheet[i, .SD, .SDcol = box.names] < 0)
    if(length(neg) > 0){
      neg.interaction <- paste(mental.sheet[i, Box], '-*', 
                               names(mental.sheet[i, neg + 1, with = F]), 
                               sep = '')
      model <- append(model, neg.interaction)
    }
  }
  return(model)
}
#function to create qnm models for qpress from signed digraphs
make.qnm<-function(modmat){
  q<-MM2Qpress(modmat)
  qnm<-dput(q)
  qnm.mod<-parse.digraph(qnm)
  qnm.model<-enforce.limitation(qnm.mod)
}

# Read in Georges Bank Mental Modeler adjacency matrix
adjGB <- read.csv("C:/Users/rwildermuth/Dropbox/PhD_UMass/QNMproject/GB/GB_QNM_adjacencyMat.csv")

# need to change the column and row names to remove spaces and periods
nodeNames <- gsub(".", "", names(adjGB), fixed = TRUE)
names(adjGB) <- nodeNames
adjGB$X <- nodeNames[-1]

qpressGB <- make.qnm(adjGB)

write.dia(qpressGB, "C:/Users/rwildermuth/Dropbox/PhD_UMass/QNMproject/GB/GeorgesBank_DiaReDo.dia")
