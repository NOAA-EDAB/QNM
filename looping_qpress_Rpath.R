#function to create signed digraph from Mental Modeler 
MM2Qpress <- function(data){
  library(data.table)
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

#--------------------------------------------------------------------
#####PERTURBATIONS#####
#to loop over multiple models
#create folder with files in directory, call list of files
library(QPress)
set.seed(1984)
#load RData files 
#create list
mod<-list(GOMqnm20, GOMqnm30, GOMqnm40, GOMqnm50)
# mod<-list(GOMqnm20, GOMqnm30,GOMqnm40, GOMqnm50)
#get MM matrix into qpress format, create model
q.models<-vector('list', length(mod))  
for(i in seq_along(mod)){
  q.models[[i]]<-make.qnm(mod[[i]])
} 
#write dia files and save
for(i in seq_along(q.models)){
  #can add text below to further specify name
  write.dia(q.models[[i]], paste0("GOMtest", i,".dia"))
}

#loop through models to create results csv file and pdf barplots
for(j in seq_along(q.models)){
  A <- adjacency.matrix(q.models[[j]])
  #A
  
  ## Function to generate the community matrix
  s <- community.sampler(q.models[[j]])
  
  ## Function to check the validation condition
  #press <- press.validate(GB2el,
  #                        perturb=c("Forage_Fish"=-1),
  #                        monitor=c("Commercial_Pelagic_Fishery"=1))
  #Press/Perturbation with decrease forage fish and no monitoring)
  press <- press.validate(q.models[[j]],
                           perturb=c("Phytoplankton Primary Producers"=1), monitor=F)
  
  ## Function to define the perturbation scenario
  impact <- press.impact(q.models[[j]],perturb=c("Phytoplankton Primary Producers"=1))
  ## Use 1000 simulations
  n.sims <- 200
  results <- 0
  i <- 0
  while(i < n.sims) {
    
    ## Randomly choose edges to retain
    z <- s$select(runif(1))
    ## Sample community matrix
    W <- s$community()
    
    ## Check press condition and stability
    if(!(press(W) && stable.community(W))) next
    
    ## Monitor impact post press
    imp <- impact(W)
    imp[abs(imp)<1e-07]=0 #added to remove small perturbations
    results <- results + outer(sign(imp),-1:1,'==')
    i <- i+1
    
    rownames(results) <- levels(q.models[[j]]$From)
    colnames(results) <- c('-','0','+')
    write.csv(results, paste0("GOM","results", j,"Phytoplankton Primary Producers up",".csv"))
    
    library(RColorBrewer)
    pal <- brewer.pal(n=5,"Greys")[5:1]
    #opar <- par(mar=c(5,10,1,1)+0.1)
    prop <- results/rowSums(results)
    r <- colSums(t(prop)*(-1:1))
    filename= paste("GOM", "plots", j,"Phytoplankton Primary Producers up", ".pdf")
    pdf(filename)
    #adjust barplot if you want it ordered by proportion
    par(mar=c(5,10,1,1)+0.1)
    barplot(t(prop),
            horiz=T,cex.names=0.7,cex.axis=0.8,las=2,border=T, col=pal,xlab="Proportion")
    
    
    dev.off()
  }
}
#code for ordered barplot
# pal <- brewer.pal(n=5,"Greys")[5:1]
# opar <- par(mar=c(5,10,1,1)+0.1)
# prop <- results/rowSums(results)
# r <- colSums(t(prop)*(-1:1))
# filename= paste("wss", "plot", j,"phytoplankton up", ".pdf")
# pdf(filename)
# barplot(t(prop[order(r),]),
#         horiz=T,cex.names=0.6,cex.axis=0.8,las=2,border=T,col=pal,xlab="Proportion")
# par(opar)
