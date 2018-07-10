#'Mental Modeler to Qpress
#'
#'Uses the output from Mental Modeler to set up the input for Qpress.
#'
#'@family QNM_comparisons
#'
#'@param MMout Output from Mental modeler
#'
#'@return Output object for Qpress
#'@export
MM2Qpress <- function(MMout){
  mental.sheet <- as.data.table(MMout)
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


#'Signed diagraphs to Qpress
#'
#'Uses the output from a signed diagraph to set up the input for Qpress.
#'
#'@family QNM_comparisons
#'
#'@param modmat Output from signed diagraphs
#'
#'@return Output object for Qpress
#'@export
make.qnm <- function(modmat){
  q <- MM2Qpress(modmat)
  qnm <- dput(q)
  qnm.mod <- parse.digraph(qnm)
  qnm.model <- enforce.limitation(qnm.mod)
}

#'Rpath to Qpress
#'
#'Uses the output from Rpath to set up the input for Qpress.
#'
#'@family QNM_comparisons
#'
#'@param Rpath.params Parameter object from Rpath
#'
#'@return Output object for Qpress
#'@export
Rpath2Qpress <- function(Rpath.params, link.strength = 0.1){
  qpress.prey <- Rpath.params$diet
  pred.cols   <- names(qpress.prey)[which(names(qpress.prey) != 'Group')]
  qpress.prey <- qpress.prey[Group != 'Import']

  #Ensure that the matrices are square
  #Need to maintain original order - maybe...
  groupnames <- as.character(qpress.prey$Group)
  #Identify detrital groups to add as predators with zero diet
  detrital.groups <- Rpath.params$model[Type == 2, Group]
  for(idead in seq_along(detrital.groups)){
    det.pred <- data.table(Group = qpress.prey[, Group], V1 = 0)
    setnames(det.pred, 'V1', detrital.groups[idead])
    qpress.prey <- merge(qpress.prey, det.pred, by = 'Group')
  }
  #Remove prey names
  qpress.prey<- qpress.prey[,-1]

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

