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
Rpath2Qpress <- function(Rpath.params, pred.link = 0.1, prey.link = 0.1){
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
  qpress.prey <- qpress.prey[,-1]

  #Run Rpath to generate Rpath.obj
  Rpath.obj <- rpath(Rpath.params)
  qpress.pred <- as.data.table(write.Rpath(Rpath.obj, morts = T))
  #Need to normalize mortality terms
  M2.cols <- names(qpress.pred)[which(!names(qpress.pred) %in%
                                        c('Group', 'type', 'PB', 'M0',
                                          'Fmort[1:ngroup, ]'))]
  qpress.pred[, mort.sum := rowSums(.SD), .SDcols = M2.cols]
  qpress.pred <- qpress.pred[, .SD / mort.sum, .SDcols = M2.cols, by = Group]
  qpress.pred[is.na(qpress.pred)] <- 0

  #make qpress.pred negative
  qpress.pred[, c('type', 'PB', 'M0', 'Fmort[1:ngroup, ]', 'mort.sum') := NULL]
  qpress.pred <- qpress.pred[, .SD * -1, .SDcol = M2.cols, by = Group]

  #Ensure that the matrices are square
  for(idead in seq_along(detrital.groups)){
    det.M2 <- data.table(Group = qpress.pred[, Group], V1 = 0)
    setnames(det.M2, 'V1', paste0('M2.', detrital.groups[idead]))
    qpress.pred <- merge(qpress.pred, det.M2, by = 'Group')
  }
  #Remove prey names
  qpress.pred <- qpress.pred[,-1]
  #transpose pred matrix because predators need to be the rows
  qpress.pred <- t(qpress.pred)

  #Merge pred and prey matrices
  commat <- qpress.pred + qpress.prey

  #Pare down links based on input link strengths
  commat[commat >= -1 * pred.link & commat <= prey.link] <- 0

  return(commat)
}


