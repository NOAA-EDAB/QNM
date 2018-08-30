# Results processing for GB BDN
# Created: 8/30/2018, Robert Wildermuth

# Function to calculate posterior probability certainty index per node (PPCI; Marcot 2012)
PPCI <- function(postsMat){
  L <- log(postsMat)
  Hmax <- ncol(postsMat)
  H <- postsMat*L
  H <- -rowSums(H)
  J <- H/Hmax
  return(1-J)
}

posteriors <- read.delim("C:/Users/rwildermuth/Dropbox/PhD_UMass/QNMproject/GB/GB_snapshot_posteriors.txt", 
                         stringsAsFactors = FALSE)

postTides <- posteriors[posteriors$X %in% c("tides", "tides1", "tides_lag1"), 2:4]
for(i in 1:ncol(postTides)){
  postTides[ ,i] <- as.numeric(postTides[ ,i])
}
postTides <- as.matrix(postTides)

allPPCI <- PPCI(postTides)

postStates <- posteriors[!posteriors$X %in% c("tides", "tides1", "tides_lag1", ""), 2:3]
for(i in 1:ncol(postStates)){
  postStates[ ,i] <- as.numeric(postStates[ ,i])
}
postStates <- as.matrix(postStates)

allPPCI <- c(allPPCI, PPCI(postStates))

# # List of projection indices
# indices <- c("profits1", "employment1", "seafood1", "recParticip1", "habPel1", "habDem1", "habNear1", 
#              "ground1", "forage1", "inverts1", "PS1")
# 
# indPPD <- posteriors[posteriors$X %in% indices, 1:3]
# for(i in 2:ncol(indPPD)){
#   indPPD[ ,i] <- as.numeric(indPPD[ ,i])
# }

results <- data.frame(index = posteriors$X, postP_mostProbable = NA, postP_PPCI = NA, IncTemp_mostProbable = NA, IncTemp_PPCI = NA)
results <- results[results$index != "", ]

results$postP_PPCI <- allPPCI

incTemp <- read.delim("C:/Users/rwildermuth/Dropbox/PhD_UMass/QNMproject/GB/GB_snapshot_IncSSTandBT.txt", 
                         stringsAsFactors = FALSE)

tempTides <- incTemp[incTemp$X %in% c("tides", "tides1", "tides_lag1"), 2:4]
for(i in 1:ncol(tempTides)){
  tempTides[ ,i] <- as.numeric(tempTides[ ,i])
}
tempTides <- as.matrix(tempTides)

allPPCI <- PPCI(tempTides)

tempStates <- incTemp[!incTemp$X %in% c("tides", "tides1", "tides_lag1", ""), 2:3]
for(i in 1:ncol(tempStates)){
  tempStates[ ,i] <- as.numeric(tempStates[ ,i])
}
tempStates <- as.matrix(tempStates)

results$IncTemp_PPCI <- c(allPPCI, PPCI(tempStates))
# posterior probability is certain for perturbed nodes
results[results$index %in% c("SST", "BT"), "IncTemp_PPCI"] <- 1

# # change order
# results <- results[match(indices, results$index), ]

# reference for completely even distribution
results$even <- PPCI(matrix(c(0.5, 0.5), nrow = 1, ncol = 2))
results[results$index == "tides", "even"] <- PPCI(matrix(c(1/3, 1/3, 1/3), nrow = 1, ncol = 3))
#any(results[,2:5] <= results$even)

# Identify which scenario is most certain for each projected index
for(i in 1:nrow(results)){
  if(length(colnames(results)[which(results[i,c("postP_PPCI", "IncTemp_PPCI")]==max(results[i,c("postP_PPCI", "IncTemp_PPCI")]))+1]) > 1){
    print(colnames(results)[which(results[i,c("postP_PPCI", "IncTemp_PPCI")]==max(results[i,c("postP_PPCI", "IncTemp_PPCI")]))+1])
    results$mostCertain[i] <- "tie"
  } else {
    results$mostCertain[i] <- colnames(results)[which(results[i,c("postP_PPCI", "IncTemp_PPCI")]==max(results[i,c("postP_PPCI", "IncTemp_PPCI")]))*2]
  }
}

# Identify most probable state in each scenario 1 = low state, 2 = high state
for(i in 1:nrow(postStates)){
  results$postP_mostProbable[i+1] <- which(postStates[i,]==max(postStates[i,]))
  results$IncTemp_mostProbable[i+1] <- which(tempStates[i,]==max(tempStates[i,]))
}

# habNear states are reverse order
results[results$index == "habNear", "postP_mostProbable"] <- which(c(1,2)!=results[results$index == "habNear", "postP_mostProbable"])
results[results$index == "habNear", "IncTemp_mostProbable"] <- which(c(1,2)!=results[results$index == "habNear", "IncTemp_mostProbable"])

# hardcode tides node as factor level 3="intermediate"
results[results$index == "tides", "postP_mostProbable"] <- 3#which(postTides==max(postTides))
results[results$index == "tides", "IncTemp_mostProbable"] <- 3#which(tempTides==max(tempTides))

results$postP_mostProbable <- as.factor(as.numeric(results$postP_mostProbable))
levels(results$postP_mostProbable) <- c("low", "high", "intermediate")
results$IncTemp_mostProbable <- as.factor(as.numeric(results$IncTemp_mostProbable))
levels(results$IncTemp_mostProbable) <- c("low", "high", "intermediate")


scaledPPCI <- results
for(i in 1:nrow(scaledPPCI)){
  for(j in c(3,5)){
    scaledPPCI[i,j] <- sqrt((scaledPPCI[i,j]-scaledPPCI[1,"even"])/(1-scaledPPCI[1,"even"]))
  }
}

write.csv(results, "C:/Users/rwildermuth/Dropbox/PhD_UMass/QNMproject/GB/GB_BBN_IncTemp_resultsSummary.csv", row.names = FALSE)
