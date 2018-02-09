#
#
## This script file creates training dataset
#  

setwd("F:/GitHub/deduplication/src")
wd <- getwd()
setwd("..")
parent <- getwd()
setwd(wd)
source("munge/01-A.R")

createTrainingSet(1,4)

# Function to create  a training set
createTrainingSet <- function(.iteration, .quality){
  
  count.train <- .iteration* 5000
  
  rds.pairs.tracker <- read.csv(file="rds.pairs.tracker.csv", stringsAsFactors=FALSE)
        
  current.rpairs <- readRDS(file=rds.pairs.tracker[rds.pairs.tracker[, 
                "Iteration.No"]==.iteration & rds.pairs.tracker[, "Quality.Rating"]==.quality,]$Rds.Filename)
  current.rpairs <- epiWeights(current.rpairs)
              
  ##get Training set
  training.set <- getMinimalTrain(current.rpairs, nEx=1)
  count.trained.set  <- nrow(training.set$pairs)
  trained.set  <- editMatch(training.set)
  
  saveRDS(trained.set, file= str_c("TrainSet_",.ds.count,".rds")) # Save the R trained pairs for retrieval
              
}


