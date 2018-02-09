#
#
## This script file iterates from the already created datasets
## "record.id","some.id", "birth.date","m.nmbr","h.nmbr"

# Function to iterate all experiments for Unsupervised classification      
runOneExpUnsupervised <- function(.max.weight, .min.weight,.iter, .qual ){ # for supervised training


  rds.pairs.tracker <- read.csv(file="rds.pairs.tracker.csv", stringsAsFactors=FALSE)
  
  
  classfn.results.parent <- as.data.frame(matrix(0,ncol=9,nrow=0))
  
  colnames(classfn.results.parent) <- c("No.","Total.Records","Duplicates",
                                        "Non.Duplicates","Record.Pairs","Quality.Perc",
                                        "Us.N","Us.P","Us.L")
  
      
      #-----------------------------------------------------------------
      pb = txtProgressBar(min = 0, max = 5, initial = 0)
      perc.value <- c(1, 2, 4, 8, 16)
      
      
      training.set <- readRDS(file="TrainSet_1000.rds")
      opt.threshold <- optimalThreshold(training.set)
      
       
          
            epiResults <- NULL
            classfn.results <- as.data.frame(matrix(0,ncol=9,nrow=0))
            
            colnames(classfn.results) <- c("No.","Total.Records","Duplicates",
                                           "Non.Duplicates","Record.Pairs","Quality.Perc",
                                           "Us.N","Us.P","Us.L")
            
                  
                    q.rating <- .qual
                    #reading the saved data pairs into memory
                    current.rpairs <- readRDS(file=rds.pairs.tracker$Rds.Filename[rds.pairs.tracker$Iteration.No==j & rds.pairs.tracker$Quality.Rating==q.rating])
                    
                    ## Determining Optimal Threshold
                    #opt.threshold <- optimalThreshold(minTrain.00)
                    record.count <- rds.pairs.tracker[rds.pairs.tracker[, "Iteration.No"]==j & rds.pairs.tracker[, "Quality.Rating"]==q.rating,]$Total.Records
                    dupli.ds <- rds.pairs.tracker[rds.pairs.tracker[, "Iteration.No"]==j & rds.pairs.tracker[, "Quality.Rating"]==q.rating,]$Duplicates
                    non.dupli.ds <- rds.pairs.tracker[rds.pairs.tracker[, "Iteration.No"]==j & rds.pairs.tracker[, "Quality.Rating"]==q.rating,]$Non.Duplicates
                    data.quality <- rds.pairs.tracker[rds.pairs.tracker[, "Iteration.No"]==j & rds.pairs.tracker[, "Quality.Rating"]==q.rating,]$Quality.Perc
                    
                    rpairs.00 <-current.rpairs
                    rpairs.11 <-current.rpairs
                    rpairs.33 <-current.rpairs
                   
                    
                    
                    ###Supervised classification
                    rpairs.00 <- epiWeights(rpairs.00)
                    
                    
                    
                    result <- epiClassify(rpairs.00, opt.threshold)
                    epiResults$comp.pairs <- getPairs(result, max.weight = .max.weight, min.weight = .min.weight)
                  
                    # Recording Results 
                    #===================================================================
                    
                    
                    classfn.results[i, 1] <- i
                    classfn.results[i, 2] <- record.count
                    classfn.results[i, 3] <- dupli.ds
                    
                    classfn.results[i, 4] <- non.dupli.ds
                    classfn.results[i, 5] <- nrow(rpairs.00$pairs)
                    classfn.results[i, 6] <- data.quality
                    
                    classfn.results[i, 8] <- summary(result$prediction)[2] # Us.N
                    classfn.results[i, 9] <- summary(result$prediction)[3] # Us.P
                    classfn.results[i, 7] <- summary(result$prediction)[1] #Us.L
                   
                
                
                
                    epiResults$classfn <- classfn.results
                    
                    
                    epiResults
        
  
      
}