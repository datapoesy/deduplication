#
#
## This script file iterates from the already created datasets
## "record.id","some.id", "birth.date","m.nmbr","h.nmbr"
#  
# Function to iterate the experiment      
# results saved to ResultsWithTrainSet15000.csv

# To Execute
#ResultsWithTrainSet20000 <- runAllExpTrainedClassifier()
#write.csv(ResultsWithTrainSet20000, file=str_c("ResultsWithTrainSet20000.csv"))



runAllExpTrainedClassifier <- function(){ # for supervised training

  setwd("F:/GitHub/deduplication/src")
  classfn.results.parent <- as.data.frame(matrix(0,ncol=11,nrow=0))
  
  
  rds.pairs.tracker <- read.csv(file="rdscsvfiles/rds.pairs.tracker.csv",stringsAsFactors=FALSE)
  
  
  colnames(classfn.results.parent) <- c("No.","Total.Records","Duplicates",
                                 "Non.Duplicates","Record.Pairs","Quality.Perc",
                                 "Tc.N","Tc.P","Tc.L","Accuracy","Error")
  
      
      #-----------------------------------------------------------------
      pb = txtProgressBar(min = 0, max = 5, initial = 0)
      perc.value <- c(1, 2, 4, 8, 16)
      
          for(j in 1:6){   # This refers to quality specified across the columns
          
            classfn.results <- as.data.frame(matrix(0,ncol=11,nrow=0))
            
            colnames(classfn.results) <- c("No.","Total.Records","Duplicates",
                                           "Non.Duplicates","Record.Pairs","Quality.Perc",
                                           "Tc.N","Tc.P","Tc.L","Accuracy","Error")
                
                
                for(i in 1:5){
                    .q.rating <- perc.value[i]
                    
                    #reading the saved data pairs into memory
                    current.rpairs <- readRDS(file=rds.pairs.tracker[rds.pairs.tracker[, "Iteration.No"]==j & rds.pairs.tracker[, "Quality.Rating"]==.q.rating,]$Rds.Filename)
                    # minTrain.00 <- readRDS(file=rds.pairs.tracker[rds.pairs.tracker[, "Iteration.No"]==1 & rds.pairs.tracker[, "Quality.Rating"]==.q.rating,]$Train.Set)
                    minTrain.00 <- readRDS(file="rdscsvfiles/minTrain.00Pairs20000.rds")
                    
                    ## Determining Optimal Threshold
                   
                    record.count <- rds.pairs.tracker[rds.pairs.tracker[, "Iteration.No"]==j & rds.pairs.tracker[, "Quality.Rating"]==.q.rating,]$Total.Records
                    dupli.ds <- rds.pairs.tracker[rds.pairs.tracker[, "Iteration.No"]==j & rds.pairs.tracker[, "Quality.Rating"]==.q.rating,]$Duplicates
                    non.dupli.ds <- rds.pairs.tracker[rds.pairs.tracker[, "Iteration.No"]==j & rds.pairs.tracker[, "Quality.Rating"]==.q.rating,]$Non.Duplicates
                    data.quality <- rds.pairs.tracker[rds.pairs.tracker[, "Iteration.No"]==j & rds.pairs.tracker[, "Quality.Rating"]==.q.rating,]$Quality.Perc
                    
                    current.rpairs <- compare.dedup(current.rpairs,blockfld = list(4,c(16,17,18)),phonetic = c(3,4),
                                                    exclude = c("record.id","some.id", "birth.date"), n_match = 4000)
                   
                                               
                   
                    current.rpairs <- epiWeights(current.rpairs, e=0.01, f =current.rpairs$frequencies)
                   
                    model.22 <- trainSupv(minTrain.00, method = "rpart",minsplit=1) # Training method is passed as argument
                    
                    results.22 <- classifySupv(model.22, newdata = current.rpairs)
                    
                  #  summary(results.22)
                   
                    
                    # Recording Results 
                    #===================================================================
                    
                    
                    classfn.results[i, 1] <- i
                    classfn.results[i, 2] <- record.count
                    classfn.results[i, 3] <- dupli.ds
                    
                    classfn.results[i, 4] <- non.dupli.ds
                    classfn.results[i, 5] <- nrow(results.22$pairs)
                    classfn.results[i, 6] <- data.quality
                    
                    classfn.results[i, 7] <- summary(results.22$prediction)[1]
                    classfn.results[i, 8] <- summary(results.22$prediction)[2]
                    classfn.results[i, 9] <- summary(results.22$prediction)[3]
                    
                    classfn.results[i, 10] <- round(summary(results.22$prediction)[3]/dupli.ds*100,2)
                    classfn.results[i, 11] <- (100-(round(summary(results.22$prediction)[3]/dupli.ds*100,2)))
                    
                   
                }
            classfn.results.parent <- rbind(classfn.results.parent,classfn.results)
          }
     
      classfn.results.parent
      
}