#
#
## This script file iterates from the already created datasets
## "record.id","some.id", "birth.date","m.nmbr","h.nmbr"
#  

# Function to iterate the experiment      
runAllExpOptimalThreshold <- function(.st.method){ # for supervised training


  classfn.results.parent <- as.data.frame(matrix(0,ncol=13,nrow=0))
  
  colnames(classfn.results.parent) <- c("No.","Total.Records","Duplicates",
                                 "Non.Duplicates","Record.Pairs","Quality.Perc",
                                 "Us.L","Us.Perc",
                                 "Tc.L","Tc.Perc",
                                 "St.L","St.Perc",
                                 "Train.Count")
  
      
      #-----------------------------------------------------------------
      pb = txtProgressBar(min = 0, max = 5, initial = 0)
      perc.value <- c(1, 2, 4, 8, 16)
      
          for(j in 1:6){   # This refers to quality specified across the columns
          
            classfn.results <- as.data.frame(matrix(0,ncol=13,nrow=0))
            
            colnames(classfn.results) <- c("No.","Total.Records","Duplicates",
                                           "Non.Duplicates","Record.Pairs","Quality.Perc",
                                           "Us.L","Us.Perc",
                                           "Tc.L","Tc.Perc",
                                           "St.L","St.Perc",
                                           "Train.Count")
                for( i in 1:5){
                  
                    q.rating <- perc.value[i]
                    
                    #reading the saved data pairs into memory
                    current.rpairs <- readRDS(file=rds.pairs.tracker[rds.pairs.tracker[, "Iteration.No"]==j & rds.pairs.tracker[, "Quality.Rating"]==q.rating,]$Rds.Filename)
                    minTrain.00 <- readRDS(file=rds.pairs.tracker[rds.pairs.tracker[, "Iteration.No"]==j & rds.pairs.tracker[, "Quality.Rating"]==q.rating,]$Train.Set)
                    
                    ## Determining Optimal Threshold
                    opt.threshold <- optimalThreshold(minTrain.00)
                    record.count <- rds.pairs.tracker[rds.pairs.tracker[, "Iteration.No"]==j & rds.pairs.tracker[, "Quality.Rating"]==q.rating,]$Total.Records
                    dupli.ds <- rds.pairs.tracker[rds.pairs.tracker[, "Iteration.No"]==j & rds.pairs.tracker[, "Quality.Rating"]==q.rating,]$Duplicates
                    non.dupli.ds <- rds.pairs.tracker[rds.pairs.tracker[, "Iteration.No"]==j & rds.pairs.tracker[, "Quality.Rating"]==q.rating,]$Non.Duplicates
                    data.quality <- rds.pairs.tracker[rds.pairs.tracker[, "Iteration.No"]==j & rds.pairs.tracker[, "Quality.Rating"]==q.rating,]$Quality.Perc
                    
                    rpairs.00 <-current.rpairs
                    rpairs.11 <-current.rpairs
                    rpairs.33 <-current.rpairs
                   
                    
                    ###Supervised classification
                    rpairs.00 <- epiWeights(rpairs.00)
                    rpairs.00 <- classifyUnsup(rpairs.00, method = "kmeans")
                    
                    
                   
                    count.train <- nrow(minTrain.00$pairs)
                    
                    model.00 <- trainSupv(minTrain.00, method = .st.method) # Training method is passed as argument
                    min.train.ds <- classifySupv(model.00, newdata = rpairs.00)
                    
                    #============================================================ 
                    #Unsupervised Classification
                    rpairs.11 <- epiWeights(rpairs.11, e=0.01, f =rpairs.11$frequencies)
                    results.11 <- classifyUnsup(rpairs.11, method = "kmeans")
                    remove(results.11)
                    
                    #============================================================
                    #Unsupervised Classification using epiClassify
                    rpairs.33 <- epiWeights(rpairs.33, e=0.01, f =rpairs.33$frequencies)
                    
                    # epiClassify with Optimal threshold
                    
                    results.33 <- epiClassify(rpairs.33,opt.threshold,opt.threshold)  
                    
                    
                    model.33 <- trainSupv(results.33, method = .st.method, use.pred=TRUE) # Training method is passed as argument
                    class.results.33 <- classifySupv(model.33, newdata = rpairs.33)
                    
                   
                    
                    # Recording Results 
                    #===================================================================
                    
                    
                    classfn.results[i, 1] <- i
                    classfn.results[i, 2] <- record.count
                    classfn.results[i, 3] <- dupli.ds
                    
                    classfn.results[i, 4] <- non.dupli.ds
                    classfn.results[i, 5] <- nrow(results.33$pairs)
                    classfn.results[i, 6] <- data.quality
                    classfn.results[i, 7] <- summary(results.33$prediction)[3]
                    classfn.results[i, 8] <- round(summary(results.33$prediction)[3]/dupli.ds*100,2)
                    classfn.results[i, 9] <- summary(class.results.33$prediction)[3]
                    classfn.results[i, 10] <- round(summary(class.results.33$prediction)[3]/dupli.ds*100,2)
                    classfn.results[i, 11]  <- summary(min.train.ds$prediction)[3]
                    classfn.results[i, 12]  <- round(summary(min.train.ds$prediction)[3]/dupli.ds*100,2)
                    classfn.results[i, 13] <- count.train
                
                }
                
            classfn.results.parent <- rbind(classfn.results.parent,classfn.results)
          }
      classfn.results.parent
      
}