#
#
## Function to run a single iteration with a Trained Classifier
## 
#  

# Function to iterate the experiment      
runOneExpTrainedClassifier <- function(j, q.rating){ # for supervised training


 j <-1
 q.rating <- 4
 
 rds.pairs.tracker <- read.csv(file="rds.pairs.tracker.csv",stringsAsFactors=FALSE)
 
          
            classfn.results <- as.data.frame(matrix(0,ncol=9,nrow=0))
            
            colnames(classfn.results) <- c("No.","Total.Records","Duplicates",
                                           "Non.Duplicates","Record.Pairs","Quality.Perc",
                                           "Tc.N","Tc.P","Tc.L")
              
                    
            #reading the saved data pairs into memory
            current.rpairs <- readRDS(file=rds.pairs.tracker[rds.pairs.tracker[, "Iteration.No"]==j & rds.pairs.tracker[, "Quality.Rating"]==q.rating,]$Rds.Filename)
            minTrain.00 <- readRDS(file=rds.pairs.tracker[rds.pairs.tracker[, "Iteration.No"]==j & rds.pairs.tracker[, "Quality.Rating"]==q.rating,]$Train.Set)
            
            ## Determining Optimal Threshold
            opt.threshold <- optimalThreshold(minTrain.00)
            record.count <- rds.pairs.tracker[rds.pairs.tracker[, "Iteration.No"]==j & rds.pairs.tracker[, "Quality.Rating"]==q.rating,]$Total.Records
            dupli.ds <- rds.pairs.tracker[rds.pairs.tracker[, "Iteration.No"]==j & rds.pairs.tracker[, "Quality.Rating"]==q.rating,]$Duplicates
            non.dupli.ds <- rds.pairs.tracker[rds.pairs.tracker[, "Iteration.No"]==j & rds.pairs.tracker[, "Quality.Rating"]==q.rating,]$Non.Duplicates
            data.quality <- rds.pairs.tracker[rds.pairs.tracker[, "Iteration.No"]==j & rds.pairs.tracker[, "Quality.Rating"]==q.rating,]$Quality.Perc
            
           
            training.set <- getMinimalTrain(current.rpairs, nEx=1)
            count.trained.set  <- nrow(training.set$pairs)
            trained.set  <- editMatch(training.set)
            
            saveRDS(trained.set, file= str_c("TrainSet_",.ds.count,".rds")) # Save the R trained pairs for retrieval
           
              
            model.33 <- trainSupv(minTrain.00, method = "rpart",minsplit=1) # Training method is passed as argument
              
            class.results.33 <- classifySupv(model.33, newdata = current.rpairs)
            
            summary(class.results.33)
            getFalseNeg(class.results.33, single.rows = FALSE)
            
            getPairs(class.results.33, filter.match = c("match"), filter.link=c("link"))

            # Recording Results 
            #===================================================================
              
              
            classfn.results[1, 1] <- 1
            classfn.results[1, 2] <- record.count
            classfn.results[1, 3] <- dupli.ds
              
            classfn.results[1, 4] <- non.dupli.ds
            classfn.results[1, 5] <- nrow(class.results.33$pairs)
            classfn.results[1, 6] <- data.quality
              
            classfn.results[1, 7] <- summary(class.results.33$prediction)[1]
            classfn.results[1, 8] <- summary(class.results.33$prediction)[2]
            classfn.results[1, 9] <- summary(class.results.33$prediction)[3]
                    
            summary(class.results.33)
               
                
         
}