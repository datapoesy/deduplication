# #
# #
# ## This script file handles the dataset excludes
# ## "record.id","some.id", "birth.date","m.nmbr","h.nmbr"
# #  
# 
# setwd("D:/Assembla/deduplication/src")
# wd <- getwd()
# setwd("..")
# parent <- getwd()
# setwd(wd)
# source("munge/01-A.R")

# Calls the iterating function
# Pass the base dataframe, number of iterations & quality rating 
# count will be multiplied by 5 
# iter <- 5
# .threshold.up -  Upper threshold for epiClassify function  if not given, default will be 0.75
# .threshold.lo -  Lower threshold for epiClassify function
# .st.method    -  Classification method for Supervised training "rpart","bagging",'ada","nnet","bumping"
# qual.rating <- 8
# QS.results <- repeatExperiment(base.ds,5,qual.rating)
# 
# #Save the results
# write.csv(QS.results, file =str_c("QS.results_iter",iter,"_qual",qual.rating,"_.csv"))


    
# Function to iterate the experiment      
repeatExperiment <- function(.base.ds,
                             .maxiter, #Maximum number of iterations  
                             .count, # Number of records in each iterations  
                             .qual, # Quality rating as defined internally  
                             .threshold.up, # upper threshold to epiClassify method  
                             .threshold.lo, # classification threshold to the epiClassify method  
                             .st.method, # for supervised training  
                             .isNewTS){  # Whether a new training set is required (TRUE or FALSE.  If false, the previous one will be used.
        
      QS.results <- as.data.frame(matrix(0,ncol=13,nrow=5))
      
      colnames(QS.results) <- c("No.","Total.Records","Duplicates",
                                    "Non.Duplicates","Record.Pairs","Quality.Perc",
                                    "Us.L","Us.Perc",
                                    "Tc.L","Tc.Perc",
                                    "St.L","St.Perc",
                                    "Train.Count")
      #-----------------------------------------------------------------
      pb = txtProgressBar(min = 0, max = 5, initial = 0)
      perc.value <- c(1, 2, 4, 8, 16)
      
      
    
      
      for( i in 1:.maxiter){
      
              set.seed(1234)
        
              .ds.count <- i* .count
              ## Set the count of records in the dataset
              
              ## Sample dataset from the original 100K records
              ds.00 <-  .base.ds[sample(nrow(.base.ds),.ds.count),]
              
              ## Remove the original 100K dataset to conserve memory
              #remove(.base.ds)
              
              ## Set a progress bar
              setTxtProgressBar(pb,i)
              
              ## set a percentage value for data to be disorganized
              # if a value is passed, that will be used, else progressively as in the
              # perc.value array
         
              p <- if(is.null(.qual)) perc.value[i] else .qual
      
              
              ## create a data frame to store duplicated records
              dupli.ds <- data.frame(matrix(0, ncol = ncol(ds.00), nrow = 0))
              colnames(dupli.ds) <- colnames(ds.00)
              
              dupli.ds$some.id <- as.character(dupli.ds$some.id)
              dupli.ds$m.nmbr <- as.character(dupli.ds$m.nmbr)
              dupli.ds$h.nmbr <- as.character(dupli.ds$h.nmbr)
              dupli.ds$birth.date <- as.character(dupli.ds$birth.date)
              dupli.ds$postcode <- as.character(dupli.ds$postcode)
              
              ## create a dataframe to store non-duplicated records
              non.dupli.ds <- dupli.ds
              
              #simulate missing email id - write into the base dataset
              ds.00$email[sample(nrow(ds.00), nrow(ds.00) * p / 100)] <- ''
              
              #simulate missing home phone numbers
              ds.00$h.nmbr[sample(nrow(ds.00), nrow(ds.00) * 40 / 100)] <- ''
              
              ###Missing Mobile phone numbers
              ds.00$m.nmbr[sample(nrow(ds.00), nrow(ds.00) * p / 100)] <- ''
              
              #Simulate missing postcodes
              ds.00$postcode[sample(nrow(ds.00), nrow(ds.00) * p / 100)] <- ''
              
              ###Simulate Missing Address 1 from Address
              ds.00$addr.1[sample(nrow(ds.00), nrow(ds.00) * p / 100)] <- ''
              
              ###simulate Missing Address 2 from Address
              ds.00$addr.2[sample(nrow(ds.00), nrow(ds.00) * p / 100)] <- ''
              
              ###Simulate Missing County Name from Address
              ds.00$county[sample(nrow(ds.00), nrow(ds.00) * p / 100)] <- ''
              
              ###Simulate Missing City Name from Address
              ds.00$cityname[sample(nrow(ds.00), nrow(ds.00) * p / 100)] <- ''
              
              #-------------------------------------------------------------------------
              
              ###Duplicate records with same  address
              temp.ds <- clonePerson(ds.00, p)
              dupli.ds <- rbind(dupli.ds, temp.ds)
              
              
              ###Duplicate Records for identical Twins living in the same address
              pt <- 0.05*p
              remove(temp.ds)
              temp.ds <- updateTwins(ds.00, pt)
              non.dupli.ds <- rbind(non.dupli.ds, temp.ds) # Non-duplicates means individual people
              
              
              ###Duplicate Records for Married couples living in the same address
              # % of couples in the loan availing population = 50%
              # % of couples living in the same address = 80%
              # Combined percentage of the above = 40%
              pc <- 5*p
              remove(temp.ds)
              temp.ds <- updateCouples(ds.00, pc)
              non.dupli.ds <- rbind(non.dupli.ds, temp.ds)
              
              #---------------------------------------------------------------
              
              ###Create Typographical errors name to simulate spelling mistakes
              ds.00 <- makeTypos(ds.00, "John", "Jon", pc)
              ds.00 <- makeTypos(ds.00, "Thompson", "Thomson", pc)
              ds.00 <- makeTypos(ds.00, "tt", "t", pc)
              ds.00 <- makeTypos(ds.00, "ss", "s", pc)
              
              #=====================================================================
              
              #changes.ds <- rbind(changes.ds, temp.ds)
              ds.00 <- rbind(ds.00, dupli.ds)
              ds.00 <- rbind(ds.00,non.dupli.ds)
              
              
              record.count <- nrow(ds.00)
              
              #===================  dataset complete =========================
              
              #Creating comparison vectors
              rpairs.00 <- compare.dedup(ds.00,blockfld = list(4),phonetic = (3:4),
                    exclude = c("record.id","some.id", "birth.date","m.nmbr","h.nmbr"))
              
              rpairs.11 <- rpairs.00
              rpairs.22 <- rpairs.00
              rpairs.33 <- rpairs.00
              
              #
              #============  pre processing complete ===========================
              
              
              ###Supervised clasification
              rpairs.00 <- epiWeights(rpairs.00)
              rpairs.00 <- classifyUnsup(rpairs.00, method = "kmeans")
              
              if(.isNewTS){  #  Is a new Training set required?
                minTrain.00 <- getMinimalTrain(rpairs.00)
                minTrain.00 <- editMatch(minTrain.00)
                count.train <- nrow(minTrain.00$pairs)
                saveRDS(minTrain.00, file= str_c("TrainSet_",count.train,"_",.ds.count,".rds")) # Save the R trained pairs for retrieval
              }else{
                minTrain.00 <- readRDS(file="rdscsvfiles/TrainSet_5000.rds")
                count.train <- nrow(minTrain.00$pairs)
              }
              
              model.00 <- trainSupv(minTrain.00, method = .st.method) # Training method is passed as argument
              min.train.ds <- classifySupv(model.00, newdata = rpairs.00)
              
              #============================================================  Quality
              #Unsupervised Classification
              rpairs.11 <- epiWeights(rpairs.11, e=0.01, f =rpairs.11$frequencies)
              results.11 <- classifyUnsup(rpairs.11, method = "kmeans")
              remove(results.11)
              
              #========================================================================
              #Determine Weights
              ##classify with emClassify or classifyUnsup as a pre-requisite
              ##rpairs.22 <- emWeights(rpairs.22, cutoff=0.7)
              #rpairs.22 <- emClassify(rpairs.22)
              #model.22 <- trainSupv(results.11, method = "rpart", use.pred=TRUE)
              
              #======================================================================
              
              #Unsupervised Classification using epiClassify
              rpairs.33 <- epiWeights(rpairs.33, e=0.01, f =rpairs.33$frequencies)
              
              #If upper threshold is not given, it will be taken as 0.75
              #If lower threshold is not given, it will default to upper threshold
              
              .threshold.up <- if(is.null(.threshold.up)) 0.75 else .threshold.up
              .threshold.lo <- if(is.null(.threshold.lo)) .threshold.lo else .threshold.up
              
              #results.33 <- epiClassify(rpairs.33,0.75)  # .threshold.up,.threshold.lo)
              results.33 <- epiClassify(rpairs.33,.threshold.up,.threshold.lo)  
              #summary(results.33)
              
              
              model.33 <- trainSupv(results.33, method = .st.method, use.pred=TRUE) # Training method is passed as argument
              class.results.33 <- classifySupv(model.33, newdata = rpairs.33)
              #summary(class.results.33)
              
              
              #============================================================  Quality
              
              #data quality = record.count / record.count + duplicate records
              data.quality <- round(.ds.count/record.count *100,2)
              #summary(result)
              
              # Recording Results 
              #===================================================================
              
              
              QS.results[i, 1] <- i
              QS.results[i, 2] <- record.count
              QS.results[i, 3] <- nrow(dupli.ds)
              
              QS.results[i, 4] <- nrow(non.dupli.ds)
              QS.results[i, 5] <- nrow(results.33$pairs)
              QS.results[i, 6] <- data.quality
              QS.results[i, 7] <- summary(results.33$prediction)[3]
              QS.results[i, 8] <- round(summary(results.33$prediction)[3]/nrow(dupli.ds)*100,2)
              QS.results[i, 9] <- summary(class.results.33$prediction)[3]
              QS.results[i, 10] <- round(summary(class.results.33$prediction)[3]/nrow(dupli.ds)*100,2)
              QS.results[i, 11]  <- summary(min.train.ds$prediction)[3]
              QS.results[i, 12]  <- round(summary(min.train.ds$prediction)[3]/nrow(dupli.ds)*100,2)
              QS.results[i, 13] <- count.train
      
      }
      
      #remove(dupli.ds,non.dupli.ds,ds.00,count.train)

QS.results

}

#=========================================================================
