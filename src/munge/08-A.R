##
##
## This script file creates paired records from comparison patterns
## excluding these records "record.id","some.id", "birth.date","m.nmbr","h.nmbr"
## The pairs are created only if two records match either on the lastnames or on the complete date of birth
## These are saved as object files for easy retrieval and classification in real-time
## rds.pairs.tracker maintains a mapping records of the object files with corresponding Train files and other parameters
## such as number of duplicate records, quality of the dataset etc.

setwd("F:/GitHub/deduplication/src")
wd <- getwd()
setwd("..")
parent <- getwd()
setwd(wd)
source("munge/01-A.R")


rds.pairs.tracker <- createRPairs(base.ds)

write.csv(rds.pairs.tracker, file="rdscsvfiles/rds.pairs.tracker.csv")  # returns the parent rds tracker

    
# Function to iterate the experiment      
createRPairs <- function(.base.ds){
     
      pb = txtProgressBar(min = 0, max = 6, initial = 0)
      pb1 = txtProgressBar(min = 0, max = 5, initial = 0)
      perc.value <- c(1, 2, 4, 8, 16)
      
      rds.tracker.parent <-  as.data.frame(matrix(0,ncol=9,nrow=6))
      colnames(rds.tracker.parent) <-  c("Iteration.No","Quality.Rating","Rds.Filename","Total.Records","Duplicates","Non.Duplicates",
                                        "Record.Pairs","Quality.Perc","Train.Set")
      
    
      for(j in 1:6){
        
        setTxtProgressBar(pb,j)
        rds.tracker.child <-  as.data.frame(matrix(0,ncol=9,nrow=6))
        colnames(rds.tracker.child) <-  c("Iteration.No","Quality.Rating","Rds.Filename","Total.Records","Duplicates","Non.Duplicates",
                                          "Record.Pairs","Quality.Perc","Train.Set")
        
              for( i in 1:5){
                
                   
                      set.seed(1234)
                      
                      .ds.count <- j* 5000
                      ## Set the count of records in the dataset
                      
                      ## Sample dataset from the original 100K records
                      ds.00 <-  base.ds[sample(nrow(base.ds),.ds.count),]
                      
                     
                      ## Remove the original 100K dataset to conserve memory
                      #remove(base.ds)
                      
                      ## Set a progress bar
                      setTxtProgressBar(pb,i)
                      
                      ## set a percentage value for data to be disorganized
                      p <- i
                      
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
                      ds.00$h.nmbr[sample(nrow(ds.00), nrow(ds.00) * 3 / 100)] <- ''
                      
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
                      dupli.ds <- rbind(dupli.ds, temp.ds$clones)
                      
                      dupli.ds <- dupli.ds[!duplicated(dupli.ds$record.id),]
                      
                      
                      
                      ###Duplicate Records for identical Twins living in the same address,
                      ## also pass the list of record ids that has been used.
                      pt <- 0.05*p
                      
                      temp.ds <- createTwins(temp.ds$ds.99, pt)
                      non.dupli.ds <- rbind(non.dupli.ds, temp.ds$twins) # Non-duplicates means individual people
                      non.dupli.ds <- non.dupli.ds[!duplicated(non.dupli.ds$record.id),]
                      
                      
                      ###Duplicate Records for Married couples living in the same address
                      # % of couples in the loan availing population = 50%
                      # % of couples living in the same address = 80%
                      # Combined percentage of the above = 40%
                      pc <- 4 * p
                      temp.ds <- createCouples(temp.ds$ds.99, pc)
                      non.dupli.ds <- rbind(non.dupli.ds, temp.ds$couples)
                      
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
                   
                      # rpairs.00 <- compare.dedup(ds.00,blockfld = list(4,c(16,17,18)),phonetic = c(3,4,10,11,12,14),
                      #                            exclude = c("record.id","some.id", "birth.date","m.nmbr","h.nmbr"), 
                      #                            strcmp = c(5,8,9,13,15),
                      #                            strcmpfun = jarowinkler)
                      
                                            
                      #============  pre processing complete ===========================
                      
                      rds.filename <- str_c("ds.00_",.ds.count,"_qual",i,".rds")
                      tset.filename <- str_c("TrainSet_",.ds.count,".rds")
                      
                      #save the rpairs object to disc
                      if (!file.exists(rds.filename)){
                        saveRDS(ds.00, file= rds.filename) # Save the R trained pairs for retrieval
                      }
                      
                      #save duplicate file
                     # dupli.filename <- str_c("dupli_",.ds.count,"_qual",i,".rds")
                    #  saveRDS(dupli.ds, file= dupli.filename) # Save the R trained pairs for retrieval
                      dupli.filename <- str_c("rdscsvfiles/dupli_",.ds.count,"_qual",i,".csv")
                      write.csv(dupli.ds, file= dupli.filename)
                      
                      # Maintains a tracker to map all required information:
                      
                      rds.tracker.child[i,1] <- j
                      rds.tracker.child[i,2] <- perc.value[i]
                      rds.tracker.child[i,3] <- rds.filename
                      rds.tracker.child[i,4] <- record.count
                      rds.tracker.child[i,5] <- nrow(dupli.ds)
                      rds.tracker.child[i,6] <- nrow(non.dupli.ds)
                      rds.tracker.child[i,7] <- 
                      
                      rds.tracker.child[i,8] <- round(.ds.count/record.count *100,2)
                      rds.tracker.child[i,9] <- tset.filename
              }
        rds.tracker.parent <- rbind(rds.tracker.parent, rds.tracker.child)
        
        
      }
      rds.tracker.parent
}
