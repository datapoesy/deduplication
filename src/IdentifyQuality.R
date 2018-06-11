
#Setup the environment
setwd("F:/GitHub/deduplication/src")
source("config/libraries.R")
source("lib/helpers.R")

## Read the sanitized dataset

# modified.count <- c(1,2,3,4,5)
modified.count <- c(1,2,3,4,5)
qual.results <- as.data.frame(matrix(0,ncol=6,nrow=1))
colnames(qual.results) <- c("No.","Orig.Records","Duplicates","Modified","Dupe.Perc","Quality")


for(m in 1:length(modified.count)){
  
  
    modified.fn <- paste("data/Modified",modified.count[m],".csv",sep="")
    ds.00 <- read.csv(file=modified.fn, stringsAsFactors = TRUE)
        
    
    #subset records associated with couples, twins, and neighbours
    ds.00.C <- subset(ds.00, grepl('C',ds.00$recoid)) 
    ds.00.T <- subset(ds.00, grepl('T',ds.00$recoid))
    ds.00.N <- subset(ds.00, grepl('N',ds.00$recoid))
    ds.00.D <- subset(ds.00, grepl('D',ds.00$recoid))
    
    #remove the above subsets from the sample keeping only duplicates
    unmatched <- anti_join(ds.00, ds.00.C)
    unmatched <- anti_join(unmatched, ds.00.T)
    unmatched <- anti_join(unmatched, ds.00.N)
    
    #Remove the D and identify the duplicates
    if(length(gsub(" D", "\\", unmatched$recoid))!=0){
    unmatched$recoid <- gsub(" D", "\\", unmatched$recoid)
    unmatched$recoid <- trimws(unmatched$recoid, which = c("both"))
    }
    orig.dupes <- nrow(unmatched[duplicated(unmatched$recoid), ])
    orig.records <- nrow(ds.00)
    dupe.percentage <- (orig.dupes/orig.records)*100
    quality <- 100-dupe.percentage
    
    alt.ds.count <- nrow(ds.00.C)+nrow(ds.00.T)+nrow(ds.00.N)+nrow(ds.00.D)
  
    # colnames(qual.results) <- c("No.","Orig.Records","Duplicates","Modified","Dupe.Perc","Quality")
    

    qual.results[m, 1] <- m
    qual.results[m, 2] <- orig.records
    qual.results[m, 3] <- orig.dupes
    qual.results[m, 4] <- alt.ds.count
    
    qual.results[m, 5] <- dupe.percentage
    qual.results[m, 6] <- quality
    
    #Save the results file
   qual.results$Quality <- round(qual.results$Quality,0)

}
qual.results.fn <- paste("data/DataQuality.csv",sep="")
write.csv(qual.results, file=qual.results.fn )
