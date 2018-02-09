

#makes a particular field a blank string
# GRANT ALL PRIVILEGES ON TABLE deduptest TO dddev;

make_blank = function(.p,.field){
  
  
  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  .con <- dbConnect(drv, dbname = "dedup", host = "localhost", port = 5432, user = "dddev", password = "secret")

  start.time <- Sys.time()
  
  # query the data from postgreSQL 
  .dbs.record.id <- dbGetQuery(.con, "SELECT userid from dbstable")
  
  #Get the defined percentage of records as random samples
  .tmp.dbs.id <- sample(nrow(.dbs.record.id)*.p/100)
  
  for(i in 1: length(.tmp.dbs.id)){
    
    switch(.field,
           email    = {update.txt <- paste("UPDATE dbstable SET email='' where userid =",.tmp.dbs.id[i])},
           hphone   = {update.txt <- paste("UPDATE dbstable SET homenumber= '' where userid =",.tmp.dbs.id[i])},
           mphone   = {update.txt <- paste("UPDATE dbstable SET mobilenumber= '' where userid =",.tmp.dbs.id[i])},
           postcode = {update.txt <- paste("UPDATE dbstable SET postcode='' where userid =",.tmp.dbs.id[i])},
           address1 = {update.txt <- paste("UPDATE dbstable SET address1='' where userid =",.tmp.dbs.id[i])},
           address2 = {update.txt <- paste("UPDATE dbstable SET address2='' where userid =",.tmp.dbs.id[i])},
           county   = {update.txt <- paste("UPDATE dbstable SET county='' where userid =",.tmp.dbs.id[i])},
           cityname = {update.txt <- paste("UPDATE dbstable SET cityname='' where userid =",.tmp.dbs.id[i])}  )
    
   
  
  }
  dbSendStatement(.con, update.txt)
  dbDisconnect(.con)
  
  end.time <- Sys.time()
  
  return(end.time - start.time)
  
}


#Function to clone a person -------------------------------

clonePerson = function(.p){
  #set.seed(123)

  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  .con <- dbConnect(drv, dbname = "dedup", host = "localhost", port = 5432, user = "dddev", password = "secret")
  
  start.time <- Sys.time()
  
  # query the data from postgreSQL 
  .dbs.record.id <- dbGetQuery(.con, "SELECT userid from dbstable")
  record.count <- round(nrow(.dbs.record.id) *.p/100,0)
  
  #Get the defined percentage of records as random samples
  .tmp.dbs.id <-  as.data.frame(.dbs.record.id[sample(record.count),])
 
 
  
  for(i in 1: nrow(.tmp.dbs.id)){
    ## Take values from .temp dataframe
    firstRow <- dbGetQuery(.con, paste("SELECT * from dbstable where userid=",.tmp.dbs.id[i,1]))
    
    firstRow$firstname <- gsub('(a|e|i|o|u){1,}$','',firstRow$firstname) # Remove trailing vowel sounds
    firstRow$address1 <- gsub('(A|E|I|O|U){1,}','',firstRow$address1)# Remove beginning vowels
    firstRow$someid  <- paste( sample( 1:9, 18, replace=TRUE ), collapse="" ) # creating a new cust id
    firstRow$recoid <- paste(.tmp.dbs.id[i,1],"D")
    .dbs.record.id <- .dbs.record.id[!.dbs.record.id == .tmp.dbs.id[i,1]]
    dbWriteTable(.con, c("public","dupli"), value=firstRow,append=TRUE, row.names=FALSE,overwrite=FALSE)
   
  }
  
 
  #Get the defined percentage of records as random samples
  .tmp.dbs.id <-  as.data.frame(.dbs.record.id[sample(record.count)])

  for(i in 1:nrow(.tmp.dbs.id)){
    ## Take values from .temp dataframe
    firstRow <- dbGetQuery(.con, paste("SELECT * from dbstable where userid=",.tmp.dbs.id[i,1]))
    
    firstRow$lastname <- gsub('(a|e|i|o|u){1,}$','',firstRow$lastname) # Remove trailing vowel sounds
    firstRow$gender <- ''# Remove gender Name
    firstRow$someid  <- paste( sample( 1:9, 18, replace=TRUE ), collapse="" ) # creating a new cust id
    firstRow$recoid <- paste(.tmp.dbs.id[i,1],"D")
    
    .dbs.record.id <- .dbs.record.id[!.dbs.record.id == .tmp.dbs.id[i,1]]
    dbWriteTable(.con, c("public","dupli"), value=firstRow,append=TRUE, row.names=FALSE,overwrite=FALSE)
  }
  
  
 
  #Get the defined percentage of records as random samples
  .tmp.dbs.id <-  as.data.frame(.dbs.record.id[sample(record.count)])
  
  for(i in 1:length(.tmp.dbs.id)){
    ## Take values from .temp dataframe
    firstRow <- dbGetQuery(.con, paste("SELECT * from dbstable where userid=",.tmp.dbs.id[i,1]))
 
    firstRow$lastname <- gsub('(t|e|i|o|z){1,}$','',firstRow$lastname) # Remove trailing vowel soundsds
    firstRow$county <- ''# Remove county
    firstRow$someid  <- paste( sample( 1:9, 18, replace=TRUE ), collapse="" ) # creating a new cust id
    firstRow$recoid <- paste(.tmp.dbs.id[i,1],"D")
    .dbs.record.id <- .dbs.record.id[!.dbs.record.id == .tmp.dbs.id[i,1]]
    dbWriteTable(.con, c("public","dupli"), value=firstRow,append=TRUE, row.names=FALSE,overwrite=FALSE)
  }
  
  
  #Get the defined percentage of records as random samples
  .tmp.dbs.id <-  as.data.frame(.dbs.record.id[sample(record.count)])
  
  for(i in 1:length(.tmp.dbs.id)){
    ## Take values from .temp dataframe
    firstRow <- dbGetQuery(.con, paste("SELECT * from dbstable where userid=",.tmp.dbs.id[i,1]))
    
    firstRow$lastname <- gsub('(t|e|i|o|z){1,}$','',firstRow$lastname) # Remove trailing vowel soundsds
    firstRow$postcode <- ''# Remove postcode
    firstRow$someid  <- paste( sample( 1:9, 18, replace=TRUE ), collapse="" ) # creating a new cust id
    firstRow$recoid <- paste(.tmp.dbs.id[i,1],"D")
    .dbs.record.id <- .dbs.record.id[!.dbs.record.id == .tmp.dbs.id[i,1]]
    dbWriteTable(.con, c("public","dupli"), value=firstRow,append=TRUE, row.names=FALSE,overwrite=FALSE)
  }
  
  
  #Get the defined percentage of records as random samples
  .tmp.dbs.id <-  as.data.frame(.dbs.record.id[sample(record.count)])
  
  for(i in 1:nrow(.tmp.dbs.id)){
    ## Take values from .temp dataframe
    firstRow <- dbGetQuery(.con, paste("SELECT * from dbstable where userid=",.tmp.dbs.id[i,1]))
    
    firstRow$postcode <- '' # Remove postcode
    firstRow$nis <- ''
    firstRow$someid  <- paste( sample( 1:9, 18, replace=TRUE ), collapse="" ) # creating a new cust id
    firstRow$recoid <- paste(.tmp.dbs.id[i,1],"D")
    .dbs.record.id <- .dbs.record.id[!.dbs.record.id == .tmp.dbs.id[i,1]]
    dbWriteTable(.con, c("public","dupli"), value=firstRow,append=TRUE, row.names=FALSE,overwrite=FALSE)
  }
  
  
  #Get the defined percentage of records as random samples
  .tmp.dbs.id <-  as.data.frame(.dbs.record.id[sample(record.count)])
  
  for(i in 1:nrow(.tmp.dbs.id)){
    ## Take values from .temp dataframe
    firstRow <- dbGetQuery(.con, paste("SELECT * from dbstable where userid=",.tmp.dbs.id[i,1]))
    
    firstRow$gender <- ''# Remove gender Name
    firstRow$dob_yy <- '' # Remove birth year
    firstRow$dob_mm <- '' # Remove birth month
    firstRow$dob_dd <- '' # Remove birth day
    firstRow$someid  <- paste( sample( 1:9, 18, replace=TRUE ), collapse="" ) # creating a new cust id
    firstRow$recoid <- paste(.tmp.dbs.id[i,1],"D")
    .dbs.record.id <- .dbs.record.id[!.dbs.record.id == .tmp.dbs.id[i,1]]
    dbWriteTable(.con, c("public","dupli"), value=firstRow,append=TRUE, row.names=FALSE,overwrite=FALSE)
  }
  dbDisconnect(.con)

}


#Function to create twins living in the same address with the same last name

createTwins = function(p){
 
  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  .con <- dbConnect(drv, dbname = "dedup", host = "localhost", port = 5432, user = "dddev", password = "secret")
  
  start.time <- Sys.time()
  
  # query the data from postgreSQL 
  .dbs.record.id <- dbGetQuery(.con, "SELECT userid from dbstable")
  record.count <- round(nrow(.dbs.record.id) *.p/100,0)
  
  #Get the defined percentage of records as random samples
  .tmp.dbs.id <-  as.data.frame(.dbs.record.id[sample(record.count),])
 
  for(i in 1:nrow(.tmp.dbs.id)){
    ## Take values from .temp dataframe
    firstRow <- dbGetQuery(.con, paste("SELECT * from dbstable where userid=",.tmp.dbs.id[i,1]))
 
    firstRow$firstname <- randomNames(1, which.names="first") ## Returns 5 first names
    firstRow$someid  <- paste( sample( 1:9, 18, replace=TRUE ), collapse="" ) # creating a new cust id
    firstRow$email <- r_email_addresses(1)
    firstRow$mobilenumber <- r_phone_numbers(1)
    nis.number <- paste( sample( 1:9, 7, replace=TRUE ), collapse="" )
    firstRow$nis <-  gsub("[[:space:]]", "", paste("NN",nis.number))# AA8371307
    firstRow$recoid <- paste(.tmp.dbs.id[i,1],"T")
    .dbs.record.id <- .dbs.record.id[!.dbs.record.id == .tmp.dbs.id[i,1]]
    dbWriteTable(.con, c("public","nondupli"), value=firstRow,append=TRUE, row.names=FALSE,overwrite=FALSE)
    
  }
  dbDisconnect(.con)
}



#Function to create couples living in the same address with the same last name

createCouples = function(.p){

  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  .con <- dbConnect(drv, dbname = "dedup", host = "localhost", port = 5432, user = "dddev", password = "secret")
  
  start.time <- Sys.time()
  
  # query the data from postgreSQL 
  .dbs.record.id <- dbGetQuery(.con, "SELECT userid from dbstable")
  record.count <- round(nrow(.dbs.record.id) *.p/100,0)
  
  #Get the defined percentage of records as random samples
  .tmp.dbs.id <-  as.data.frame(.dbs.record.id[sample(record.count),])
  
  for(i in 1:nrow(.tmp.dbs.id)){
    ## Take values from .temp dataframe
    firstRow <- dbGetQuery(.con, paste("SELECT * from dbstable where userid=",.tmp.dbs.id[i,1]))
    
    firstRow$firstname <- randomNames(1, which.names="first") ## Returns  first names
    firstRow$someid  <- paste( sample( 1:9, 18, replace=TRUE ), collapse="" ) # creating a new cust id
    firstRow$gender <- if(firstRow$gender=="male") "female" else "male"
    firstRow$dob_mm <- sample(1:12,1) # Take a random month of birth
    firstRow$dob_dd <- sample(1:28,1) # Take a random month of birth
    firstRow$dob_yy <- as.integer(firstRow$dob_yy) - sample(1:6,1) # Take a random month of birth
    firstRow$mobilenumber <- r_phone_numbers(1)
    firstRow$email <- r_email_addresses(1)
    firstRow$recoid <- paste(.tmp.dbs.id[i,1],"C")
    .dbs.record.id <- .dbs.record.id[!.dbs.record.id == .tmp.dbs.id[i,1]]
    dbWriteTable(.con, c("public","nondupli"), value=firstRow,append=TRUE, row.names=FALSE,overwrite=FALSE)
  }
  dbDisconnect(.con)
 
}


makeTypos = function(old,new,.p){
  
  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  .con <- dbConnect(drv, dbname = "dedup", host = "localhost", port = 5432, user = "dddev", password = "secret")
  
  start.time <- Sys.time()
  
  
  # query the data from postgreSQL 
  .dbs.record.id <- dbGetQuery(.con, "SELECT userid from dbstable")
  record.count <- round(nrow(.dbs.record.id) *.p/100,0)
  
  #Get the defined percentage of records as random samples
  .tmp.dbs.id <-  as.data.frame(.dbs.record.id[sample(record.count),])
  

  for(i in 1: nrow(.tmp.dbs.id)){
    ## Take values from .temp dataframe
    firstRow <- dbGetQuery(.con, paste("SELECT * from dbstable where userid=",.tmp.dbs.id[i,1]))
    
    firstRow <- str_replace_all(firstRow,old,new)
    firstRow <- str_replace_all(firstRow,"'","''")
    
    update.txt.fn <- paste("UPDATE dbstable SET firstname='",firstRow[4], "' where userid =",firstRow[1])
    update.txt.ln <- paste("UPDATE dbstable SET lastname='",firstRow[5], "' where userid =",firstRow[1])
    update.txt.ad <- paste("UPDATE dbstable SET address1='",firstRow[13], "' where userid =",firstRow[1])


    dbSendStatement(.con, update.txt.fn)
    dbSendStatement(.con, update.txt.ln)
    dbSendStatement(.con, update.txt.ad)
 
  }
  
  dbDisconnect(.con)
  return(end.time - start.time)

}


#
#
## This script file iterates from the already created datasets
## "record.id","some.id", "birth.date","m.nmbr","h.nmbr"

# Function to iterate all experiments for Unsupervised classification      
runAllExpUnsupervised <- function( ){ # for supervised training
  
  
  rds.pairs.tracker <- read.csv(file="rds.pairs.tracker.csv", stringsAsFactors=FALSE)
  
  
  classfn.results.parent <- as.data.frame(matrix(0,ncol=9,nrow=0))
  
  colnames(classfn.results.parent) <- c("No.","Total.Records","Duplicates",
                                        "Non.Duplicates","Record.Pairs","Quality.Perc",
                                        "Us.N","Us.P","Us.L")
  comp.pairs <- NULL
  
  #-----------------------------------------------------------------
  pb = txtProgressBar(min = 0, max = 5, initial = 0)
  perc.value <- c(1, 2, 4, 8, 16)
  
  
  training.set <- readRDS(file="TrainSet_1000.rds")
  opt.threshold <- optimalThreshold(training.set)
  
  for(j in 1:6){   # This refers to the iterations
    
    classfn.results <- as.data.frame(matrix(0,ncol=9,nrow=0))
    
    colnames(classfn.results) <- c("No.","Total.Records","Duplicates",
                                   "Non.Duplicates","Record.Pairs","Quality.Perc",
                                   "Us.N","Us.P","Us.L")
    for( i in 1:5){
      
      q.rating <- perc.value[i] 
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
      
      
    }
    
    classfn.results.parent <- rbind(classfn.results.parent,classfn.results)
  }
  classfn.results.parent
  
}


#==========================================================================
#==========================================================================



runAllExpTrainedClassifier <- function(){ # for supervised training
  
  setwd("D:/Assembla/deduplication/src")
  classfn.results.parent <- as.data.frame(matrix(0,ncol=11,nrow=0))
  
  
  rds.pairs.tracker <- read.csv(file="rds.pairs.tracker.csv",stringsAsFactors=FALSE)
  
  
  colnames(classfn.results.parent) <- c("No.","Total.Records","Duplicates",
                                        "Non.Duplicates","Record.Pairs","Quality.Perc",
                                        "Tc.N","Tc.P","Tc.L","Accuracy","Error")
  
  
  #-----------------------------------------------------------------
  pb = txtProgressBar(min = 0, max = 5, initial = 0)
  perc.value <- c(1, 2, 4, 8, 16)
  
  for(j in 1:6){   # This refers to quality specified across the columns
    
    classfn.results <- as.data.frame(matrix(0,ncol=9,nrow=0))
    
    colnames(classfn.results) <- c("No.","Total.Records","Duplicates",
                                   "Non.Duplicates","Record.Pairs","Quality.Perc","Ident.Pairs","Accuracy","Error")
    
    
    for(i in 1:5){
      .q.rating <- perc.value[i]
      
      #reading the saved data pairs into memory
      current.rpairs <- readRDS(file=rds.pairs.tracker[rds.pairs.tracker[, "Iteration.No"]==j & rds.pairs.tracker[, "Quality.Rating"]==.q.rating,]$Rds.Filename)
      # minTrain.00 <- readRDS(file=rds.pairs.tracker[rds.pairs.tracker[, "Iteration.No"]==1 & rds.pairs.tracker[, "Quality.Rating"]==.q.rating,]$Train.Set)
      minTrain.00 <- readRDS(file="minTrain.00Pairs20000.rds")
      
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
      classfn.results[i, 7] <- summary(results.22$prediction)[3]
      
      classfn.results[i, 8] <- round(summary(results.22$prediction)[3]/dupli.ds*100,2)
      classfn.results[i, 9] <- (100-(round(summary(results.22$prediction)[3]/dupli.ds*100,2)))
      
      
    }
    classfn.results.parent <- rbind(classfn.results.parent,classfn.results)
  }
  
  classfn.results.parent
  
}

#======================================================