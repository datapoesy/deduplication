

## This is a function that will count the number of NAs in a particular column in a dataset
NAhunter<-function(dataset) { 
  find.NA<-function(variable) { 
    if(is.numeric(variable)){ 
      n<-length(variable) 
      mean<-mean(variable, na.rm=T) 
      median<-median(variable, na.rm=T) 
      sd<-sd(variable, na.rm=T) 
      NAs<-is.na(variable) 
      total.NA<-sum(NAs) 
      percent.missing<-total.NA/n 
      descriptives<-data.frame(n,mean,median,sd,total.NA,percent.missing) 
      rownames(descriptives)<-c(" ") 
      Case.Number<-1:n 
      Missing.Values<-ifelse(NAs>0,"Missing Value"," ") 
      missing.value<-data.frame(Case.Number,Missing.Values) 
      missing.values<-missing.value[ which(Missing.Values=='Missing Value'),] 
      list("NUMERIC DATA","DESCRIPTIVES"=t(descriptives)) 
    } else{ 
      n<-length(variable) 
      NAs<-is.na(variable) 
      total.NA<-sum(NAs) 
      percent.missing<-total.NA/n 
      descriptives<-data.frame(n,total.NA,percent.missing) 
      rownames(descriptives)<-c(" ") 
      Case.Number<-1:n 
      Missing.Values<-ifelse(NAs>0,"Missing Value"," ") 
      missing.value<-data.frame(Case.Number,Missing.Values) 
      missing.values<-missing.value[ which(Missing.Values=='Missing   
        Value'),] 
      list("CATEGORICAL DATA","DESCRIPTIVES"=t(descriptives)) 
    } 
  } 
  dataset<-data.frame(dataset) 
  options(scipen=100) 
  options(digits=2) 
  lapply(dataset,find.NA) 
}  





## Replaces NAs with the given string  ---------------------------------
NAreplace <- function(x, replString){  
    {
      x[is.na(x)] <- replString
      replace(x, is.na(x), replString) 
      return(x)
    }
}

# Create a confusion matrix from the given outcomes, whose rows correspond
# to the actual and the columns to the predicated classes.
createConfusionMatrix <- function(act, pred) {
  # You've mentioned that neither actual nor predicted may give a complete
  # picture of the available classes, hence:
  numClasses <- max(act, pred)
  # Sort predicted and actual as it simplifies what's next. You can make this
  # faster by storing `order(act)` in a temporary variable.
  pred <- pred[order(act)]
  act  <- act[order(act)]
  sapply(split(pred, act), tabulate, nbins=numClasses)
}


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## Function to Remove Outliers ----------------------------------------------------------
rem_outliers <- function(x, na.rm = TRUE,...){
  qnt <- quantile (x, probs=c(0.25, 0.75), na.rm = na.rm, ...)
  H <- 1.5* IQR (x, na.rm=na.rm)
  y <-x
  y[x<(qnt[1] - H)] <- mean(x)
  y[x > (qnt[2] + H)] <- mean(x)
  y
}



## Find outliers
findOutlier <- function(data, cutoff = 3) {
  ## Calculate the sd
  sds <- apply(data, 2, sd, na.rm = TRUE)
  ## Identify the cells with value greater than cutoff * sd (column wise)
  result <- mapply(function(d, s) {
    which(d > cutoff * s)
  }, data, sds)
  result
}

##Remove outliers by marking them as NA
removeOutlier <- function(data, outliers) {
  result <- mapply(function(d, o) {
    res <- d
    res[o] <- NA
    return(res)
  }, data, outliers)
  return(as.data.frame(result))
}


## Function to remove outliers for numerical columns only ------------------------------
remove_ol <- function (data){
  for (i in names(data)) {
    if(is.numeric(data[[i]])){
      data[[i]] <- rem_outliers(data[[i]])
    }
  }
  data
}

transform_lognormal <- function(data){
  for (i in names(data)) {
    if(is.numeric(data[[i]])){
      data[[i]] <- log(data[[i]])
    }
  }
  data
}


## Function to show most highly correlated data ---------------------------------
mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  # find the correlations
  cormatrix <- cor(mydataframe)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}


#Function to remove rows based on NA values in specific columns---------------------------------

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}


#Function to clone a person -------------------------------

clonePerson = function(.df, p){
  #set.seed(123)
  
  .result <- data.frame(matrix(0, ncol = ncol(.df), nrow = 0))
  colnames(.result) <- colnames(.df)
  .rs <- .result
  
  .temp <- (.df[sample(x=nrow(.df),size=p*nrow(.df)/100, replace = FALSE),]) # Percentage of rows
  
  #remove the used rows to avoid repeated selection
  .df <- .df[ !(.df$recoid %in% .temp$recoid), ]
  
  for(i in 1:nrow(.temp)){
    ## Take values from .temp dataframe
    firstRow =  .temp[i,]
    firstRow$firstname <- gsub('(a|e|i|o|u){1,}$','',.temp[i,c("firstname")]) # Remove trailing vowel sounds
    firstRow$addr1 <- gsub('(A|E|I|O|U){1,}','',.temp[i,c("addr1")])# Remove beginning vowels
    firstRow$someid  <- as.character(paste( sample( 1:9, 18, replace=TRUE ), collapse="" )) # creating a new cust id
    firstRow$recoid <- paste(.temp$recoid[i],"D")
    .rs[i,]<- firstRow
  }
  .result <- rbind(.result,.rs)
  
  .temp <- (.df[sample(x=nrow(.df),size=p*nrow(.df)/100, replace = FALSE),]) # Percentage of rows
  
  #remove the used rows to avoid repeated selection
  .df <- .df[ !(.df$recoid %in% .temp$recoid), ]
  
  for(i in 1:nrow(.temp)){
    ## Take values from .temp dataframe
    firstRow =  .temp[i,]
    firstRow$lastname <- gsub('(a|e|i|o|u){1,}$','',.temp[i,c("lastname")]) # Remove trailing vowel sounds
    firstRow$gender <- ''# Remove gender Name
    firstRow$someid  <- as.character(paste( sample( 1:9, 18, replace=TRUE ), collapse="" )) # creating a new cust id & convert to string
    firstRow$recoid <- paste(.temp$recoid[i],"D")
    .rs[i,]<- firstRow
  }
  .result <- rbind(.result,.rs)
 
  .temp <- (.df[sample(x=nrow(.df),size=p*nrow(.df)/100, replace = FALSE),]) # Percentage of rows
  
  #remove the used rows to avoid repeated selection
  .df <- .df[ !(.df$recoid %in% .temp$recoid), ]
  
  for(i in 1:nrow(.temp)){
    ## Take values from .temp dataframe
    firstRow =  .temp[i,]
    firstRow$lastname <- gsub('(t|e|i|o|z){1,}$','',.temp[i,c("lastname")]) # Remove trailing vowel sounds
    firstRow$county <- ''# Remove county
    firstRow$someid  <- as.character(paste( sample( 1:9, 18, replace=TRUE ), collapse="" )) # creating a new cust id & convert to string
    firstRow$recoid <- paste(.temp$recoid[i],"D")
    .rs[i,]<- firstRow
  }
  .result <- rbind(.result,.rs)
  
  
  .temp <- (.df[sample(x=nrow(.df),size=p*nrow(.df)/100, replace = FALSE),]) # Percentage of rows
  
  #remove the used rows to avoid repeated selection
  .df <- .df[ !(.df$recoid %in% .temp$recoid), ]
  
  for(i in 1:nrow(.temp)){
    ## Take values from .temp dataframe
    firstRow =  .temp[i,]
    firstRow$lastname <- gsub('(l|n|i){1,}$','',.temp[i,c("lastname")]) # Remove trailing vowel sounds
    firstRow$postcode <- '' # Remove postcode
    firstRow$someid  <- as.character(paste( sample( 1:9, 18, replace=TRUE ), collapse="" )) # creating a new cust id & convert to string
    firstRow$recoid <- paste(.temp$recoid[i],"D")
    .rs[i,]<- firstRow
  }
  .result <- rbind(.result,.rs)
 
  .temp <- (.df[sample(x=nrow(.df),size=p*nrow(.df)/100, replace = FALSE),]) # Percentage of rows
  
  #remove the used rows to avoid repeated selection
  .df <- .df[ !(.df$recoid %in% .temp$recoid), ]
  
  for(i in 1:nrow(.temp)){
    ## Take values from .temp dataframe
    firstRow =  .temp[i,]
    firstRow$postcode <- '' # Remove postcode
    firstRow$nis <- ''
    firstRow$someid  <- as.character(paste( sample( 1:9, 18, replace=TRUE ), collapse="" )) # creating a new cust id & convert to string
    
    firstRow$recoid <- paste(.temp$recoid[i],"D")
    .rs[i,]<- firstRow
  }
  .result <- rbind(.result,.rs)
 
  
  .temp <- (.df[sample(x=nrow(.df),size=p*nrow(.df)/100, replace = FALSE),]) # Percentage of rows
  
  #remove the used rows to avoid repeated selection
  .df <- .df[ !(.df$recoid %in% .temp$recoid), ]
  
  for(i in 1:nrow(.temp)){
    ## Take values from .temp dataframe
    firstRow =  .temp[i,]
    firstRow$gender <- ''# Remove gender Name
    firstRow$b.year <- '' # Remove birth year
    firstRow$b.month <- '' # Remove birth month
    firstRow$b.day <- '' # Remove birth day
    firstRow$someid  <- as.character(paste( sample( 1:9, 18, replace=TRUE ), collapse="" )) # creating a new cust id & convert to string
    firstRow$recoid <- paste(.temp$recoid[i],"D")
    .rs[i,]<- firstRow
  }
  .result <- rbind(.result,.rs)

 
  .newList <- list("ds.99" = .df, "clones" = .result)
  return (.newList)

}


#Function to create twins living in the same address with the same last name

createTwins = function(.df, p, .used.ids){
 
  .temp <- (.df[sample(x=nrow(.df),size=p*nrow(.df)/100, replace = FALSE),]) # Percentage of rows
  .result <- data.frame(matrix(0, ncol = ncol(.df), nrow = 0))
  colnames(.result) <- colnames(.df)
  .df <- .df[ !(.df$recoid %in% .temp$recoid), ]
 
  for(i in 1:nrow(.temp)){
    firstRow =  .temp[i,]
 
    firstRow$firstname <- .df$firstname[sample(nrow(.df),1)] # Take a random name from the firstname column
    firstRow$someid  <- as.character(paste( sample( 1:9, 18, replace=TRUE ), collapse="" )) # creating a new cust id & convert to string
    firstRow$email <- r_email_addresses(1)
    firstRow$mnumber <- r_phone_numbers(1)
    nis.number <- paste( sample( 1:9, 7, replace=TRUE ), collapse="" )
    firstRow$nis <-  gsub("[[:space:]]", "", paste("NN",nis.number))# AA8371307
    firstRow$recoid <- paste(.temp$recoid[i],"T")
    .result[i,]<- firstRow
    
  }
  .newList <- list("ds.99" = .df, "twins" = .result)
  return (.newList)
 
}


#Function to create neighbors living in the same address with the same last name

createNeighbors = function(.df, pn){
  
  .temp <- .df[sample(x=nrow(.df),size=round(pn*nrow(.df)/100,0), replace = FALSE),] # Percentage of rows
  .result <- data.frame(matrix(0, ncol = ncol(.df), nrow = 0))
  
  colnames(.result) <- colnames(.df)
  .df <- .df[ !(.df$recoid %in% .temp$recoid), ]
  

  for(i in 1:nrow(.temp)){
    ## Take values from .temp dataframe
    firstRow =  .temp[i,]
    firstRow$someid  <- as.character(paste( sample( 1:9, 18, replace=TRUE ), collapse="" )) # creating a new cust id & convert to string
    firstRow$b.month <- sample(x=.df$b.month,size=1) # Take a random month of birth
    firstRow$b.day <- sample(x=.df$b.day,size=1) # Take a random month of birth
    firstRow$b.year <- sample(x=.df$b.year,size=1) # Take a random month of birth
    firstRow$mnumber <- r_phone_numbers(1)
    firstRow$hnumber <- r_phone_numbers(1)
    firstRow$email <- r_email_addresses(1)
    firstRow$addr1 <- sample(x=.df$addr1,size=1, replace = FALSE)
    nis.number <- paste( sample( 1:9, 7, replace=TRUE ), collapse="" )
    firstRow$nis <-  gsub("[[:space:]]", "", paste("NN",nis.number))# AA8371307
    firstRow$recoid <- paste(.temp$recoid[i],"N")
    .result[i,]<- firstRow
  }

  .newList <- list("ds.99" = .df, "neighbors" = .result)
  return (.newList)
  
}



#Function to create couples living in the same address with the same last name

createCouples = function(.df, p){

  .temp <- .df[sample(x=nrow(.df),size=p*nrow(.df)/100, replace = FALSE),] # Percentage of rows
  .result <- data.frame(matrix(0, ncol = ncol(.df), nrow = 0))
  
  colnames(.result) <- colnames(.df)
  .df <- .df[ !(.df$recoid %in% .temp$recoid), ]
  
  for(i in 1:nrow(.temp)){
    firstRow =  .temp[i,]
    firstRow$firstname <- .df$firstname[sample(nrow(.df),1)] # Take a random name from the firstname column
    firstRow$someid  <- as.character(paste( sample( 1:9, 18, replace=TRUE ), collapse="" )) # creating a new cust id & convert to string
    firstRow$gender <- if(firstRow$gender=="male") "female" else "male"
    firstRow$b.month <- .df$b.month[sample(nrow(.df),1)] # Take a random month of birth
    firstRow$b.day <- .df$b.day[sample(nrow(.df),1)] # Take a random month of birth
    firstRow$b.year <- .df$b.year[sample(nrow(.df),1)] # Take a random month of birth
    firstRow$mnumber <- r_phone_numbers(1)
    firstRow$email <- r_email_addresses(1)
    firstRow$recoid <- paste(.temp$recoid[i],"C")
    .result[i,]<- firstRow
  }
 
  .newList <- list("ds.99" = .df, "couples" = .result)
  .newList
}


makeTypos = function(.df,old,new,p){
  
  test <- .df$recoid[sample(nrow(.df)*p/100)]

  for(i in 1: length(test)){
   
  .df[.df$recoid==test[i],] <- str_replace_all(.df[.df$recoid==test[i],],old,new)
  print(.df[.df$recoid==test[i],])
 
 }
return(.df)
  
}


#
#
## This script file iterates from the already created datasets
## "recoid","someid", "birth.date","mnumber","hnumber"

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
                                      exclude = c("recoid","someid", "birth.date"), n_match = 4000)
      
      
      
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