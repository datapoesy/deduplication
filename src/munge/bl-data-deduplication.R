#===============  loading libraries ====================


setwd("F:/GitHub/deduplication/src")
wd <- getwd()
setwd("..")
parent <- getwd()
setwd(wd)
source("config/libraries.R")
source("lib/helpers.R")



options(fftempdir = "D:/Assembla/deduplication/src/data/temp")


#hhp <- read.table.ffdf(file="data/ds.00.csv",quote = "", FUN = "read.csv", na.strings = "",sep=",", colClasses=xclass) 

xclass <- c("factor","factor","factor","factor","factor",
            "factor","factor","factor","factor","factor",
            "factor","factor","factor","factor","factor",
            "integer","integer","integer")

#df to store information on data processing
#ds.rds.data <- as.data.frame(matrix(0,ncol=4,nrow=1))
#colnames(ds.rds.data) <- c("No.","record.count","file.name","time.taken")

#base.ds <- read.csv(file="data/ds.00.csv", colClasses = xclass,  stringsAsFactors = FALSE) # read from csv

it.count <- c(20,40,60,80,100,120)

i <- 1


  ## Set the count of records in the dataset
  ds.count <- it.count[i]*1000
  
  ## Sample dataset from the original 200K records
  ds.00 <-  base.ds[sample(nrow(base.ds),ds.count),]
  
  ds.00=as.data.frame(ds.00, stringsAsFactors=TRUE)
  write.csv(ds.00,file=str_c("ff-based/ds.00-",it.count[i],"k.csv")) # write the df as csv
  
  ds.00=as.ffdf(ds.00) # convert the df to ffdf
  
  colnames(ds.00) <- c("X_recoid_","X_someid_","X_firstname_","X_lastname_","X_gender_",
                       "X_birth_date_","X_email_","X_mnumber_","X_hnumber_","X_addr1_",
                       "X_addr2_","X_cityname_","X_postcode_","X_county_","X_nis_",
                       "X_b_year_","X_b_month_","X_b_day_")
  
  depup.st.time <- Sys.time()
  
  #Creating comparison vectors with in-memory comparison
  rpairs.00 <- RLBigDataDedup(ds.00,blockfld = list(4,c(16,17,18)),phonetic = c(3,4),exclude = c("X_recoid_",
                                                        "X_someid_", "X_birth_date_","X_mnumber_","X_hnumber_","X_gender_"))
 
  rds.dest.file <- str_c("ff-based/rpairs.00-",it.count[i],"k.rds")
  
 
  save.ffdf(rpairs.00, dir=str_c("ff-based/ffdb-",it.count[i],"k.rds"),overwrite=TRUE)
  
 # rpairs.00 <- load.ffdf(dir=str_c("ff-based/ffdb-",it.count[i],"k.rds"))
  
  ds.rds.data[i,1] <- i
  ds.rds.data[i,2] <- ds.count
  ds.rds.data[i,3] <- rds.dest.file
  ds.rds.data[i,4] <- Sys.time() - depup.st.time  # current time - start time
  
  #rpairs.00 <- readRDS(file=str_c("ff-based/rpairs.00-",it.count[i],"k.rds"))
  
 # write.csv(ds.rds.data,file="ff-based/dataset-info.csv")
#=============================================================================================
#Prepare Training Set -  Will not create if the training set exists
#=============================================================================================

destfile="ff-based/minTrain-usertrained-80k-nogender.rds"
  
  minTrain.00 <- NULL

if(file.exists(destfile)){
  minTrain.00 <- readRDS(file=destfile)
  
}else{
  #Get Minimal Training Pairs
  minTrain.00 <- getMinimalTrain(rpairs = rpairs.00, nEx = 1)

  #edit the pairs inline
  #minTrain.00 <- editMatch(minTrain.00)

  editable.pairs <- getPairs(minTrain.00) # Get pairs to export to excel
  editable.pairs$is_match <- "" # Add an is_match column for user entry
  write.csv(editable.pairs,file= str_c("ff-based/user-trained-pairs-",it.count[i],"k.csv"), row.names = FALSE )

  edited.pairs <- read.csv(file= str_c("ff-based/user-trained-pairs-",it.count[i],"k.csv"), stringsAsFactors=FALSE )
  edited.pairs <- edited.pairs[!apply(is.na(edited.pairs) | edited.pairs == "", 1, all),] # The blank line that separates rows is removed

  edited.pairs <- edited.pairs[,c("id","is_match")] #  the relevant columns from the edited excel file

  is.match.ds <- as.data.frame(matrix(NA,ncol=3,nrow=1))  #create dataframe to take input from excel
  colnames(is.match.ds) <- c("id1", "id2","is_match")

  #Take only the relevant columns from user input
  for(i in 1:nrow(edited.pairs)){
    alt.row = i+1
    is.match.ds[i,1] <- edited.pairs[i,1]
    is.match.ds[i,2] <- edited.pairs[alt.row,1]
    is.match.ds[i,3] <- edited.pairs[i,2]
  }
  is.match.ds <- is.match.ds[complete.cases(is.match.ds), ]

  #Load the trained set into the engine
  for(i in 1:nrow(is.match.ds)){
    if(minTrain.00$pairs[i,1]==is.match.ds[i,1] && minTrain.00$pairs[i,2]== is.match.ds[i,2]){
      minTrain.00$pairs[i,c("is_match")] <- is.match.ds[i,c("is_match")]
    }
  }

  #save minTrain as RDS
  saveRDS(minTrain.00,file=destfile)
  
}

#===================================================================================================
###Supervised clasification with Minimal Training Set
#===================================================================================================

count.train <- nrow(minTrain.00$pairs)  
  rpairs.00 <- epiWeights(rpairs.00, e=0.01, f =getFrequencies(rpairs.00))
model.00 <- trainSupv(minTrain.00, method = "rpart",minsplit=1) # Training method is passed as argument
results.00 <- classifySupv(model.00, newdata = rpairs.00)
fr <- summary(results.00)

# Recording Results 
#------------------------------------------------------------------------------------------------

#cl.results.00 <- as.data.frame(matrix(0,ncol=9,nrow=1))

#colnames(cl.results.00) <- c("No.","Total.Records","Record.Pairs","Ident.Pairs","Real.Pairs","Quality.Perc","Train.Count","Error")

cl.results.00[i, 1] <- i
cl.results.00[i, 2] <- ds.count
cl.results.00[i, 3] <- fr$nPairs
cl.results.00[i, 4] <- fr$nLinks
cl.results.00[i, 5] <- "0"
cl.results.00[i, 6] <- "0"
cl.results.00[i, 7]  <- nrow(minTrain.00$pairs)
cl.results.00[i, 8]  <- "0"

#write results to a file
write.csv(cl.results.00,file=str_c("ff-based/cl-results-",it.count[i],"k.csv"))

#write identified pairs to a csv file
ident.pairs.00 <- getPairs(results.00,filter.link = "link")
write.csv(ident.pairs.00, file= str_c("ff-based/ident-pairs-",it.count[i],"k.csv"))

#=================================================================================================

#results.all.exp <- runAllExpTrainedClassifier()
#write.csv(results.all.exp, file="results/results.all.exp.csv")



