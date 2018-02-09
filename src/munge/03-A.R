setwd("F:/GitHub/deduplication/src")
wd <- getwd()
setwd("..")
parent <- getwd()
setwd(wd)
#source("munge/01-A.R")

## Set the count of records in the dataset

base.ds <- read.csv(file="rdscsvfiles/base.ds.csv", stringsAsFactors = FALSE)[,-c(1)]
desc.base.ds <- read.csv(file="rdscsvfiles/desc.base.ds.csv", stringsAsFactors = FALSE)[,-c(1)]
ds.count <- 5000

perc.value <- c(1, 2, 4, 8, 16)

i = 1

set.seed(1234)

## Sample dataset from the original 100K records
ds.00 <-  base.ds[sample(nrow(base.ds),ds.count),]


## set a percentage value for data to be disorganized
p <- 4
pt <- 0.05*p
pc <- 5*p

dupli.ds <- read.csv(file="rdscsvfiles/dupli.ds.5k.csv", stringsAsFactors = FALSE)[,-c(1)]
non.dupli.ds <- read.csv(file="rdscsvfiles/non.dupli.ds.5k.csv", stringsAsFactors = FALSE)[,-c(1)]


ds.00 <- read.csv(file="rdscsvfiles/ds.00.csv", stringsAsFactors = FALSE)[,-c(1)]

record.count <- nrow(ds.00)

#==================================================================
#This is for the report:

pre.process <-as.data.frame(matrix(ncol = 3, nrow = 0))
colnames(pre.process) <- c("Sl.No.","Change","Description")

pre.process[1,c(2:3)] <- c("Missing email ids", 
                           paste(p,"% of records updated to remove email ids"))

pre.process[2,c(2:3)] <- c("Missing home phone numbers", 
                           paste(p,"% of records updated to remove home phone numbers"))

pre.process[3,c(2:3)] <- c("Missing mobile phone numbers", 
                           paste(p,"% of records updated to remove mobile phone numbers"))

pre.process[4,c(2:3)] <- c("Missing post codes", 
                           paste(p,"% of records updated to remove post codes"))

pre.process[5,c(2:3)] <- c("Missing address 1 from address", 
                           paste(p,"% of records updated to remove address 1"))

pre.process[6,c(2:3)] <- c("Missing address 2", 
                           paste(p,"% of records updated to remove address 2"))

pre.process[7,c(2:3)] <- c("Missing county names", 
                           paste(p,"% of records updated to remove county names"))

pre.process[8,c(2:3)] <- c("Missing Cityname", 
                           paste(p,"% of records updated to remove Cityname"))

pre.process[9,c(2:3)] <- c("clone Records", 
                           paste(nrow(dupli.ds)," records cloned to represent duplicate records"))

pre.process[10,c(2:3)] <- c("Entities with same DoB and same address", 
                          paste(pt,"% records are created with same last name and a 
                          random first name from the dataset to simulate twins living in the same house.  
                          The record.id is tagged with 'T' to identify these records."))

pre.process[11,c(2:3)] <- c("Married couples living in the same address", 
                          paste(pc,"% records created for couples living in the same house:",
                          "The First Name is changed. " ,
                          "The gender is changed to the opposite of the originial record. " ,
                          "The record.id is tagged with 'C' to identify these records. "  ,
                          "The date, month and year in DoB is changed  to a random number. ",
                          "The firstname is changed to a random name. " ))


pre.process[12,c(2:3)] <- c("Altering name to simulate spelling mistakes", 
                          paste(pc,"% of the records with a particular name is 
                          changed to a new one to simulate spelling mistatakes:
                          'John' is changed to 'Jon'  
                         'Thompson' is changed to 'Thomson'  
                         'tt' of words changed to single 't'" ))

pre.process[12,c(2:3)] <- c("Clone Entities to create duplicate records 
                          to be potentially identified by the dedup engine.", 
                            paste(pc,"% of the original records created new.  These records are created to be 
                          duplicates and tagged with a 'D'. 
                          The records are cloned and subsequently,
                          20% of first names have their trailing vowel sounds removed. 
                          The last names are stripped of their beginning vowels
                          Replaced with a new 'some.id'.
                          20% of records are stripped of their email address, NIS, County names etc.
                          20% have their addresses modified.
                          20% have their postcodes removed.
                          20% have the gender information removed." ))


pre.process[,c(1)]<- c(1:nrow(pre.process))
colnames(pre.process) <- c("Sl.No","Scenario","Description")




#===================  dataset complete =========================



#Creating comparison vectors
rpairs.00 <- compare.dedup(ds.00,blockfld = list(4,c(16,17,18)),phonetic = c(3,4),
                           exclude = c("record.id","some.id", "birth.date"), n_match = 4000)
rpairs.11 <- rpairs.00
rpairs.22 <- rpairs.00
rpairs.33 <- rpairs.00
rpairs.00 <- rpairs.22
#============================================================

#Unsupervised classification with epiclassify

cl.results.11 <- as.data.frame(matrix(0,ncol=9,nrow=1))

colnames(cl.results.11) <- c("No.","Total.Records","Duplicates",
                             "Non.Duplicates","Record.Pairs","Quality.Perc",
                             "Ident.Pairs","Pairs.Perc"  )

results.11 <- epiWeights(rpairs.11, e=0.01, f =rpairs.11$frequencies)
summary(results.11)

results.11.dupli <- getPairs(results.11,1,0.85)

results.11.dupli.count <- nrow(results.11.dupli)

unsup.results.11 <- classifyUnsup(rpairs.11, method = "kmeans")


data.quality <- round(ds.count/record.count *100,2)

# Recording Results 
#===================================================================


cl.results.11[1, 1] <- 1
cl.results.11[1, 2] <- record.count
cl.results.11[1, 3] <- nrow(dupli.ds)

cl.results.11[1, 4] <- nrow(non.dupli.ds)
cl.results.11[1, 5] <- nrow(unsup.results.11$pairs)
cl.results.11[1, 6] <- data.quality
cl.results.11[1, 7] <- summary(unsup.results.11$prediction)[3]
cl.results.11[1, 8] <- round(summary(unsup.results.11$prediction)[3]/nrow(dupli.ds)*100,2)

write.csv(cl.results.11,file="cl.results.11.csv")


#=====================================================================

#Unsupervised classification using Optimal thresholds
#============================================================


cl.results.33 <- as.data.frame(matrix(0,ncol=9,nrow=1))

colnames(cl.results.33) <- c("No.","Total.Records","Duplicates",
                             "Non.Duplicates","Record.Pairs","Quality.Perc",
                             "Ident.Pairs","Pairs.Perc",
                             "Train.Count")

results.33 <- epiWeights(rpairs.33, e=0.01, f =rpairs.11$frequencies)
minTrain.33 <- readRDS(file="rdscsvfiles/minTrain.00Pairs20000.rds")
opt.threshold <- optimalThreshold(minTrain.33)

count.train <- nrow(minTrain.33$pairs)
unsup.results.33 <- epiClassify(results.33, opt.threshold)


results.33.dupli <- getPairs(unsup.results.33, show="links")
results.33.dupli.count <- nrow(results.33.dupli)

# Recording Results 
#===================================================================


cl.results.33[1, 1] <- 1
cl.results.33[1, 2] <- record.count
cl.results.33[1, 3] <- nrow(dupli.ds)

cl.results.33[1, 4] <- nrow(non.dupli.ds)
cl.results.33[1, 5] <- nrow(unsup.results.33$pairs)
cl.results.33[1, 6] <- data.quality
cl.results.33[1, 7] <- summary(unsup.results.33$prediction)[3]
cl.results.33[1, 8] <- round(summary(unsup.results.33$prediction)[3]/nrow(dupli.ds)*100,2)
cl.results.33[1, 9] <- count.train

write.csv(cl.results.33,file="cl.results.33.csv")


#=====================================================================
###Supervised clasification with Minimal Training Set
#=====================================================================

cl.results.00 <- as.data.frame(matrix(0,ncol=9,nrow=1))

colnames(cl.results.00) <- c("No.","Total.Records","Duplicates",
                             "Non.Duplicates","Record.Pairs","Quality.Perc",
                             "Ident.Pairs","Pairs.Perc",
                             "Train.Count")


#Reading manually trained data

minTrain.00 <- readRDS(file="rdscsvfiles/minTrain.00Pairs20000.rds")

count.train <- nrow(minTrain.00$pairs)
rpairs.00 <- epiWeights(rpairs.00, e=0.01, f =rpairs.00$frequencies)
model.00 <- trainSupv(minTrain.00, method = "rpart",minsplit=1) # Training method is passed as argument
results.00 <- classifySupv(model.00, newdata = rpairs.00)


data.quality <- round(ds.count/record.count *100,2)

# Recording Results 
#===================================================================


cl.results.00[1, 1] <- 1
cl.results.00[1, 2] <- record.count
cl.results.00[1, 3] <- nrow(dupli.ds)

cl.results.00[1, 4] <- nrow(non.dupli.ds)
cl.results.00[1, 5] <- nrow(results.00$pairs)
cl.results.00[1, 6] <- data.quality
cl.results.00[1, 7]  <- summary(results.00$prediction)[3]
cl.results.00[1, 8]  <- round(summary(results.00$prediction)[3]/nrow(dupli.ds)*100,2)
cl.results.00[1, 9] <- count.train

ident.pairs.00 <- getPairs(results.00,show="links")

write.csv(cl.results.00,file="cl.results.00.csv")
#============================================================================================
#============================================================================================


###Supervised clasification with trained classifier from Unsupervised Classification

cl.results.22 <- as.data.frame(matrix(0,ncol=9,nrow=1))

colnames(cl.results.22) <- c("No.","Total.Records","Duplicates",
                             "Non.Duplicates","Record.Pairs","Quality.Perc",
                             "Tc.L","Tc.Perc",
                             "Train.Count")

rpairs.22 <- epiWeights(rpairs.22)

min.train.22 <- readRDS(file="rdscsvfiles/minTrain.00Pairs20000.rds")

count.train <- nrow(min.train.22$pairs)
results.22 <- classifyUnsup(min.train.22, method = "kmeans")

data.quality <- round(ds.count/record.count *100,2)

model.22 <- trainSupv(min.train.22, method = "rpart",minsplit=1) # Training method is passed as argument
min.results.22 <- classifySupv(model.22, newdata = rpairs.22)
summary(min.results.22)

# Recording Results 
#===================================================================


cl.results.22[1, 1] <- 1
cl.results.22[1, 2] <- record.count
cl.results.22[1, 3] <- nrow(dupli.ds)

cl.results.22[1, 4] <- nrow(non.dupli.ds)
cl.results.22[1, 5] <- nrow(results.33$pairs)
cl.results.22[1, 6] <- data.quality
cl.results.22[1, 7]  <- summary(min.results.22$prediction)[3]
cl.results.22[1, 8]  <- round(summary(min.results.22$prediction)[3]/nrow(dupli.ds)*100,2)
cl.results.22[1, 9] <- count.train


#============================================================================================


#results.all.exp <- runAllExpTrainedClassifier()
#write.csv(results.all.exp, file="results.all.exp.csv")