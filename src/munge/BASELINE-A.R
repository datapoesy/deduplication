setwd("F:/GitHub/deduplication/src")
wd <- getwd()
setwd("..")
parent <- getwd()
setwd(wd)
source("config/libraries.R")
source("lib/helpers.R")
#source("munge/01-A.R")

## Set the count of records in the dataset

desc.base.ds <- read.csv(file="rdscsvfiles/desc.base.ds.csv")[,-c(1)]



perc.value <- c(5, 10, 20, 30, 40,50,60)

i = 1
ds.count <- perc.value[i]*1000

set.seed(1234)

## Sample dataset from the original 200K records
#ds.00 <-  base.ds[sample(nrow(base.ds),ds.count),]

ds.00 <- read.csv(file="data/base.ds.200k.csv", stringsAsFactors = FALSE)[,-c(1)]

## set a percentage value for data to be disorganized
p <- 4

## create a data frame to store duplicated records
dupli.ds <- data.frame(matrix(0, ncol = ncol(ds.00), nrow = 0))
colnames(dupli.ds) <- colnames(ds.00)

dupli.ds$someid <- as.character(dupli.ds$someid)
dupli.ds$mnumber <- as.character(dupli.ds$mnumber)
dupli.ds$hnumber <- as.character(dupli.ds$hnumber)
dupli.ds$birth.date <- as.character(dupli.ds$birth.date)
dupli.ds$postcode <- as.character(dupli.ds$postcode)

## create a dataframe to store non-duplicated records
non.dupli.ds <- dupli.ds

#========================================================================
# Simulating real-world data
#-------------------------------------------------------------------------

###Clone  records with same  address
temp.ds <- clonePerson(ds.00, p)
dupli.ds <- rbind(dupli.ds, temp.ds$clones)

dupli.ds <- dupli.ds[!duplicated(dupli.ds$recoid),]


###Duplicate Records for identical Twins living in the same address,
## also pass the list of record ids that has been used.
pt <- 0.06*p

temp.ds <- createTwins(temp.ds$ds.99, pt)
non.dupli.ds <- rbind(non.dupli.ds, temp.ds$twins) # Non-duplicates means individual people
non.dupli.ds <- non.dupli.ds[!duplicated(non.dupli.ds$recoid),]



###Duplicate Records for neighbors living in the same apartment,
## also pass the list of record ids that has been used.
pn <- 0.01*p
temp.ds <- createNeighbors(temp.ds$ds.99, pn)
non.dupli.ds <- rbind(non.dupli.ds, temp.ds$neighbors) # Non-duplicates means individual people
non.dupli.ds <- non.dupli.ds[!duplicated(non.dupli.ds$recoid),]

###Duplicate Records for Married couples living in the same address
# % of couples in the loan availing population = 50%
# % of couples living in the same address = 80%
# Combined percentage of the above = 40%
pc <- 4*p
temp.ds <- createCouples(temp.ds$ds.99, pc)
non.dupli.ds <- rbind(non.dupli.ds, temp.ds$couples)


ds.00 <- rbind(ds.00, dupli.ds)
ds.00 <- rbind(ds.00,non.dupli.ds)

record.count <- nrow(ds.00)

#---------------------------------------------------------------

pe <- .01* p 
###Create Typographical errors name to simulate spelling mistakes
ds.00 <- makeTypos(ds.00, "John", "Jon", pe)
ds.00 <- makeTypos(ds.00, "Thompson", "Thomson", pe)
ds.00 <- makeTypos(ds.00, "tt", "t", pe)
ds.00 <- makeTypos(ds.00, "ss", "s", pe)

#simulate missing email id - write into the base dataset
ds.00$email[sample(nrow(ds.00), nrow(ds.00) * p / 100)] <- ''

#simulate missing home phone numbers
ds.00$hnumber[sample(nrow(ds.00), nrow(ds.00) * p / 100)] <- ''

###Missing Mobile phone numbers
ds.00$mnumber[sample(nrow(ds.00), nrow(ds.00) * p / 100)] <- ''

#Simulate missing postcodes
ds.00$postcode[sample(nrow(ds.00), nrow(ds.00) * p / 100)] <- ''

###Simulate Missing Address 1 from Address
ds.00$addr1[sample(nrow(ds.00), nrow(ds.00) * p / 100)] <- ''

###simulate Missing Address 2 from Address
ds.00$addr2[sample(nrow(ds.00), nrow(ds.00) * p / 100)] <- ''

###Simulate Missing County Name from Address
ds.00$county[sample(nrow(ds.00), nrow(ds.00) * p / 100)] <- ''

###Simulate Missing City Name from Address
ds.00$cityname[sample(nrow(ds.00), nrow(ds.00) * p / 100)] <- ''


#write.csv(ds.00,file="data/ds.00.200k.csv", row.names = FALSE ,quote=c(1:15) )

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
                                  The recoid is tagged with 'T' to identify these records."))

pre.process[11,c(2:3)] <- c("Married couples living in the same address", 
                            paste(pc,"% records created for couples living in the same house:",
                                  "The First Name is changed. " ,
                                  "The gender is changed to the opposite of the originial record. " ,
                                  "The recoid is tagged with 'C' to identify these records. "  ,
                                  "The date, month and year in DoB is changed  to a random number. ",
                                  "The firstname is changed to a random name. " ))


pre.process[12,c(2:3)] <- c("Altering name to simulate spelling mistakes", 
                            paste(pc,"% of the records with a particular name is 
                                  changed to a new one to simulate spelling mistatakes:
                                  'John' is changed to 'Jon'  
                                  'Thompson' is changed to 'Thomson'  
                                  'tt' of words changed to single 't'" ))


pre.process[13,c(2:3)] <- c("Neighbors in the same apartment with identical first names", 
                            paste(pn,"% of the records with a particular name is 
                                  changed on dob, phone numbers and nis number and record tagged with 'N'" ))

pre.process[14,c(2:3)] <- c("Clone Entities to create duplicate records 
                            to be potentially identified by the dedup engine.", 
                            paste(pc,"% of the original records created new.  These records are created to be 
                                  duplicates and tagged with a 'D'. The records are cloned and subsequently, 
                                  20% of first names have their trailing vowel sounds removed.  
                                  The last names are stripped of their beginning vowels replaced with a new 'someid'. 
                                  20% of records are stripped of their email address, NIS, County names etc. 
                                  20% have their addresses modified.  20% have their postcodes removed. 
                                  20% have the gender information removed." ))

pre.process[,c(1)]<- c(1:nrow(pre.process))
colnames(pre.process) <- c("Sl.No","Scenario","Description")

#===============  Convert a local df to ffdf ====================


xclass <- c("factor","factor","factor","factor","factor",
            "factor","factor","factor","factor","factor",
            "factor","factor","factor","factor","factor",
            "integer","integer","integer")

## Set the count of records in the dataset
base.ds <- read.csv(file="data/ds.00.200k.csv", colClasses = xclass,  stringsAsFactors = FALSE)


## Sample dataset from the original 200K records
ds.00 <-  base.ds[sample(nrow(base.ds),ds.count),]

ds.00.C <- subset(ds.00, grepl('C',ds.00$recoid))
ds.00.T <- subset(ds.00, grepl('T',ds.00$recoid))
ds.00.N <- subset(ds.00, grepl('N',ds.00$recoid))
ds.00.D <- subset(ds.00, grepl('D',ds.00$recoid))
unmatched <- anti_join(ds.00, ds.00.C)
unmatched <- anti_join(unmatched, ds.00.T)
unmatched <- anti_join(unmatched, ds.00.N)
unmatched$recoid <- gsub(" D", "\\", unmatched$recoid)
unmatched$recoid <- trimws(unmatched$recoid, which = c("both"))

dupli.count <- nrow(unmatched[duplicated(unmatched$recoid), ])

alt.ds.count <- nrow(ds.00.C)+nrow(ds.00.T)+nrow(ds.00.N)+nrow(ds.00.D)

#subset(unmatched, grepl('D',unmatched$recoid)) ## To verify whether all D has been removed.

write.csv(ds.00,file="data/ds.00.5k.csv", row.names = FALSE ,quote=c(1:15) )


#ds.00=as.data.frame(ds.00, stringsAsFactors=TRUE)
ds.00=as.ffdf(ds.00)


#===================  dataset complete =========================



options(fftempdir = "F:/GitHub/deduplication/src/data/temp")

#hhp <- read.table.ffdf(file="data/ds.00.5k.csv",quote = "", FUN = "read.csv", na.strings = "",sep=",", colClasses=xclass) 


colnames(ds.00) <- c("X_recoid_","X_someid_","X_firstname_","X_lastname_","X_gender_",
                   "X_birth_date_","X_email_","X_mnumber_","X_hnumber_","X_addr1_",
                   "X_addr2_","X_cityname_","X_postcode_","X_county_","X_nis_",
                   "X_b_year_","X_b_month_","X_b_day_")

depup.st.time <- Sys.time()

#Creating comparison vectors with in-memory comparison
rpairs.00 <- RLBigDataDedup(ds.00,blockfld = list(4,c(16,17,18)),phonetic = c(3,4),exclude = c("X_recoid_","X_someid_", "X_birth_date_","X_mnumber_","X_hnumber_","X_gender_"))


depup.end.time <- Sys.time()
time.taken <- depup.end.time - depup.st.time



#============================================================

data.quality <- round(alt.ds.count/ds.count *100,2)

#===================================================================================================
###Supervised clasification with Minimal Training Set
#===================================================================================================

cl.results.00 <- as.data.frame(matrix(0,ncol=9,nrow=1))

colnames(cl.results.00) <- c("No.","Total.Records","Duplicates",
                             "Non.Duplicates","Record.Pairs","Quality.Perc",
                             "Ident.Pairs","Pairs.Perc",
                             "Train.Count")

#Get Minimal Training Pairs
minTrain.00 <- getMinimalTrain(rpairs = rpairs.00, nEx = 1)

#edit the pairs inline
#minTrain.00 <- editMatch(minTrain.00)


editable.pairs <- getPairs(minTrain.00) # Get pairs to export to excel
editable.pairs$is_match <- "" # Add an is_match column for user entry
write.csv(editable.pairs,"data/user-trained-pairs.csv", row.names = FALSE ) 

#=============================================================================================
#The update excel needs to be read and processed back
#=============================================================================================
edited.pairs <- read.csv(file="data/user-trained-pairs.csv", stringsAsFactors=FALSE )  
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
#============================================================================================= 

count.train <- nrow(minTrain.00$pairs)  
rpairs.00 <- epiWeights(rpairs.00, e=0.01, f =getFrequencies(rpairs.00))
model.00 <- trainSupv(minTrain.00, method = "rpart",minsplit=1) # Training method is passed as argument
results.00 <- classifySupv(model.00, newdata = rpairs.00)



# Recording Results 
#------------------------------------------------------------------------------------------------


cl.results.00[1, 1] <- 1
cl.results.00[1, 2] <- record.count
cl.results.00[1, 3] <- nrow(dupli.ds)

cl.results.00[1, 4] <- nrow(non.dupli.ds)
cl.results.00[1, 5] <- nrow(results.00$pairs)
cl.results.00[1, 6] <- data.quality
cl.results.00[1, 7]  <- summary(results.00$prediction)[3]
cl.results.00[1, 8]  <- round(summary(results.00$prediction)[3]/nrow(dupli.ds)*100,2)
cl.results.00[1, 9] <- count.train

ident.pairs.00 <- getPairs(results.00,filter.link = "link")


write.csv(cl.results.00,file="results/cl.results.00.csv")



#results.all.exp <- runAllExpTrainedClassifier()
#write.csv(results.all.exp, file="results/results.all.exp.csv")