setwd("F:/GitHub/deduplication/src")
wd <- getwd()
setwd("..")
parent <- getwd()
setwd(wd)
source("config/libraries.R")
source("lib/helpers.R")

## Set the count of records in the dataset
base.ds <- read.csv(file="data/base.ds.csv", stringsAsFactors = FALSE)[,-c(1)]
desc.base.ds <- read.csv(file="rdscsvfiles/desc.base.ds.csv")[,-c(1)]

ds.count <- 5000

perc.value <- c(1, 2, 4, 8, 16)

i = 1

set.seed(1234)


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

write.csv(dupli.ds,file="results/dupli.ds.5k.csv")


###Duplicate Records for identical Twins living in the same address,
## also pass the list of record ids that has been used.
pt <- 0.06*p

temp.ds <- createTwins(temp.ds$ds.99, pt)
non.dupli.ds <- rbind(non.dupli.ds, temp.ds$twins) # Non-duplicates means individual people
non.dupli.ds <- non.dupli.ds[!duplicated(non.dupli.ds$recoid),]

write.csv(non.dupli.ds,file="results/non.dupli.ds.5k.csv")
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

pe <- .1* p 
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


write.csv(ds.00,file="data/ds.00.5k.csv", row.names = FALSE ,quote=c(1:15) )

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
                            paste(pe,"% of the records with a particular name is 
                                  changed to a new one to simulate spelling mistatakes:
                                  'John' is changed to 'Jon'  
                                  'Thompson' is changed to 'Thomson'  
                                  'tt' of words changed to single 't'" ))

pre.process[12,c(2:3)] <- c("Clone Entities to create duplicate records 
                            to be potentially identified by the dedup engine.", 
                            paste(pc,"% of the original records created new.  These records are created to be 
                                  duplicates and tagged with a 'D'. The records are cloned and subsequently, 20% of first names have their trailing vowel sounds removed.  The last names are stripped of their beginning vowels replaced with a new 'someid'. 20% of records are stripped of their email address, NIS, County names etc. 20% have their addresses modified.  20% have their postcodes removed. 20% have the gender information removed." ))

pre.process[,c(1)]<- c(1:nrow(pre.process))
colnames(pre.process) <- c("Sl.No","Scenario","Description")

