setwd("F:/GitHub/deduplication/src")
wd <- getwd()
setwd("..")
parent <- getwd()
setwd(wd)
source("config/libraries.R")
source("lib/helpers.R")

#Reading customer data
cust.rcds = read.csv(file="data/customers-200k.csv")

colnames(cust.rcds)
#Column filters for the dataset
col.filter <- c("userid","someid","firstname","lastname",
                "gender","birthdate","email","mobilenumber",
                "homenumber","address1","address2","cityname","postcode",
                "county","nis")

base.ds.200k <- cust.rcds[col.filter]

#Renaming columns in the dataset
col.names <-  c("recoid","someid","firstname","lastname",
                  "gender","birthdate","email","mnumber",
                  "hnumber","addr1","addr2","cityname","postcode",
                  "county","nis")
colnames(base.ds.200k) <- col.names


#date formatting of the issue log
base.ds.200k$birthdate <- as.POSIXct(base.ds.200k$birthdate, format='%Y-%m-%d')
base.ds.200k$birthdate <- as.Date(base.ds.200k$birthdate, format='%Y-%m-%d')

#Column Descriptions  ----------------------------------------------------------------------------
desc.base.ds.200k <- as.data.frame(t(base.ds.200k[1,]))
desc.base.ds.200k$variables <- rownames(desc.base.ds.200k)
rownames(desc.base.ds.200k) <- NULL
colnames(desc.base.ds.200k)<- c("Description","Variable")
desc.base.ds.200k$Description <- c("Unique ID in the dataset","Some Org ID","First Name","Last Name","Gender",
                              "Date of Birth","e-mail ID","Mobile Number",
                              "Home Number","Home Nummber","Street Address",
                              "Name of the City","Post Code","Country","National Insurance Number")

desc.base.ds.200k$No. <- c(1:nrow(desc.base.ds.200k))
desc.base.ds.200k <-desc.base.ds.200k[,c(3,2,1)]
write.csv(desc.base.ds.200k, file="desc.base.ds.200k.csv")
#------------------------------------------------------------------------------------------------

base.ds.200k$b.year <- format(base.ds.200k$birthdate, format='%y')
base.ds.200k$b.month <- format(base.ds.200k$birthdate, format='%m')
base.ds.200k$b.day <- format(base.ds.200k$birthdate, format='%d')

#dropping the DoB column as it is not required
#base.ds.200k <- base.ds.200k[,-6]


#Convert cont.id and someid to characters as required for string comparison
base.ds.200k$someid <- as.character(base.ds.200k$someid)
base.ds.200k$mnumber <- as.character(base.ds.200k$mnumber)
base.ds.200k$hnumber <- as.character(base.ds.200k$hnumber)
base.ds.200k$birthdate <- as.character(base.ds.200k$birthdate)
base.ds.200k$email <- as.character(base.ds.200k$email)
base.ds.200k$addr1 <- as.character(base.ds.200k$addr1)
base.ds.200k$addr2 <- as.character(base.ds.200k$addr2)
base.ds.200k$county <- as.character(base.ds.200k$county)
base.ds.200k$postcode <- as.character(base.ds.200k$postcode)
base.ds.200k$nis <- as.character(base.ds.200k$nis)


write.csv(base.ds.200k, file="data/base.ds.200k.csv")

remove(cust.rcds)





