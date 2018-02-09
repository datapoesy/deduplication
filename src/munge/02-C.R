setwd("F:/GitHub/deduplication/src")
wd <- getwd()
setwd("..")
parent <- getwd()
setwd(wd)
source("config/libraries.R")
source("lib/helpers.R")



dbs.00 = read_csv(file="data/customers-200k.csv")
dbs.00 <- dbs.00[,-c(1)]

require(ffbase)
hhp <- read.table.ffdf(file="data/RL.base.ds.csv",sep = ",", quote = "", FUN = "read.csv", na.strings = "")

hhp <- hhp[,-c(1)]
str(hhp[1:10,])

head(hhp)
colnames(hhp) <- c("X_record_id_","X_some_id_","X_firstname_","X_lastname_","X_gender_","X_birth_date_","X_email_","X_m_nmbr_","X_h_nmbr_","X_addr_1_","X_addr_2_","X_cityname_","X_postcode_","X_county_","X_nis_","X_b_year_","X_b_month_","X_b_day_")


#===================  dataset complete =========================

dbs.00 <-   dbGetQuery(con, paste("SELECT * from dbstable"))
dupli.ds <- dbGetQuery(con, paste("SELECT * from dupli"))
nondupli.ds <- dbGetQuery(con, paste("SELECT * from nondupli"))

dbs.00 <- rbind(dbs.00,dupli.ds)
dbs.00 <- rbind(dbs.00, nondupli.ds)
dbs.00 <- dbs.00[,-c(2)]
#write.csv(dbs.00, file="data/dbs-00-700k.csv")


rpairs.00 <- RLBigDataDedup(hhp,blockfld = list(4,c(6,7,8)),phonetic = c(3,4),  exclude = c(1,2))
rpairs.00 <- epiWeights(rpairs.00, e=0.01, f=getFrequencies(rpairs.00))



minTrain.00 <- getMinimalTrain(rpairs.00)
tpairs <- getPairs(minTrain.00,show="all",single.row=FALSE)
tpairs$is_match <- ""

write.csv(tpairs,file="test-input.csv")
tpairs <- read.csv(file="test-input.csv")
tpairs <- tpairs[,-c(1:2)]


tpairs <- tpairs[!apply(is.na(tpairs) | tpairs == "", 1, all),]
tpairs <- tpairs[seq(1, nrow(tpairs), 2),]

minTrain.00$pairs$id1



minTrain.00 <- editMatch(minTrain.00)
write.csv(minTrain.00$pairs, file="rdscsvfiles/minTrain-pairs.csv")
count.train <- nrow(minTrain.00$pairs)


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

colnames(minTrain.00$data) <- c("X_record_id_","X_some_id_","X_firstname_","X_lastname_","X_gender_","X_birth_date_","X_email_","X_m_nmbr_","X_h_nmbr_","X_addr_1_","X_addr_2_","X_cityname_","X_postcode_","X_county_","X_nis_","X_b_year_","X_b_month_","X_b_day_")

colnames(minTrain.00$frequencies) <- c("X_record_id_","X_some_id_","X_firstname_","X_lastname_","X_gender_","X_birth_date_","X_email_","X_m_nmbr_","X_h_nmbr_","X_addr_1_","X_addr_2_","X_cityname_","X_postcode_","X_county_","X_nis_","X_b_year_","X_b_month_","X_b_day_")



count.train <- nrow(minTrain.00$pairs)
rpairs.00 <- epiWeights(rpairs.00, e=0.01, f =rpairs.00$frequencies)
model.00 <- trainSupv(minTrain.00, method = "rpart",minsplit=1) # Training method is passed as argument
results.00 <- classifySupv(model.00, newdata = rpairs.00)


data.quality <- round(dbs.count/record.count *100,2)

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

write.csv(cl.results.00,file="results/cl.results.00.csv")
#============================================================================================
#============================================================================================

