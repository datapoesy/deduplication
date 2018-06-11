
#Setup the environment
setwd("F:/GitHub/deduplication/src")
source("config/libraries.R")
source("lib/helpers.R")
options(fftempdir = "F:/GitHub/deduplication/src/data/temp")



#Convert a local df to ffdf
xclass <- c("factor","factor","factor","factor","factor",
            "factor","factor","factor","factor","factor",
            "factor","factor","factor","factor","factor",
            "integer","integer","integer")


## Read the sanitized dataset

# modified.count <- c(1,2,3,4,5)
modified.count <- c(1,2,3,4,5)

for(m in 1:length(modified.count)){
  
    train.set <- "mt5"
  
    modified.fn <- paste("data/Modified",modified.count[m],".csv",sep="")
    
    base.ds <- read.csv(file=modified.fn, stringsAsFactors = TRUE)
    
    # base.ds <- base.ds[,c(-1)] #dropping the  first column
    
    
    ##### ============  Create a dataframe to store the results =======
    
    cl.results.fn <- paste("data/",train.set,".results.mod",modified.count[m],".csv",sep="")
    cl.results.00 <- as.data.frame(matrix(0,ncol=7,nrow=1))
    colnames(cl.results.00) <- c("No.","Sample.Count","Total.Pairs","Orig.Dupes",
                                 "Ident.Dupes","Train.Count","Time.Taken")
    
    #========================================================================
    
    #Set the number of records to be sampled from the sanitized dataset
    perc.value <- c(1,5,10,15,20,25,30,35,40,45)
    # perc.value <- c(1,5)
    
    for(i in 1:length(perc.value)){
    
            # ds.count <- 100000
            ds.count <- perc.value[i]*1000
            
            ds.00 <-  base.ds[sample(nrow(base.ds),ds.count),]
            
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
            
            alt.ds.count <- nrow(ds.00.C)+nrow(ds.00.T)+nrow(ds.00.N)+nrow(ds.00.D)
            #subset(unmatched, grepl('D',unmatched$recoid)) ## To verify whether all D has been removed.
            
            
            ##Write the current dataframe to a csv file for reference
            #sample.fn <- paste("data/sample.exp4withExp3Train",ds.count,".csv",sep="")
            #write.csv(ds.00,file=sample.fn, row.names = FALSE ,quote=c(1:15) )
            
            
            #Converting to a ffbase object format
            #Also setting the temporary directory to write the intermediate files.
            
            ds.00=as.ffdf(ds.00)
            
            colnames(ds.00) <- c("X_recoid_","X_someid_","X_firstname_","X_lastname_","X_gender_",
                                 "X_birth_date_","X_email_","X_mnumber_","X_hnumber_","X_addr1_",
                                 "X_addr2_","X_cityname_","X_postcode_","X_county_","X_nis_",
                                 "X_b_year_","X_b_month_","X_b_day_")
            
            #Record the start time & create the comparison vectors.
            
            # trace("RLBigDataDedup",edit=TRUE) # Modify the function RLBigDataDedup
            depup.st.time <- Sys.time()
            rpairs.00 <- RLBigDataDedup(ds.00,blockfld = list(4,c(16,17,18)),phonetic = c(3,4),exclude = c("X_recoid_","X_someid_", "X_birth_date_","X_mnumber_","X_hnumber_","X_gender_"))
           
            #Record a measurement of data quality.  Percentage of modified records.
            data.quality <- round(alt.ds.count/ds.count *100,2)
            
            #============================== UNCOMMENT ONLY FOR NEW TRAINING PAIRS ==========
            #Get Minimal Training Pairs.  Uncomment only if this step needs to be executed.
            # minTrain.00 <- getMinimalTrain(rpairs = rpairs.00, nEx = 1)
            # minTrain.00$pairs <- read.csv(file="data/minTrain.00Pairsexp5.csv")
            
            #Get pairs to export to excel.  Add an is_match column for user entry.  
            #Uncomment only if a training set is created again. 
            # editable.pairs <- getPairs(minTrain.00)
            # editable.pairs$is_match <- ""

            # trained.set.fn <- paste("data/",ds.count,"trained.set.exp5.csv",sep="")
            # write.csv(editable.pairs,file=trained.set.fn, row.names = FALSE )
            
            #edit the pairs inline.  This is commented out since the editing is done in excel.  
            #Uncomment only if this step needs to be done in R.
            
            #minTrain.00 <- editMatch(minTrain.00)
            #==============================================================================
            
            
            ##The updated Excel by the user needs to be fed back into the system
            
            # edited.pairs <- read.csv(file="data/1e+05trained.set.exp5.csv", stringsAsFactors=FALSE )
            # edited.pairs <- edited.pairs[!apply(is.na(edited.pairs) | edited.pairs == "", 1, all),]
            # 
            # 
            # 
            # # # The blank line that separates rows is removed
            # edited.pairs <- edited.pairs[,c("id","is_match")]
            # # 
            # # The relevant columns from the edited excel file
            # is.match.ds <- as.data.frame(matrix(NA,ncol=3,nrow=1))
            # #create dataframe to take input from excel
            # colnames(is.match.ds) <- c("id1", "id2","is_match")
            # 
            # # Take only the relevant columns from user input
            # for(i in 1:nrow(edited.pairs)){
            #   alt.row = i+1
            #   is.match.ds[i,1] <- edited.pairs[i,1]
            #   is.match.ds[i,2] <- edited.pairs[alt.row,1]
            #   is.match.ds[i,3] <- edited.pairs[i,2]
            # }
            # is.match.ds <- is.match.ds[complete.cases(is.match.ds), ]
            # 
            # ##Load the trained set into the engine
            # for(i in 1:nrow(is.match.ds)){
            #   if(minTrain.00$pairs[i,1]==is.match.ds[i,1] && minTrain.00$pairs[i,2]== is.match.ds[i,2]){
            #     minTrain.00$pairs[i,c("is_match")] <- is.match.ds[i,c("is_match")]
            #   }
            # }
            # 
            # 
            # write.csv(minTrain.00$pairs,file="data/minTrain.00Pairsexp5.csv", row.names = FALSE)
            
           #===============================================================================================
            
            
            
            #Loading weights and classifying
            count.train <- nrow(minTrain.00$pairs)  
            rpairs.00 <- epiWeights(rpairs.00, e=0.01, f =getFrequencies(rpairs.00))
            model.00 <- trainSupv(minTrain.00, method = "rpart",minsplit=1) # Training method is passed as argument
            results.00 <- classifySupv(model.00, newdata = rpairs.00)
            
            # save.ffdf(rpairs.00,dir="data", file="rpairs.00")
            depup.end.time <- Sys.time()
            time.taken <- difftime(depup.end.time, depup.st.time, units='mins')
            
            #Get the identified pairs and write to a file for manual verification
            # ident.pairs.00 <- getPairs(results.00,filter.link = "link")
            
            # results.fn <- paste("data/",ds.count,"results.00.csv",sep="")
            # write.csv(ident.pairs.00,file=results.fn)
            
            #Recording Results 
            ident.dupes <-unlist(summary(results.00)[9]) # take value from the summary list
            ident.dupes <- unname(ident.dupes)
            
            pairs.count <- unlist(summary(rpairs.00)[7]) # take value from the summary list
            pairs.count <- unname(pairs.count)
            
            if(ident.dupes>orig.dupes){ false.pos <- ident.dupes - orig.dupes }
            
            #set the value of the row to capture results
            #c("No.","Sample.Count","Total.Pairs","Orig.Dupes","Ident.Dupes","Train.Count","Time.Taken")
            
            cl.results.00[i, 1] <- i
            cl.results.00[i, 2] <- ds.count
            cl.results.00[i, 3] <- pairs.count
            cl.results.00[i, 4] <- orig.dupes
            
            cl.results.00[i, 5] <- ident.dupes
            cl.results.00[i, 6] <- count.train
            cl.results.00[i, 7] <- time.taken
            
            
            #Save the results file
            #summary.fn <- paste("data/summary.exp4withExp3TrainSet.csv",sep="")
            
            write.csv(cl.results.00, file=cl.results.fn )
    }

}