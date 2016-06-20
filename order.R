# Modes available: 
# normal:  Normal operation
# test: 
# run: 
opMode <- 'normal'

# Load required libraries
library(RCurl)
library(jsonlite)
library(dplyr)
library(stringr)
library(lubridate)
library(log4r)
library(parallel)
library(plotrix)
library(base64)
library(ggplot2)
library(xtable)
library(gbm)

# Set config params
model <- 'data/fitGbm.rda'
config <- 'store/config.R'

# Load helper functions
source('funcs.R')

# Set correct working directory to use relative paths
cmdArgs <- commandArgs(trailingOnly = FALSE)
needle <- "--file="
match <- grep(needle, cmdArgs)
if (length(match) > 0) {
  # Run from script with --file option
  setwd(dirname(normalizePath(sub(needle, "", cmdArgs[match]))))
} else {
  if (! is.null(sys.frames())) {
    # Sourced via R console
    setwd(dirname(normalizePath(sys.frames()[[1]]$ofile,winslash='/')))
    print('john')
  } else {
    # Run from RStudio (development - these are my home dirs)
    if ( file.exists('/home/john') ) {
      setwd("/home/john/Dropbox/cPLS")
    } else if ( file.exists('/home/user') ) {
      setwd("/home/user/cPLS")
    } else {
      setwd("C:/Users/john/Dropbox/cPLS")
    }
  }
}

# Create directories if they don't exist
dir.create('store', showWarnings = FALSE)
dir.create('tmp', showWarnings = FALSE)

# Initialize log
logFile='store/system.log'
log <- create.logger(level='INFO',logfile=logFile)
info(log,'-------------------------------------')
info(log,'Starting Command Line PLS Version 1.0')


# Load pre-built GBM model
reqFile(model)
info(log,'Loading machine learning model')
load(model)


# Load cPLS configuration
reqFile(config)
info(log,'Importing configuration')
source(config)


# Load all users from store sub-directory (must end with .acc extension)
info(log,'Importing User Accounts')
users <- list()
files <- list.files(path="store", pattern="*.acc", full.names=T, recursive=FALSE)
for (file in files) {
  lc=list()
  source(file)
  users <- append(users,list(lc))
  info(log,paste("Importing user: ",lc$name))
}
if (length(users)==0) {
  err('No user accounts configured')
}


# Start continous loop to wait for scheduled execution
info(log,'Load complete.  Waiting for scheduled start time ...')
while (1) {

  # Get current California time and start at appropriate time
  nowPST <- with_tz(now(),"America/Los_Angeles")
  hmPST <- paste(hour(nowPST),minute(nowPST),sep=":")

  # Start system only at given start times
  if (hmPST %in% startTimes | test == TRUE) {
        
    # Read log file (connection)
    closeAllConnections()
    con <- file(logFile,"r")
    seek(con,where=0,origin='end')
    
    # Obtain initial cash for each user
    for (i in 1:length(users)) {
      for (attempt in 1:5) {
        users[[i]]$pre$jsonCash <- getURL(paste("https://api.lendingclub.com/api/investor/",apiVersion,"/accounts/",users[[i]]$accID,"/availablecash",sep=''),
                                                                              httpheader = c('Authorization' = users[[i]]$token,
                                                                             'Accept' = "application/json",
                                                                             'Content-type' = "application/json"))
        if ( is.null(users[[i]]$pre$jsonCash) | length(users[[i]]$pre$jsonCash) == 0 ) {
          warn(log,paste('User (',users[[i]]$name,') - Unable to obtain available cash (Null API Response). Attempt: ',attempt,sep=""))
          next
        }
        if ( ! grepl("availableCash",users[[i]]$pre$jsonCash) ) {
          warn(log,paste('User (',users[[i]]$name,') - Unable to obtain available cash (Invalid API Response). Attempt: ',attempt,sep=""))
          warn(log,paste('User (',users[[i]]$name,') - API Response: ', users[[i]]$pre$jsonCash,sep=''))
          next
        }
        users[[i]]$pre$cash <- fromJSON(users[[i]]$pre$jsonCash)$availableCash
        if ( ! is.numeric(users[[i]]$pre$cash) ) {
          warn(log,paste('User (',users[[i]]$name,') - Unable to obtain available cash (Non-numeric API Response). Attempt: ',attempt,sep=""))
          warn(log,paste('User (',users[[i]]$name,') - API Response: ', users[[i]]$pre$jsonCash,sep=''))
          next
        }
        info(log,paste('User (',users[[i]]$name,') - Initial cash available: ',printCurrency(users[[i]]$pre$cash),sep=""))
        break
      }
    }
    
    info(log,"Starting loan list detection")
    
    # Obtain starting note count
    for (attempt in 1:5) {
      startJson <- getURL(urlLoanList,httpheader = c('Authorization' = users[[1]]$token,
                                                     'Accept' = "application/json",
                                                     'Content-type' = "application/json"))
      if ( is.null(startJson) | length(startJson) == 0 ) {
        warn(log,paste('Unable to obtain initial note count (Null API Response). Attempt: ',attempt,sep=""))
        next
      }
      if ( ! grepl("pubRec",startJson) ) {
        warn(log,paste('Unable to obtain initial note count (Invalid API Response). Attempt: ',attempt,sep=""))
        next
      }
      prevIds <- fromJSON(startJson)$loans$id
      if ( ! length(prevIds) ) {
        warn(log,paste('Unable to obtain initial note count (API Conversion Error). Attempt: ',attempt,sep=""))
        next
      }
      info(log,paste("Previous note count:",length(prevIds)))
      break
    }

    # List detection
    list=FALSE
    # Set default apiTime
    apiTimeStart <- proc.time()[3]
    if (test == TRUE) {
      num <- 1
    } else {
      num <- maxNoteCount
    }
    for (cnt in 1:num) {
      # Loop to wait 1 second between API calls
      while (TRUE) {
        if(proc.time()[3] > apiTimeStart+1) { 
          apiTimeStart <- proc.time()[3]
          newJson <- getURL(urlLoanList,httpheader = c('Authorization' = users[[1]]$token,
                                                       'Accept' = "application/json",
                                                       'Content-type' = "application/json"))
          apiTimeElapse <- proc.time()[3] - apiTimeStart
          break
        }
      }
      if ( is.null(newJson) | length(newJson) == 0 ) {
        warn(log,paste("List detection (",cnt," of ",num,") - Null API Response ",sep=''))
        next
      }
      if ( ! grepl("pubRec",newJson) ) {
        warn(log,paste("List detection (",cnt," of ",num,") - Invalid API response",sep=''))
        next
      }
      loans = fromJSON(newJson)$loans
      if ( ! nrow(loans) ) {
          warn(log,paste("List detection (",cnt," of ",num,") - API Conversion Error",sep=''))
          next
      }
      newIds <- loans$id
      newNoteCount <- length(newIds)
      
      # Must have no previous notes in new notes, and greater than threshold to detect list
      if ( ! any(prevIds %in% newIds) & newNoteCount > numNotesThresh ) {
        list=TRUE
        listTime=with_tz(now(),"America/Los_Angeles")
        info(log,paste("List detected - New note count: ",newNoteCount,sep=''))
        break
      } else {
        info(log,paste("List detection (",cnt," of ",num,")",sep=''))
      }
    }

    if(test == TRUE) {
      listTime=with_tz(now(),"America/Los_Angeles")
    } else {
      # Only continue if note list detected
      if(!list) { 
        warn(log,"New notes listing not detected")
        next
      }
    }
  
    info(log,'Modeling available notes')
    
    # Record start of modeling time
    startModelTime <- proc.time()


    # Add model probability to each loan
    loans$earliestCrLine <- ymd(substring(loans$earliestCrLine,1,10))
    loans$n=ymd(Sys.Date())
    loans$earliestCrLineMonths=as.integer(round((loans$n - loans$earliestCrLine)/30.4375)-1)
    loans$installmentIncomeRatio=loans$installment/(loans$annualInc/12)
    loans$revolBalAnnualIncRatio=loans$revolBal/loans$annualInc
    loans$grade <- factor(loans$grade)
    loans$subGrade <- factor(loans$subGrade)
    loans$homeOwnership <- factor(loans$homeOwnership)
    loans$purpose <- factor(loans$purpose)
    loans$addrState <- factor(loans$addrState)
    loans$addrZip <- factor(loans$addrZip)
    loans$model=predict(fitGbm,newdata=loans,n.trees=fitGbm$n.trees,type="response")
      
    elapsedModelTime <- round(proc.time() - startModelTime[3],2)

    loans$n=NULL
    loans$gbmProb=NULL
    loans$survProb=NULL

    loans$pctFunded = loans$fundedAmount / loans$loanAmount
    avgPctFunded <- loans$pctFunded
    timeStampFile = gsub(" ","_",gsub(":","-",listTime))

    # Record start time of selection process per user
    startTime <- proc.time()

    # Process each user account
    info(log,'Processing all user accounts')
    result <- mclapply(1:length(users),function(i){
      
      # Simple loop to stop execution on error
      for(one in 1) {
        
        # Record start time of filter process
        users[[i]]$startFilterTime <- proc.time()
        
        # Verify we have cash available for user
        if(is.null(users[[i]]$pre$cash) | length(users[[i]]$pre$cash) == 0 | ! is.numeric(users[[i]]$pre$cash)) {
          warn(log,paste('User (',users[[i]]$name,') - No initial cash amount (stopping)',sep=""))
          break
        }
    
        # Verify have above minimum cash level + investment amount
        if(users[[i]]$pre$cash <= users[[i]]$minCash + users[[i]]$amountPerNote) {
          warn(log,paste('User (',users[[i]]$name,') - Available cash $',users[[i]]$pre$cash,' less than $',
                         users[[i]]$minCash + users[[i]]$amountPerNote,' (stopping)',sep=''))
          break
        }
        
        # Filter loans based on user provided criteria
        users[[i]]$filteredLoans <- loans %>%
          arrange(desc(loans[[users[[i]]$sortField]])) %>%
          users[[i]]$filterCriteria() %>%
          select(id,grade,term)
          
        
        users[[i]]$filteredLoans$grade <- factor(users[[i]]$filteredLoans$grade,levels=c("A","B","C","D","E","F","G"))
        users[[i]]$filteredLoans$term <- factor(users[[i]]$filteredLoans$term,levels=c("36","60"))
        
        # Stop processing if no filtered loans
        users[[i]]$totalFilteredLoans <- dim(users[[i]]$filteredLoans)[1]
        if (users[[i]]$totalFilteredLoans < 1) {
          info(log,paste('User (',users[[i]]$name,') - No notes match filter criteria',sep=""))
          break
        } else {
          info(log,paste('User (',users[[i]]$name,') - Total filtered notes: ',users[[i]]$totalFilteredLoans,sep=""))
        }
        users[[i]]$numFilteredNotes <<- users[[i]]$totalFilteredLoans
        
        # Obtain filtered notes id's
        users[[i]]$filteredIds <- users[[i]]$filteredLoans$id

        # If user specified grade or term allocation, get total notes in portfolio + filtered to do calcs
        if (exists('gradeAllocation', where=users[[i]]) | exists('termAllocation', where=users[[i]]) ) {
          if(is.null(users[[i]]$pre$portNoteCnt) | length(users[[i]]$pre$portNoteCnt) == 0 ) {
            warn(log,paste('User (',users[[i]]$name,') - No initial portfolio information (stopping)',sep=""))
            break          
          }
          # Total number of notes in portfolio and new notes matching criteria
          users[[i]]$totalNotesFilteredPort <- users[[i]]$pre$portNoteCnt + users[[i]]$totalFilteredLoans
        }
        
        if (exists('gradeAllocation', where=users[[i]])) {
          # Determine number of notes allowed per grade in portfolio based on max note count
          users[[i]]$maxGradeTotalNotes <- round(users[[i]]$totalNotesFilteredPort*users[[i]]$gradeAllocation)
          users[[i]]$maxPerGrade <- users[[i]]$maxGradeTotalNotes - users[[i]]$pre$portGradeFreq$Freq
          users[[i]]$maxPerGrade <- ifelse(users[[i]]$maxPerGrade<0,0,users[[i]]$maxPerGrade)
               
          # Number of notes per grade
          users[[i]]$filteredGradeCnt <- table(users[[i]]$filteredLoans$grade)
          
          # Select appropriate notes per grade
          users[[i]]$gradeFilter <- do.call(rbind,lapply(LETTERS[1:7], getSample, 
                                         field = "grade",
                                         filteredCnt=users[[i]]$filteredGradeCnt, 
                                         maxPer=users[[i]]$maxPerGrade, i))
          
          users[[i]]$filteredIds <- users[[i]]$gradeFilter$id
          info(log,paste('User (',users[[i]]$name,') - Total filtered notes after grade allocation: ',length(users[[i]]$filteredIds),sep=""))
        }
        
        
        if (exists('termAllocation', where=users[[i]])) {
          # Determine number of notes allowed per term in portfolio based on max note count
          users[[i]]$maxTermNotes <- round(users[[i]]$totalNotesFilteredPort*users[[i]]$termAllocation)
          users[[i]]$maxPerTerm <- users[[i]]$maxTermNotes - users[[i]]$pre$portTermFreq$Freq
          users[[i]]$maxPerTerm <- ifelse(users[[i]]$maxPerTerm<0,0,users[[i]]$maxPerTerm)
          
          # Number of notes per term
          users[[i]]$filteredTermCnt <- table(users[[i]]$filteredLoans$term)
            
          # Select appropriate notes per term
          users[[i]]$termFilter <- do.call(rbind,lapply(c("36","60"), getSample, 
                                         field = "term",
                                         filteredCnt=users[[i]]$filteredTermCnt, 
                                         maxPer=users[[i]]$maxPerTerm, i))
          
          users[[i]]$filteredIds <- users[[i]]$termFilter$id
          info(log,paste('User (',users[[i]]$name,') - Total filtered notes after term allocation: ',length(users[[i]]$filteredIds),sep=""))
        }
        
        # Select notes that are common between gradeFilter and termFilter (must satisfy both rules)
        if (exists('gradeAllocation', where=users[[i]]) & exists('termAllocation', where=users[[i]]) ) {
          users[[i]]$combIds <- c(users[[i]]$gradeFilter$id,users[[i]]$termFilter$id)
          users[[i]]$filteredIds <- users[[i]]$combIds[duplicated(users[[i]]$combIds)]
          info(log,paste('User (',users[[i]]$name,') - Total filtered notes after grade and term allocation: ',length(users[[i]]$filteredIds),sep=""))
        }
        
        # If no notes after term and grade allocation filter, then break
        if (length(users[[i]]$filteredIds) < 1) {
          info(log,paste('User (',users[[i]]$name,') - No notes match filter criteria after allocation',sep=""))
          break
        }
        
        # Sort filtered ids again (in case allocation rearranged)
        tmp <- loans[loans$id %in% users[[i]]$filteredIds,]
        users[[i]]$filteredIds <- arrange(tmp,desc(tmp[[users[[i]]$sortField]]))$id

        # Set the maximum notes to order based on available cash and investment amount per note
        users[[i]]$maxNotesPerCash <- floor(users[[i]]$pre$cash / users[[i]]$amountPerNote)
        if (length(users[[i]]$filteredIds) > users[[i]]$maxNotesPerCash) {
          users[[i]]$filteredIds <- head(users[[i]]$filteredNotesSorted$id,users[[i]]$maxNotesPerCash)
          info(log,paste('User (',users[[i]]$name,') - Notes to order based on available cash: ',length(users[[i]]$filteredIds),sep=""))
        }
        
        # Limit maximum notes per order
        if(users[[i]]$maxNotesPerOrder < length(users[[i]]$filteredIds)) {
          users[[i]]$filteredIds <- head(users[[i]]$filteredIds,users[[i]]$maxNotesPerOrder)
          info(log,paste('User (',users[[i]]$name,') - Max notes per order: ',length(users[[i]]$filteredIds),sep=""))
        }
        
        users[[i]]$filteredIds <<- users[[i]]$filteredIds
        info(log,paste('User (',users[[i]]$name,') - Total notes to order: ',length(users[[i]]$filteredIds),sep=""))
        
        ##################
        ### Order code ###
        ##################

        # Create order JSON based on filtered Ids
        users[[i]]$order$aid <- users[[i]]$accID
        if (users[[i]]$portfolioId) {
          users[[i]]$order$orders <- data.frame(users[[i]]$filteredIds,
          users[[i]]$amountPerNote,
          users[[i]]$portfolioId)
          colnames(users[[i]]$order$orders) <- c('loanId','requestedAmount','portfolioId')
        } else {
          users[[i]]$order$orders <- data.frame(users[[i]]$filteredIds,
          users[[i]]$amountPerNote)
          colnames(users[[i]]$order$orders) <- c('loanId','requestedAmount')
        }
        users[[i]]$orderJSON <- toJSON(users[[i]]$order,auto_unbox=TRUE)

        
        # Time markers
        users[[i]]$endFilterTime <- proc.time()
        users[[i]]$elapsedProcTime <- round((users[[i]]$endFilterTime - startTime + elapsedModelTime)[3],2)
        users[[i]]$startOrderTime <- proc.time()

        # Order notes
        if (test == TRUE) {
          load('data/resultOrder.rda')
          users[[i]]$resultOrder <- resultOrder
        } else {
          stop('what the hell')
          users[[i]]$resultOrderJSON <- postForm(users[[i]]$urlOrders,.opts=list(postfields = users[[i]]$orderJSON,
                                                                                 httpheader = c('Authorization' = users[[i]]$token,
                                                                                                 'Accept' = "application/json",
                                                                                                 'Content-type' = "application/json")))
          if ( is.null(users[[i]]$resultOrderJSON) | length(users[[i]]$resultOrderJSON) == 0 ) {
            err(paste('User (',users[[i]]$name,') - Order Error (Empty API Response)',sep=""))
            err(paste('User (',users[[i]]$name,') - API Response: ', users[[i]]$resultOrderJSON,sep=''))
            break
          }
          if ( ! grep("orderInstructId",users[[i]]$resultOrderJSON) ) {
            err(paste('User (',users[[i]]$name,') - Order Error (Invalid API Resposne)',sep=""))
            err(paste('User (',users[[i]]$name,') - API Response: ', users[[i]]$resultOrderJSON,sep=''))
            break
          }
          users[[i]]$resultOrder <- fromJSON(users[[i]]$resultOrderJSON)
        }

        # Set variable that order was sent to LC
        users[[i]]$orderSent <<- 'true'
        
        users[[i]]$resultOrder$numOrderedNotes <- nrow(subset(users[[i]]$resultOrder$orderConfirmation, investedAmount>0))
        users[[i]]$resultOrder$investedAmount <- sum(users[[i]]$resultOrder$orderConfirmation$investedAmount)
        users[[i]]$resultOrder$requestedAmount <- sum(users[[i]]$resultOrder$orderConfirmation$requestedAmount)
        
        # Save result order to master namespace for reporting purposes
        users[[i]]$resultOrder <<- users[[i]]$resultOrder
        
        # Vector of notes ordered by loanId
        users[[i]]$notesOrderedIds <<- subset(users[[i]]$resultOrder$orderConfirmations,investedAmount > 0,select=c('loanId'))

        # Time markers
        users[[i]]$elapsedOrderTime <<- round((proc.time() - users[[i]]$startOrderTime)[3],2)
        users[[i]]$elapsedTotalTime <<- users[[i]]$elapsedProcTime + users[[i]]$elapsedOrderTime
        
        info(log,paste('User (',users[[i]]$name,') - Order submitted',sep=""))
        
        # 
        # # CSV report of new notes and auto invest actions
        # info(log,paste('User (',users[[i]]$name,') - Writing new loans CSV file',sep=""))
        # users[[i]]$newLoansCSV <- merge(users[[i]]$resultOrder$orderConfirmations, loans, by.x='loanId', by.y='id',all=TRUE)
        # users[[i]]$newLoansCSV$executionStatus <- gsub("NULL","",as.character(users[[i]]$newLoansCSV$executionStatus))
        # write.csv(users[[i]]$newLoansCSV,row.names=FALSE,na='',file=paste('reports/',gsub(' ','_',users[[i]]$name),'/new_listed_notes.csv',sep=''))
            
      }
    },mc.cores=cores)
    
    # Read 
    lastLog <- readLines(con)
    close(con)
    
    # Analyze portfolio for all users
    source('portfolio.R', local=TRUE)

    # Create report per user
    source('report.R', local=TRUE)
    
    # Shutdown server after 1 execution
    if (shutdown) system(shutdownCmd)
        
  } else {
    # Sleep between checking startTimes
    Sys.sleep(1)
  }
  
 
}



