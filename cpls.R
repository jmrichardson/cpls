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
library(tools)
library(xgboost)

# Function to set home directory
defaultDir = '/home/user/cpls'
csf <- function() {
    cmdArgs = commandArgs(trailingOnly = FALSE)
    needle = "--file="
    match = grep(needle, cmdArgs)
    if (length(match) > 0) {
        # Rscript via command line
        return(normalizePath(sub(needle, "", cmdArgs[match])))
    } else {
        ls_vars = ls(sys.frames()[[1]])
        if ("fileName" %in% ls_vars) {
            # Source'd via RStudio
            return(normalizePath(sys.frames()[[1]]$fileName)) 
        } else {
            if (!is.null(sys.frames()[[1]]$ofile)) {
            # Source'd via R console
            return(normalizePath(sys.frames()[[1]]$ofile))
            } else {
                # RStudio Run Selection
                return(normalizePath(rstudioapi::getActiveDocumentContext()$path))
            }
        }
    }
}
dir <- tryCatch(dirname(csf()),
  error = function(e) {
    defaultDir
  }
)
if (is.null(dir) | length(dir) == 0) {
  dir <- defaultDir
}
if(!dir.exists(dir)) {
  err('Unable to determine home directory')
} else {
  setwd(dir)
}


# Load helper functions
source('funcs.R')

# Initialize log
logFile='store/system.log'
log <- create.logger(level='INFO',logfile=logFile)
info(log,'-------------------------------------')
info(log,'Starting Command Line PLS Version 1.0')

# Set program operation mode (default is schedule)
opMode <- 'schedule'
args<-commandArgs(TRUE)
if(!is.na(args[1])) {
  if(args[1]=='schedule' | args[1]=='runOnce' | args[1]=='test' ) { 
    opMode <- args[1] 
  } else {
    print('asdf')
    err('Invalid operation mode argument: Options are schedule, runOnce, test')
  }
}
### Overide opMode for testing purposes
# Run once without list detection
#
# Exit after modeling notes
opMode <- 'model'
#
# Run in test mode (desciption update later)
# opMode <- 'test'
#
# Normal operation
# opMode <- 'schedule'
##########################
info(log,paste('Operation Mode:',opMode))

# Create directories if they don't exist
dir.create('store', showWarnings = FALSE)
dir.create('tmp', showWarnings = FALSE)

# Software load process (includes model and users)
info(log,'Loading software configuration')
source('load.R')

info(log,'Loading zip code database')
source('zip.R')

info(log,'Loading FRED data')
source('fred.R')

# info(log,'Loading historical notes')
# load('data/data.rda')

# Start continous loop
loopCount <- 0
while (1) {
  
  # On initial loop, log schedule start times
  if (loopCount == 0) {
    if(opMode=='schedule') {
      for (time in startTimes) {
        info(log,paste('Configured start time:',time,'PDT'))
      }
      info(log,'Waiting for next scheduled start time ...')
    }
  }

  # Start system only at given start times (1 minute prior) or if running tests
  if ((hmMin() %in% startTimes & opMode=='schedule') | opMode != 'schedule') {
        
    # Read log file (connection)
    closeAllConnections()
    con <- file(logFile,"r")
    seek(con,where=0,origin='end')
    
    # Obtain initial cash for each user (Try multiple times)
    for (i in 1:length(users)) {
      cashFlag <- TRUE
      for (attempt in 1:3) {
        users[[i]]$pre$jsonCash <- gURL(paste("https://api.lendingclub.com/api/investor/",apiVersion,"/accounts/",users[[i]]$accID,"/availablecash",sep=''),users[[i]]$token)
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
        cashFlag <- FALSE
        info(log,paste('User (',users[[i]]$name,') - Initial cash available: ',printCurrency(users[[i]]$pre$cash),sep=""))
        break
      }
    }
    
    # Error checking
    if (cashFlag) {
      warn(log,'Unable to get initial user cash')
      next
    }

    # If using allocation distribution, load allocation
    if (exists('gradeAllocation', where=users[[i]]) | exists('termAllocation', where=users[[i]]) ) {
      phase <- 'pre'
      source('portfolioAlloc.R')  
    }
    
    # Get current platform note count (try multiple times)
    for (attempt in 1:3) {
      newJson <- gURL(urlLoanListAll,users[[i]]$token)
      if ( is.null(newJson) | length(newJson) == 0 ) {
        warn(log,paste("Note Count Attempt (",attempt," of 3) - Null API Response ",sep=''))
        next
      }
      if ( ! grepl("pubRec",newJson) ) {
        warn(log,paste("List detection (",attempt," of 3) - Invalid API response",sep=''))
        next
      }
      loans = fromJSON(newJson)$loans
      if ( ! nrow(loans) ) {
          warn(log,paste("List detection (",attempt," of 3) - API Conversion Error",sep=''))
          next
      }
      noteCount <- dim(loans)[1]
      if (!is.numeric(noteCount)) {
        warn(log,'Unable to obtain platform note count')
      } else {
        info(log,paste('Platform note count:',noteCount))
      }
      # set some defaults for reporting to work in test mode
      listTime=with_tz(now(),"America/Los_Angeles")
      newNoteCount=0
      apiTimeElapse=0
      break
    }
      
    # Start list detection only in schedule mode
    if ( opMode == 'schedule') {
      
      # Wait until X seconds before list to begin polling LC API
      # Check to make sure we are starting before scheduled start time (should never happen but checking anyways)
      if (!hmMin() %in% startTimes ) { 
        warn(log,'List detection cannot start after scheduled start time')
        next
      }
      # Sleep until just prior to start time
      if (second(nowPST())<50) info(log,'Waiting for list detection ...')
      while(second(nowPST())<50) {
        Sys.sleep(1)
      }
      
      info(log,"Starting loan list detection")
      
      # Obtain starting note count (try multiple times)
      for (attempt in 1:3) {
        startJson <- gURL(urlLoanList,users[[i]]$token)
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
      num <- maxNoteCount

      for (cnt in 1:num) {
        # Loop to wait 1 second between API calls
        while (TRUE) {
          if(proc.time()[3] > apiTimeStart+1) { 
            apiTimeStart <- proc.time()[3]
            newJson <- gURL(urlLoanList,users[[i]]$token)
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
  
      # Only continue if note list detected
      if(!list) { 
        warn(log,"New notes listing not detected")
        next
      }
    }

    # Load old batch of notes for test purposes only (ensures that my filter selects some notes on this batch)
    if (opMode == 'test') {
      loans <- read.csv('data/loans_sample.csv')
    } else if (opMode == 'model') {
      newJson <- gURL(urlLoanList,users[[i]]$token)
      loans = fromJSON(newJson)$loans
    }
    
    
    info(log,'Modeling available notes')
    # Record start of modeling time
    # startModelTime <- proc.time()
    
    # Add zip code data
    loans <- merge(x=loans,y=zip,by="addrZip",all.x=TRUE)
    
    # Add FRED data
    loans <- cbind(loans,lastFred[rep(1,nrow(loans)),])
    
    # Feature engineering
    loans$earliestCrLine <- ymd(substring(loans$earliestCrLine,1,10))
    loans$n=ymd(Sys.Date())
    loans$earliestCrLineMonths=as.integer(round((loans$n - loans$earliestCrLine)/30.4375)-1)
    loans$amountTerm <- loans$loanAmount/loans$term
    loans$amountTermIncomeRatio=loans$amountTerm/(loans$annualInc/12)
    loans$revolBalAnnualIncRatio=loans$revolBal/loans$annualInc

    # Add model probability to each loan  
    loans$model <- predict(xgbModel, data.matrix(data.frame(predict(dmy, newdata=loans[,featureNames]))), missing=NA)
      
    # elapsedModelTime <- round(proc.time() - startModelTime[3],2)

    # Record start time of selection process per user
    startTime <- proc.time()

    if (opMode == 'model') {
      info(log, 'Model complete')
      stop()
    }

    # Process each user account
    info(log,'Processing all user accounts')
    result <- mclapply(1:length(users),function(i){
      
      # Simple loop to stop execution on error
      for(one in 1) {
        
        # Set order sent flag
        users[[i]]$orderSent <<- 'no'
        
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
            warn(log,paste('User (',users[[i]]$name,') - No portfolio information or 0 notes owned (stopping)',sep=""))
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
        users[[i]]$order <- list()
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
        users[[i]]$elapsedProcTime <<- round((users[[i]]$endFilterTime - startTime + elapsedModelTime)[3],2)
        users[[i]]$startOrderTime <- proc.time()

        # Order notes
        if (opMode == 'test') {
          load('data/resultOrder.rda')
          users[[i]]$resultOrder <- resultOrder
        } else {
          # Submit order to LC
          users[[i]]$resultOrderJSON <- postForm(paste("https://api.lendingclub.com/api/investor/",apiVersion,"/accounts/",users[[i]]$accID,"/orders",sep=''),
            .opts=list(postfields = users[[i]]$orderJSON,
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
        users[[i]]$orderSent <<- 'yes'
        
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

    # Loans percent funded
    loans$n=NULL
    loans$pctFunded = loans$fundedAmount / loans$loanAmount
    avgPctFunded <- loans$pctFunded
    timeStampFile = gsub(" ","_",gsub(":","-",listTime))
    
    # Analyze portfolio for all users
    source('portfolio.R', local=TRUE)

    # Read 
    lastLog <- readLines(con)
    close(con)
    

    # Create report per user
    source('report.R', local=TRUE)
    
    # Save the loans for testing purposes
    # write.csv(loans,row.names=FALSE,na='',file=paste('store/',gsub(':','-',listTime),' loans.csv',sep=''))

    if (opMode == 'runOnce' | opMode == 'test') {
      info(log,'Operation complete')
      break
    }
    
    # Shutdown server after 1 execution (Experimental)
    if (shutdown) system(shutdownCmd)
    
    if(opMode=='schedule') {
      info(log,'Waiting for next scheduled start time ...')
    }
    
  } else {
    # Sleep between checking startTimes
    Sys.sleep(5)
    
    # Increment loopCount
    loopCount <- loopCount+1
    
    # Load configuration if checksums are different or new number of accounts
    if (any(md5sum(sort(c(files,config)))!=checkSums) | length(files) != length(list.files(path="store", pattern="*.acc", full.names=T, recursive=FALSE))) {
      info(log,'Configuration change detected.  Reloading ...')
      source('load.R')
      loopCount <- 0
    }

    

  }
  
 
}



