# Log and save portfolio information including notes owned and account summary
# The information is used to send report as well

# Loop through users in sequence (not in parallel to avoid flooding LC API)
for (i in 1:length(users)) {

  # Skip users where order was not sent (no need to run portfolio analysis)
  if (is.null(users[[i]]$orderSent)) next
  

  # Obtain avaiable cash
  users[[i]]$attemptCash <- 1
  while(users[[i]]$attemptCash < 5) {
    users[[i]]$post$jsonCash <- getURL(paste("https://api.lendingclub.com/api/investor/",apiVersion,"/store/",users[[i]]$accID,"/availablecash",sep=''),
                                                                         httpheader = c('Authorization' = users[[i]]$token,
                                                                        'Accept' = "application/json",
                                                                        'Content-type' = "application/json"))
    if ( is.null(users[[i]]$post$jsonCash) | length(users[[i]]$post$jsonCash) == 0 ) {
      warn(log,paste('User (',users[[i]]$name,') - Unable to obtain post cash (Null API Response). Attempt: ',attempt,sep=""))
      users[[i]]$attemptCash <- users[[i]]$attemptCash + 1
      next
    }
    if(! grepl("availableCash",users[[i]]$post$jsonCash)) {
      warn(log,paste('User (',users[[i]]$name,') - Unable to obtain post cash (Invalid API Response). Attempt: ',attempt,sep=""))
      users[[i]]$attemptCash <- users[[i]]$attemptCash + 1
      next
    }
    users[[i]]$post$cash <- fromJSON(users[[i]]$post$jsonCash)$availableCash
    if(is.null(users[[i]]$post$cash) | length(users[[i]]$post$cash) == 0 ) {
      warn(log,paste('User (',users[[i]]$name,') - Unable to obtain post  cash (Non-Numeric API Response. Attempt: ',attempt,sep=""))
      users[[i]]$attemptCash <- users[[i]]$attemptCash + 1
      next
    } else {
      info(log,paste('User (',users[[i]]$name,') - Post cash available: ',printCurrency(users[[i]]$post$cash),sep=""))
      break
    }
  }
  


  # Account summary
  users[[i]]$attemptSummary <- 1
  while(users[[i]]$attemptSummary < 5){
    users[[i]]$post$jsonSummary <- getURL(paste("https://api.lendingclub.com/api/investor/",apiVersion,"/store/",users[[i]]$accID,"/summary",sep=''),
                                                                                    httpheader = c('Authorization' = users[[i]]$token,
                                                                                   'Accept' = "application/json",
                                                                                   'Content-type' = "application/json"))
    if ( is.null(users[[i]]$post$jsonSummary) | length(users[[i]]$post$jsonSummary) == 0 ) {
      warn(log,paste('User (',users[[i]]$name,') - Unable to obtain portfolio details. Attempt:',attempt,sep=""))
      users[[i]]$attemptSummary <- users[[i]]$attemptSummary + 1
      next
    }
    if(! grepl("receivedInterest",users[[i]]$post$jsonSummary)) {
      warn(log,paste('User (',users[[i]]$name,') - Unable to obtain portfolio summary. Attempt:',attempt,sep=""))
      users[[i]]$attemptSummary <- users[[i]]$attemptSummary + 1
      next
    }
    users[[i]]$post$accountTotal <- fromJSON(users[[i]]$post$jsonSummary)$accountTotal
    if(is.null(users[[i]]$post$accountTotal) | length(users[[i]]$post$accountTotal) == 0 ) {
      warn(log,paste('User (',users[[i]]$name,') - Unable to get portfolio summary. Attempt:',attempt,sep=""))
      users[[i]]$attemptSummary <- users[[i]]$attemptSummary + 1
      next
    } else {
      users[[i]]$post$accruedInterest <- fromJSON(users[[i]]$post$jsonSummary)$accruedInterest
      users[[i]]$post$receivedInterest <- fromJSON(users[[i]]$post$jsonSummary)$receivedInterest
      users[[i]]$post$outstandingPrincipal <- fromJSON(users[[i]]$post$jsonSummary)$outstandingPrincipal
      users[[i]]$post$receivedPrincipal <- fromJSON(users[[i]]$post$jsonSummary)$receivedPrincipal
      users[[i]]$post$totalNotes <- fromJSON(users[[i]]$post$jsonSummary)$totalNotes
      users[[i]]$post$totalPortfolios <- fromJSON(users[[i]]$post$jsonSummary)$totalPortfolios 
      info(log,paste('User (',users[[i]]$name,') - Received Interest: $',users[[i]]$post$receivedInterest,sep=""))
      break
    }
  }

  # Obtain current portfolio grade frequency and total note count
  users[[i]]$attemptNotesOwned <- 1
  while(users[[i]]$attemptNotesOwned < 5) {
    users[[i]]$post$jsonNotesOwned <- getURL(paste("https://api.lendingclub.com/api/investor/",apiVersion,"/store/",users[[i]]$accID,"/detailednotes",sep=''),
                                                                                          httpheader = c('Authorization' = users[[i]]$token,
                                                                                         'Accept' = "application/json",
                                                                                         'Content-type' = "application/json"))
    if ( is.null(users[[i]]$post$jsonNotesOwned) | length(users[[i]]$post$jsonNotesOwned) == 0 ) {
      warn(log,paste('User (',users[[i]]$name,') - Problem obtaining notes owned. Attempt:',attempt,sep=""))
      users[[i]]$attemptSummary <- users[[i]]$attemptSummary + 1
      next
    }
    if(! grepl("loanStatus",users[[i]]$post$jsonNotesOwned)) {
      warn(log,paste('User (',users[[i]]$name,') - Unable to obtain notes owned. Attempt:',attempt,sep=""))
      users[[i]]$attemptSummary <- users[[i]]$attemptSummary + 1
      next
    }
    users[[i]]$post$notesOwned <- fromJSON(users[[i]]$post$jsonNotesOwned)$myNotes
    if(is.null(users[[i]]$post$notesOwned) | length(users[[i]]$post$notesOwned) == 0 ) {
      warn(log,paste('User (',users[[i]]$name,') - Unable to get notes owned. Attempt:',attempt,sep=""))
      users[[i]]$attemptSummary <- users[[i]]$attemptSummary + 1
      next
    } else {
  
      users[[i]]$post$notesOwned$grade <- as.factor(substring(users[[i]]$post$notesOwned$grade, 1, 1))
      users[[i]]$post$notesOwned$grade <- factor(users[[i]]$post$notesOwned$grade,levels=c("A","B","C","D","E","F","G"))
      users[[i]]$post$notesOwned$loanLength <- factor(users[[i]]$post$notesOwned$loanLength,levels=c("36","60"))
      
      users[[i]]$post$portGradeFreq <- as.data.frame(xtabs( ~ grade, users[[i]]$post$notesOwned))
      users[[i]]$post$portGradePerc <- round(prop.table(users[[i]]$post$portGradeFreq$Freq)*100,0)
      users[[i]]$post$portGradeStats <- cbind(users[[i]]$post$portGradeFreq,users[[i]]$post$portGradePerc)
      names(users[[i]]$post$portGradeStats) <- c("Grade","Frequency","Percent")
      
      users[[i]]$post$portStatusFreq <- as.data.frame(xtabs( ~ loanStatus, users[[i]]$post$notesOwned))
      users[[i]]$post$portStatusPerc <- round(prop.table(users[[i]]$post$portStatusFreq$Freq)*100,0)
      users[[i]]$post$portStatusStats <- cbind(users[[i]]$post$portStatusFreq,users[[i]]$post$portStatusPerc)
      names(users[[i]]$post$portStatusStats) <- c("Status","Frequency","Percent")
      
      users[[i]]$post$portTermFreq <- as.data.frame(xtabs( ~ loanLength, users[[i]]$post$notesOwned))
      users[[i]]$post$portTermPerc <- round(prop.table(users[[i]]$post$portTermFreq$Freq)*100,0)
      users[[i]]$post$portTermStats <- cbind(users[[i]]$post$portTermFreq,users[[i]]$post$portTermPerc)
      users[[i]]$post$portNoteCnt <- dim(users[[i]]$post$notesOwned)[1]
      names(users[[i]]$post$portTermStats) <- c("Term","Frequency","Percent")
      info(log,paste('User (',users[[i]]$name,') - Notes owned: ',nrow(users[[i]]$post$notesOwned),sep="")) 
      break
    }
  }

  
  # info(log,paste('User (',users[[i]]$name,') - Post portfolio complete',sep=""))
}

