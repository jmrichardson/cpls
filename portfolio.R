# Log and save portfolio information including notes owned and account summary
# The information is used to send report as well

# Loop through users in sequence (not in parallel to avoid flooding LC API)
for (i in 1:length(users)) {

  # Skip users where order was not sent (no need to run portfolio analysis)
  if (users[[i]]$orderSent == 'no' ) next
  

  # Obtain avaiable cash
  users[[i]]$attemptCash <- 1
  while(users[[i]]$attemptCash < 5) {
    users[[i]]$post$jsonCash <- gURL(paste("https://api.lendingclub.com/api/investor/",apiVersion,"/accounts/",users[[i]]$accID,"/availablecash",sep=''),users[[i]]$token)
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
    users[[i]]$post$jsonSummary <- gURL(paste("https://api.lendingclub.com/api/investor/",apiVersion,"/accounts/",users[[i]]$accID,"/summary",sep=''),users[[i]]$token)
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


  # load portfolio allocation script
  phase <- 'post'
  source('portfolioAlloc.R')
  

  
  # info(log,paste('User (',users[[i]]$name,') - Post portfolio complete',sep=""))
}

