# Obtain initial cash for each user
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

# If unable to get cash, log warning and not continue until next scheduled run
if (cashFlag) {
  warn(log,'Unable to get initial user cash')
  next
}
