# Get current platform note count (not required but nice to have)
for (attempt in 1:3) {
  newJson <- gURL(urlLoanListAll,users[[i]]$token)
  if ( nchar(gsub("[[:blank:]]", "", newJson)) <= 1) {
    warn(log,paste("Note Count Attempt (",attempt," of 3) - Empty API Response ",sep=''))
    next
  }
  if ( ! grepl("asOfDate",newJson) ) {
    warn(log,paste('Unable to obtain platform note count (Invalid API Response). Attempt: ',attempt,sep=""))
    if(nchar(startJson)<=50) {
      warn(log,paste("API Response: ", newJson,sep=''))
    } else {
      warn(log,paste("API Response: ", substr(newJson, start=1, stop=50)," ...",sep=''))
    }
    next
  }
  loans = fromJSON(newJson)$loans$id
  noteCount <- length(loans)
  info(log,paste('Platform note count:',noteCount))
  break
}
