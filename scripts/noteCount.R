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
  break
}
