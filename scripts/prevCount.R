# Obtain starting note count and Ids from previous list
flag=FALSE
for (attempt in 1:5) {
  startJson <- gURL(urlLoanList,users[[i]]$token)
  if ( nchar(gsub("[[:blank:]]", "", startJson)) <= 1) {
    warn(log,paste('Unable to obtain initial note count (Empty API Response). Attempt: ',attempt,sep=""))
    next
  }
  if ( ! grepl("pubRec",startJson) ) {
    warn(log,paste('Unable to obtain initial note count (Invalid API Response). Attempt: ',attempt,sep=""))
    if(nchar(startJson)<=50) {
      warn(log,paste("API Response: ", startJson,sep=''))
    } else {
      warn(log,paste("API Response: ", substr(startJson, start=1, stop=50)," ...",sep=''))
    }
    next
  }
  prevIds <- fromJSON(startJson)$loans$id
  if ( ! length(prevIds) ) {
    warn(log,paste('Unable to obtain initial note count (API Conversion Error). Attempt: ',attempt,sep=""))
    next
  }
  numPrev <- length(prevIds)
  info(log,paste("Previous note count:",numPrev))
  flag=TRUE
  break
} 

# Previous note ids ARE required to determine new notes
if(!flag) { 
  warn(log,"Unable to obtain previous notes listing")
  next
}
