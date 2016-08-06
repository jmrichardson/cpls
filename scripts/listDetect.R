# Start list detection only in schedule mode
if ( opMode == 'schedule') {
  
  # Wait until X seconds before list to begin polling LC API
  # Check to make sure we are starting before scheduled start time (should never happen but checking anyways)
  if (!hmMin() %in% startTimes ) { 
    warn(log,'List detection cannot start after scheduled start time')
    next
  }
  # Sleep until just prior to start time
  startSec = 45
  if (second(nowPST())<startSec) info(log,'Waiting for list detection ...')
  while(second(nowPST())<startSec) {
    Sys.sleep(1)
  }
  
  info(log,"Starting new loan list detection")
  
  # Obtain starting note count (try multiple times)
  for (attempt in 1:3) {
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
    info(log,paste("Initial note count:",numPrev))
    break
  } 

  # List detection
  list=FALSE
  num <- maxNoteCount

  iter <- 0
  for (cnt in 1:num) {
    # Loop to wait 1 second between API calls
    while (TRUE) {
      if(proc.time()[3] > apiTimeStart+1) { 
        apiTimeStart <- proc.time()[3]
        newJson <- gURL(urlLoanList,users[[i]]$token)
        break
      }
    }
    

    if ( nchar(gsub("[[:blank:]]", "", newJson)) <= 1) {
      warn(log,paste("List detection (",cnt," of ",num,") - Empty API Response ",sep=''))
      next
    }
    if ( ! grepl("pubRec",newJson) ) {
      warn(log,paste("List detection (",cnt," of ",num,") - Invalid API response",sep=''))
      if(nchar(newJson)<=50) {
        warn(log,paste("API Response: ", newJson,sep=''))
      } else {
        warn(log,paste("API Response: ", substr(newJson, start=1, stop=50)," ...",sep=''))
      }
      next
    }
    loans = fromJSON(newJson)$loans
    if ( ! nrow(loans) ) {
        warn(log,paste("List detection (",cnt," of ",num,") - API Conversion Error",sep=''))
        next
    }
    nids <- loans$id
    newIds <- nids[! nids %in% prevIds]
    newNoteCount <- length(newIds)
    
    # Must have no previous notes in new notes, and greater than threshold to detect list
    if ( newNoteCount > numNotesThresh ) {
      list=TRUE
      listTime=with_tz(now(),"America/Los_Angeles")
      info(log,paste("List detected (New notes > ",numNotesThresh,") - New notes: ",newNoteCount,sep=''))
      break
    } else if (newNoteCount >= 1) {
      iter <- iter + 1
      if (iter>1) {
        list=TRUE
        listTime=with_tz(now(),"America/Los_Angeles")
        info(log,paste("List detected (Consecutive new notes) - New notes: ",newNoteCount,sep=''))
        break
      }
    } 
    info(log,paste("Detection ",cnt," of ",num," (New notes: ",newNoteCount,")",sep=''))
  }

  # Only continue if note list detected
  if(!list) { 
    warn(log,"New notes listing not detected")
    next
  }
}