# Start list detection only in schedule mode
if ( opMode == 'schedule') {
  
  # Wait until X seconds before list to begin polling LC API
  # Check to make sure we are starting before scheduled start time (should never happen but checking anyways)
  if (!hmMin() %in% startTimes ) { 
    warn(log,'List detection cannot start after scheduled start time')
    next
  }
  # Sleep until just prior to start time
  if (second(nowPST())<45) info(log,'Waiting for list detection ...')
  while(second(nowPST())<45) {
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