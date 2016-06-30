# This is a script file because it is used twice (pre and post)

# if (phase == 'pre') {
#   info(log,paste('User (',users[[i]]$name,') - Obtaining current portfolio allocation',sep=""))
# } else {
#   info(log,paste('User (',users[[i]]$name,') - Obtaining post order portfolio information',sep=""))
# }

# Obtain current portfolio grade frequency and total note count
users[[i]]$attemptNotesOwned <- 1
while(users[[i]]$attemptNotesOwned < 5) {
  users[[i]][[phase]]$jsonNotesOwned <- gURL(paste("https://api.lendingclub.com/api/investor/",apiVersion,"/accounts/",users[[i]]$accID,"/detailednotes",sep=''),users[[i]]$token)
  if ( is.null(users[[i]][[phase]]$jsonNotesOwned) | length(users[[i]][[phase]]$jsonNotesOwned) == 0 ) {
    warn(log,paste('User (',users[[i]]$name,') - Problem obtaining notes owned. Attempt:',attempt,sep=""))
    users[[i]]$attemptSummary <- users[[i]]$attemptSummary + 1
    next
  }
  if(! grepl("loanStatus",users[[i]][[phase]]$jsonNotesOwned)) {
    warn(log,paste('User (',users[[i]]$name,') - Unable to obtain notes owned. Attempt:',attempt,sep=""))
    users[[i]]$attemptSummary <- users[[i]]$attemptSummary + 1
    next
  }
  users[[i]][[phase]]$notesOwned <- fromJSON(users[[i]][[phase]]$jsonNotesOwned)$myNotes
  if(is.null(users[[i]][[phase]]$notesOwned) | length(users[[i]][[phase]]$notesOwned) == 0 ) {
    warn(log,paste('User (',users[[i]]$name,') - Unable to get notes owned or 0 notes owned. Attempt:',attempt,sep=""))
    users[[i]]$attemptSummary <- users[[i]]$attemptSummary + 1
    next
  } else {

    users[[i]][[phase]]$jsonNotesOwned <- NULL
    users[[i]][[phase]]$notesOwned$grade <- as.factor(substring(users[[i]][[phase]]$notesOwned$grade, 1, 1))
    users[[i]][[phase]]$notesOwned$grade <- factor(users[[i]][[phase]]$notesOwned$grade,levels=c("A","B","C","D","E","F","G"))
    users[[i]][[phase]]$notesOwned$loanLength <- factor(users[[i]][[phase]]$notesOwned$loanLength,levels=c("36","60"))
    
    users[[i]][[phase]]$portGradeFreq <- as.data.frame(xtabs( ~ grade, users[[i]][[phase]]$notesOwned))
    users[[i]][[phase]]$portGradePerc <- round(prop.table(users[[i]][[phase]]$portGradeFreq$Freq)*100,0)
    users[[i]][[phase]]$portGradeStats <- cbind(users[[i]][[phase]]$portGradeFreq,users[[i]][[phase]]$portGradePerc)
    names(users[[i]][[phase]]$portGradeStats) <- c("Grade","Frequency","Percent")
    
    users[[i]][[phase]]$portStatusFreq <- as.data.frame(xtabs( ~ loanStatus, users[[i]][[phase]]$notesOwned))
    users[[i]][[phase]]$portStatusPerc <- round(prop.table(users[[i]][[phase]]$portStatusFreq$Freq)*100,0)
    users[[i]][[phase]]$portStatusStats <- cbind(users[[i]][[phase]]$portStatusFreq,users[[i]][[phase]]$portStatusPerc)
    names(users[[i]][[phase]]$portStatusStats) <- c("Status","Frequency","Percent")
    
    users[[i]][[phase]]$portTermFreq <- as.data.frame(xtabs( ~ loanLength, users[[i]][[phase]]$notesOwned))
    users[[i]][[phase]]$portTermPerc <- round(prop.table(users[[i]][[phase]]$portTermFreq$Freq)*100,0)
    users[[i]][[phase]]$portTermStats <- cbind(users[[i]][[phase]]$portTermFreq,users[[i]][[phase]]$portTermPerc)
    users[[i]][[phase]]$portNoteCnt <- dim(users[[i]][[phase]]$notesOwned)[1]
    names(users[[i]][[phase]]$portTermStats) <- c("Term","Frequency","Percent")
    info(log,paste('User (',users[[i]]$name,') - Total notes owned: ',nrow(users[[i]][[phase]]$notesOwned),sep="")) 
    users[[i]][[phase]]$notesOwned <- NULL
    break
  }
}