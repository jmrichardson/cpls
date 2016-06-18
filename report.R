# Create email report per user

for (i in 1:length(users)) {
  
  # Skip users where order was not sent (no need to run portfolio analysis)
  if (is.null(users[[i]]$orderSent)) next
  
  # Load template html
  users[[i]]$html <- readChar('data/report.html', file.info('data/report.html')$size)

  # Update template html
  users[[i]]$html <- gsub('DATE',with_tz(now(),"America/Los_Angeles"),users[[i]]$html)
  users[[i]]$html <- gsub('USER',users[[i]]$name,users[[i]]$html)
  users[[i]]$html <- gsub('LISTTIME',listTime,users[[i]]$html)
  users[[i]]$html <- gsub('NEWNOTES',newNoteCount,users[[i]]$html)
  users[[i]]$html <- gsub('PRECASH',printCurrency(users[[i]]$pre$cash),users[[i]]$html)
  users[[i]]$html <- gsub('ORDEREDNOTES',printNumber(users[[i]]$resultOrder$numOrderedNotes),users[[i]]$html)
  users[[i]]$html <- gsub('INVESTAMOUNT',printCurrency(users[[i]]$resultOrder$investedAmount),users[[i]]$html)
  users[[i]]$html <- gsub('REQUESTAMOUNT',printCurrency(users[[i]]$resultOrder$requestedAmount),users[[i]]$html)
  users[[i]]$html <- gsub('PROCTIME',paste (users[[i]]$elapsedProcTime,'seconds'),users[[i]]$html)
  users[[i]]$html <- gsub('ORDERTIME',paste(users[[i]]$elapsedOrderTime,'seconds'),users[[i]]$html)
  users[[i]]$html <- gsub('TOTALTIME',paste (users[[i]]$elapsedTotalTime,'seconds'),users[[i]]$html)
  users[[i]]$html <- gsub('MAXPERORDER',users[[i]]$maxNotesPerOrder,users[[i]]$html)
  users[[i]]$html <- gsub('AMTPERNOTE',printCurrency(users[[i]]$amountPerNote),users[[i]]$html)
  users[[i]]$html <- gsub('DOWNTIME',paste(round(apiTimeElapse,2),"seconds"),users[[i]]$html)
  users[[i]]$html <- gsub('POSTCASH',printCurrency(users[[i]]$post$cash),users[[i]]$html)
  users[[i]]$html <- gsub('ACCOUNTTOTAL',printCurrency(users[[i]]$post$accountTotal),users[[i]]$html)
  users[[i]]$html <- gsub('INTACC',paste(round(users[[i]]$post$receivedInterest/users[[i]]$post$accountTotal*100,2),'%',sep=''),users[[i]]$html)
  users[[i]]$html <- gsub('RECINTEREST',printCurrency(users[[i]]$post$receivedInterest),users[[i]]$html)
  users[[i]]$html <- gsub('OUTPRINCIPAL',printCurrency(users[[i]]$post$outstandingPrincipal),users[[i]]$html)
  users[[i]]$html <- gsub('RECPRINCIPAL',printCurrency(users[[i]]$post$receivedPrincipal),users[[i]]$html)
  users[[i]]$html <- gsub('NOTESOWNED',printNumber(users[[i]]$post$totalNotes),users[[i]]$html)
  users[[i]]$html <- gsub('TOTALPORT',printNumber(users[[i]]$post$totalPortfolios),users[[i]]$html)
  users[[i]]$html <- gsub('FILTEREDNOTES',users[[i]]$numFilteredNotes,users[[i]]$html)
  
  # API order confirmation
  users[[i]]$table <- print(xtable(users[[i]]$resultOrder$orderConfirmations, digits=3), print.results=FALSE, type="html", include.rownames=F, html.table.attributes=NULL)
  users[[i]]$table <- gsub('<th','<th style="border-top:double; text-align:center; font-size:12px; font-style:italic; font-weight:normal; padding:0.1cm; border-bottom:1px solid black;" ',users[[i]]$table)
  users[[i]]$table <- gsub('<td','<td style="padding:0.1cm; font-size: 12px; text-align:left; vertical-align:to=p;text-align:center;" ',users[[i]]$table)
  users[[i]]$table <- gsub('<table','<table style="color:#6B6767; font-size: 12px; font-weight:normal;border-collapse:collapse; border:none;margin-left:auto; margin-right:auto;"" ',users[[i]]$table)
  users[[i]]$html <- gsub('ORDEREDREPORT',users[[i]]$table,users[[i]]$html)
  
  # Ordered notes report
  users[[i]]$orderedNotesDF <- loans[loans$id %in% users[[i]]$notesOrderedIds$loanId,users[[i]]$reportCriteria]
  if (nrow(users[[i]]$orderedNotesDF)>0) {
    users[[i]]$table <- print(xtable(users[[i]]$orderedNotesDF, digits=3), type="html", include.rownames=F, html.table.attributes=NULL)
    users[[i]]$table <- gsub('<th','<th style="border-top:double; text-align:center; font-size:12px; font-style:italic; font-weight:normal; padding:0.1cm; border-bottom:1px solid black;" ',users[[i]]$table)
    users[[i]]$table <- gsub('<td','<td style="padding:0.1cm; font-size: 12px; text-align:left; vertical-align:to=p;text-align:center;" ',users[[i]]$table)
    users[[i]]$table <- gsub('<table','<table style="color:#6B6767; font-size: 12px; font-weight:normal;border-collapse:collapse; border:none;margin-left:auto; margin-right:auto;"" ',users[[i]]$table)
    users[[i]]$html <- gsub('ORDEREDTABLE',users[[i]]$table,users[[i]]$html)
  } else {
    users[[i]]$html <- gsub('ORDEREDTABLE','No notes were ordered',users[[i]]$html)
  }
   
  
  # Create portfolio by Status
  users[[i]]$imgPortStatus <-'tmp/portfolio_by_status.png'
  png(users[[i]]$imgPortStatus,width=450,height=300)
  print(ggplot(data=users[[i]]$post$portStatusStats, aes(x=Status, y=Frequency, fill=Status)) + 
        geom_bar(colour="black", stat="identity",position=position_dodge(), size=.3) +  
        geom_text(aes(label = paste(Percent,"%",sep=''), y = Frequency+80), size = 3) +
        guides(fill=FALSE) + xlab("Loan Status") + ylab("Number of Notes Owned") +
        theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))
  dev.off()
  users[[i]]$htmlImgPortStatus <- '<img src="tmp/portfolio_by_status.png"/>'
  users[[i]]$html <- gsub('PORTSTATUS',users[[i]]$htmlImgPortStatus,users[[i]]$html)
   
  # Create portfolio by grade png per user
  users[[i]]$imgPortGrade <- 'tmp/portfolio_by_grade.png'
  # if (file.exists(users[[i]]$imgPortGrade)) file.remove(users[[i]]$imgPortGrade)
  png(users[[i]]$imgPortGrade,width=450,height=300)
  print(ggplot(data=users[[i]]$post$portGradeStats, aes(x=Grade, y=Frequency, fill=Grade)) + 
        geom_bar(colour="black", stat="identity",position=position_dodge(), size=.3) +  
        geom_text(aes(label = paste(Percent,"%",sep=''), y = Frequency+40), size = 3) +
        guides(fill=FALSE) + xlab("Note Grade") + ylab("Number of Notes Owned") + theme_bw())
  dev.off()
  users[[i]]$htmlImgPortGrade <- '<img src="tmp/portfolio_by_grade.png"/>'
  users[[i]]$html <- gsub('PORTGRADE',users[[i]]$htmlImgPortGrade,users[[i]]$html)
  
  # Create base64 image of portfolio by term
  users[[i]]$imgPortTerm <- 'tmp/portfolio_by_term.png'
  # if (file.exists(users[[i]]$imgPortTerm)) file.remove(users[[i]]$imgPortTerm)
  png(users[[i]]$imgPortTerm,width=400,height=250)
  print(pie3D(users[[i]]$post$portTermStats$Frequency,
        labels=paste(users[[i]]$post$portTermStats$Term," Months (",
                     printNumber(users[[i]]$post$portTermStats$Frequency), "/",
                     users[[i]]$post$portTermStats$Percent,"%)",sep=""),
        explode=0.1,
        height=.1,
        radius=1.5,
        # labelrad=1.4,
        labelcex=1.1,
        pty='m',
        mar=c(0,9,0,9)))
  dev.off()
  users[[i]]$htmlImgPortTerm <- '<img src="tmp/portfolio_by_term.png"/>'
  users[[i]]$html <- gsub('PORTTERM',users[[i]]$htmlImgPortTerm,users[[i]]$html)

  

  # Get log data  
  userIdx <- grep(users[[i]]$name,lastLog)
  sysIdx <- grep('User \\(',lastLog,invert=TRUE)
  userSys <- sort(unique(c(userIdx,sysIdx)))
  userLog <- lastLog[userSys]
  userLogData <- paste(userLog,collapse='<br>')
  users[[i]]$html <- gsub('INFOLOG',userLogData,users[[i]]$html)
  
  
  
  library(mailR)
  
  # Send portfolio report email
  if (exists('receipt')) rm(receipt)
  
  receipt <- send.mail(from = mailFrom,
    to = users[[i]]$email,
    # cc = c('jmrpublic@gmail.com'),
    subject = "Lending Club Investment Report",
    body = users[[i]]$html,
    html = TRUE,
    inline = TRUE,
    smtp = list(host.name = host, port = port, user.name = userName, passwd = userPasswd, ssl = ssl),
    authenticate = TRUE,
    send = TRUE)

  
  
  if (exists('receipt')) {
    info(log,paste('User (',users[[i]]$name,') - Email report sent',sep=""))
  } else {
    warn(log,paste('User (',users[[i]]$name,') - Email not sent',sep=""))
  }
  
}

