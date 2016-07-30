# # 
# # # CSV report of new notes and auto invest actions
# # info(log,paste('User (',users[[i]]$name,') - Writing new loans CSV file',sep=""))
# users[[i]]$newLoansCSV <- merge(users[[i]]$resultOrder$orderConfirmations, loans, by.x='loanId', by.y='id',all=TRUE)
# users[[i]]$newLoansCSV$executionStatus <- gsub("NULL","",as.character(users[[i]]$newLoansCSV$executionStatus))
# write.csv(users[[i]]$newLoansCSV,row.names=FALSE,na='',file=paste('store/',gsub(' ','_',users[[i]]$name),'_new_listed_notes.csv',sep=''))
