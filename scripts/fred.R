# Get St Louis FRED data

df <- read.csv('data/PLS_Daily.txt',sep='\t')
df$SP500 <- suppressWarnings(as.numeric(as.character(df$SP500)))
issue_d = as.Date(strftime(df$DATE, "%Y-%m-01"))
daily = aggregate(SP500 ~ issue_d, FUN = mean, data=df)

df <- read.csv('data/PLS_Weekly_Ending_Friday.txt',sep='\t')
df$STLFSI <- as.numeric(as.character(df$STLFSI))
issue_d = as.Date(strftime(df$DATE, "%Y-%m-01"))
weeklyF = aggregate(STLFSI ~ issue_d, FUN = mean, data=df)

df <- read.csv('data/PLS_Weekly_Ending_Wednesday.txt',sep='\t')
df$FF <- as.numeric(as.character(df$FF))
issue_d = as.Date(strftime(df$DATE, "%Y-%m-01"))
weeklyW = aggregate(FF ~ issue_d, FUN = mean, data=df)

df <- read.csv('data/PLS_Monthly.txt',sep='\t')
df$issue_d = as.Date(strftime(df$DATE, "%Y-%m-01"))
df$DATE <- NULL
monthly <- df

# Capture all data in one data frame for Model (merged with data by issue_d)
allFred <- merge(x=daily,y=weeklyF,by="issue_d",all=TRUE)
allFred <- merge(x=allFred,y=weeklyW,by="issue_d",all=TRUE)
allFred <- merge(x=allFred,y=monthly,by="issue_d",all=TRUE)

# Get last value of each column to make predictions
lastFred <- apply(allFred[,-1],2,function(x) as.numeric(tail(na.omit(x), 1)))
lastFred <- data.frame(t(data.frame(lastFred)))
