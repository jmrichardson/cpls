
# Helper function to view structures in RStudio window
more <- function(x) {
  file <- tempfile()
  sink(file); on.exit(sink())
  print(x)
  file.show(file, delete.file = T)
}

# Currency formatter function
printCurrency <- function(value, currency.sym="$", digits=2, sep=",", decimal=".") {
  paste(currency.sym,
        formatC(value, format = "f", big.mark = sep, digits=digits, decimal.mark=decimal),sep="")
}

# Number formatter function
printNumber <- function(value, digits=0, sep=",", decimal=".") {
  formatC(value, format = "f", big.mark = sep, digits=digits, decimal.mark=decimal)
}

# Sample function (used by term and grade percent maximum)
getSample <- function(val,field,filteredCnt,maxPer,i) {
  cnt <- filteredCnt[[val]]
  if(!cnt) return()
  max <- maxPer[[val]]
  if(!max) return()
  sel=ifelse(max<=cnt,max,cnt)
  if (sel) {
    sub <- subset(users[[i]]$filteredLoans,users[[i]]$filteredLoans[[field]] == val)
    sub[sample(nrow(sub), sel), ][1]
  }
}

# Error handling function
err <- function(str) {
  error(log,str)
  stop(str, call.=FALSE) 
}

# Check for file existance
reqFile <- function(file) {
  if (!file.exists(file)) {
    err(paste('Required file does not exist:',file))
  }
}


# Function for getURL error handling
gURL <- function(url,token) {
  res <- getURL(url,
    httpheader = c('Authorization' = token,
    'Accept' = "application/json",
    'Content-type' = "application/json"))
  return(res)
}



###############################################################################
### Advanced configuration below - Do NOT MODIFY
###############################################################################

# How many cores/threads to use (1 per user).  Defaults to all cores
# Windows can only have 1 core
cores = detectCores()
if (.Platform$OS.type == 'windows') {
  cores = 1
  shutdownCmd = "shutdown /t 0 /s"
}
  

# LC API Version
apiVersion = "v1"

# API URL
urlLoanList = paste("https://api.lendingclub.com/api/investor/", 
                    apiVersion, 
                    "/loans/listing",
                    sep='')

urlLoanListAll = paste("https://api.lendingclub.com/api/investor/", 
                    apiVersion, 
                    "/loans/listing?showAll=true",
                    sep='')


# Curl options
options(RCurlOptions = list(verbose = FALSE, 
                            followlocation = TRUE, 
                            autoreferer = TRUE,
                            ssl.verifypeer = FALSE,
                            useragent = "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/30.0.1599.101 Safari/537.36")
)

# Check config.R
checkConfig <- function() {
  parms <- c('startTimes','maxNoteCount','numNotesThresh','mailFrom','host',
    'port','userName','userPasswd','ssl','shutdown','shutdownCmd')
  for (parm in parms) {
    if(!exists(parm)){
      err(paste("Config.R parameter does not exist:",parm))
    }
  }
}

# Check user account
checkUser <- function(file,lc) {
  parms <- c('name','email','accID','token','maxNotesPerOrder','minCash','portfolioId','amountPerNote',
    'sortField','reportCriteria','filterCriteria')
  for (parm in parms) {
    if(!exists(parm,where=lc)){
      err(paste("User parameter in",file,"does not exist:",parm))
    }
  }
}

# Get California time
nowPST <- function() {
  with_tz(now(),"America/Los_Angeles")
  # parse_date_time('2016-07-04 09:59:00 PDT','Y-m-d H:M:S', tz='America/Los_Angeles')
}

# Get hour and minute of California time plus 1 minute
hmMin <- function() {
  nowMin <- nowPST()+minutes(1)
  paste(str_pad(hour(nowMin),2,pad="0"),str_pad(minute(nowMin),2,pad="0"),sep=":")
}


