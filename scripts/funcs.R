
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
  res <- tryCatch(getURL(url,
    httpheader = c('Authorization' = token,
    # ssl.verifypeer = FALSE,
    # ssl.verifyhost = FALSE,
    'Accept' = "application/json",
    'Content-type' = "application/json"))
  , error = function(e) {
    warn(log,paste('User (',users[[i]]$name,') - ',e,sep=""))
    ""
  })
    return(res)
}



###############################################################################
### Advanced configuration below - Do NOT MODIFY
###############################################################################

# Loan Archive
archive <- 'store/loanArchive.rda'

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
    'port','userName','userPasswd','ssl')
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

# XIRR Functions
sppv <- function (i, n) {
  return((1 + i/100)^(-n))
}

npv <- function(x, i) {
  npv = c()
  for (k in 1:length(i)) {
    pvs = x * sppv(i[k], 1:length(x))
    npv = c(npv, sum(pvs))
  }
  return(npv)
}

xIRR <- function (df) {

  df=subset(df,Payment!=0)
  df=aggregate(Payment ~ Month, data=df, sum)
  cashflow = df$Payment
  dates = df$Month
  numDays <<- as.numeric(max(dates) - min(dates))
  if(numDays>365) {
    numDays = 365 
  }
  
  cashflow_adj <- c(cashflow[1])
  for (i in 1:(length(cashflow)-1)) {
    d1 <- as.Date(dates[i])
    d2 <- as.Date(dates[i+1])
    # There are no checks about the monotone values of dates
    interval <- as.integer(d2 - d1)
    cashflow_adj <- c(cashflow_adj, rep(0, interval-1), cashflow[i+1])
  }
  cashflow_adj <<- cashflow_adj


  # Bisection method finding the rate to zero npv
  left = -30
  right = 30
  epsilon = 1e-8
  while (abs(right-left) > 2*epsilon) {
    if(is.nan(npv(cashflow_adj, left))) {
      left = left+1
      next
    }
    midpoint = (right+left)/2
    if(is.nan(npv(cashflow_adj, midpoint))) {
      right = right-1
      next
    }
    if (npv(cashflow_adj, left) * npv(cashflow_adj, midpoint) > 0) {
      left = midpoint
    } else {
      right = midpoint
    }
  }

  irr = (right+left) / 2 / 100 
  irr <- (1 + irr) ^ numDays - 1
  round(irr,4)
}


