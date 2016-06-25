
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
    err('Configuration file does not exist: store/config.R')
  }
}


# Function for getURL error handling
gURL <- function(url,token) {
  status <- tryCatch(getURL(url,
                           httpheader = c('Authorization' = token,
                           'Accept' = "application/json",
                           'Content-type' = "application/json")),
             error = function(e) {
               warn(log,status)
            })
  return(status)
}

# Function for postForm error handling
pForm <- function(url,token,orderJSON) {
  status <- tryCatch(postFORM(url,
                              .opts=list(postfields = orderJSON,
                              httpheader = c('Authorization' = token,
                              'Accept' = "application/json",
                              'Content-type' = "application/json"))),
             error = function(e) {
               warn(log,status)
            })
  return(status)
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



