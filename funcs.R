
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


