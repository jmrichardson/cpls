
# Load pre-built GBM model
info(log,'Loading machine learning model')
load('data/fitGbm.rda')

# Load cpls configuration
config <- 'store/config.R'
reqFile(config)
info(log,'Importing configuration')
source(config)
checkConfig()

# Load all users from store sub-directory (must end with .acc extension)
info(log,'Loading user accounts')
users <- list()
ls <- read.csv('data/loans_sample.csv')
files <- sort(list.files(path="store", pattern="*.acc", full.names=T, recursive=FALSE))
for (file in files) {
  lc=list()
  source(file)
  checkUser(file,lc)
  
  # Check filter syntax
  res <- tryCatch({
    ls %>% lc$filterCriteria()
  }, error = function(e) {
    err(paste('User (',lc$name,') - Filter error',sep=''))
  })
  
  
  users <- append(users,list(lc))
  info(log,paste("Importing user: ",lc$name,sep=''))
}
if (length(users)==0) {
  err('No user accounts configured')
}

checkSums <- md5sum(sort(c(files,config)))
