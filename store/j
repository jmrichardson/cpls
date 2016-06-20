# cPLS Start Times (PST)
# Lending Club lists new loans at 6AM, 10AM, 2PM and 6PM PST
# cPLS by default starts 1 minute prior for loading purposes
startTimes = c("5:59", "9:59", "13:59", "17:59")

# Maximum number of times to check for new notes per list (roughly once a second)
# If new notes have not been detected by this number of try's, then fail
maxNoteCount = 300

# Number of new notes required for list detection
# When starting cPLS, how many new notes are required for list to be detected
numNotesThresh = 10

# Sender return address for outgoing email
mailFrom = 'noreply@peerlendingserver.com'

# SMTP MAil Host Server
host = "smtp.gmail.com"

# SMTP Port
port = 465

# SMTP User Name
userName = "username"

# SMTP Password
userPasswd = "userpass"

# Use SSL
ssl = TRUE

# Shutdown server after completion
shutdown = FALSE
shutdownCmd = "shutdown -P now"


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

# Curl options
options(RCurlOptions = list(verbose = FALSE, 
                            followlocation = TRUE, 
                            autoreferer = TRUE,
                            ssl.verifypeer = FALSE,
                            useragent = "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/30.0.1599.101 Safari/537.36")
)

