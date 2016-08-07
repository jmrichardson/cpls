# cPLS Start Times (PST)
# Lending Club lists new loans at 6AM, 10AM, 2PM and 6PM PST
# cPLS by default starts 1 minute prior for loading purposes
startTimes = c("06:00", "10:00", "14:00", "18:00")

# Maximum number of times to check for new notes per list (roughly once a second)
# If new notes have not been detected by this number of try's, then fail
maxNoteCount = 60

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
userName = "user@domain.com"

# SMTP Password
userPasswd = "password"

# Use SSL
ssl = TRUE

