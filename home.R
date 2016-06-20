# Set home directory

# Set correct working directory to use relative paths
cmdArgs <- commandArgs(trailingOnly = FALSE)
needle <- "--file="
match <- grep(needle, cmdArgs)
if (length(match) > 0) {
  # Run from script with --file option
  setwd(dirname(normalizePath(sub(needle, "", cmdArgs[match]))))
} else {
  if (! is.null(sys.frames())) {
    # Sourced via R console
    setwd(dirname(normalizePath(sys.frames()[[1]]$ofile,winslash='/')))
    print('john')
  } else {
    # Run from code snippets (RStudio development)
    if ( dir.exists('/home/john/Dropbox/cpls') ) {
      setwd("/home/john/Dropbox/cpls")
    } else if ( dir.exists('/home/user/cpls') ) {
      setwd("/home/user/cpls")
    } else {
      setwd("C:/Users/john/Dropbox/cpls")
    }
  }
}

