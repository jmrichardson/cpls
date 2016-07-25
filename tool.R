
# Load libraries
library('shiny')
library('InformationValue')
library('xgboost')
library('caret')
library('plotly')

# Seed for model comparisons
set.seed(1)

# Function to set home directory
defaultDir = '/home/user/cpls'
csf <- function() {
    cmdArgs = commandArgs(trailingOnly = FALSE)
    needle = "--file="
    match = grep(needle, cmdArgs)
    if (length(match) > 0) {
        # Rscript via command line
        return(normalizePath(sub(needle, "", cmdArgs[match])))
    } else {
        ls_vars = ls(sys.frames()[[1]])
        if ("fileName" %in% ls_vars) {
            # Source'd via RStudio
            return(normalizePath(sys.frames()[[1]]$fileName)) 
        } else {
            if (!is.null(sys.frames()[[1]]$ofile)) {
            # Source'd via R console
            return(normalizePath(sys.frames()[[1]]$ofile))
            } else {
                # RStudio Run Selection
                return(normalizePath(rstudioapi::getActiveDocumentContext()$path))
            }
        }
    }
}
dir <- tryCatch(dirname(csf()),
  error = function(e) {
    defaultDir
  }
)
if (is.null(dir) | length(dir) == 0) {
  dir <- defaultDir
}
if(!dir.exists(dir)) {
  err('Unable to determine home directory')
} else {
  setwd(dir)
}

# Load original stats and combine with model prediction
if (!exists('stats')) { 
  # Load saved model with stats
  load('data/statsModel.rda')
  statsModel <- stats
  
  # Load LC stats data
  load('data/stats.rda')
  
  # Model only complete notes
  stats = subset(stats,(loan_status=='Fully Paid' | loan_status=='Charged Off'))
  
  # Quick error checking
  if ( nrow(stats) != nrow(statsModel)) stop('Row count not the same')
  
  # Get model prediction
  stats$label <- statsModel$label
  rm(statsModel)
}




#

















server <- function(input, output, session) { }

ui <- (
  fluidPage(
    verticalLayout(
      titlePanel("Filter Analysis"),
      wellPanel(
        sliderInput("n", "Number of points", 10, 200,
          value = 50, step = 10)
      ),
      plotOutput("plot1")
    )
  )
)

shinyApp(ui = ui, server = server)