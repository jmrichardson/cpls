
# Load libraries
library('shiny')
library('InformationValue')
library('xgboost')
library('caret')
library('plotly')
library('xgboost')

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

# Load data
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
  
  # Use same data train/test sets
  train <- stats[inTrain,]
  test <- stats[-inTrain,]
  
  # Obtain actuals and predictions
  actuals <- test$label
  predictedScores <- predict(xgbModel, data.matrix(test), missing=NA)
  
  # Get optimal cutoff 
  opt <- optimalCutoff(actuals, predictedScores, optimiseFor = "Both", returnDiagnostics = F)
  
  # Obtain predicted class based on probability and optimal cutoff
  predictedClasses <- ifelse(predictedScores>opt,1,0)
  
  # Get importance matrix
  names = dimnames(data.matrix(train))[[2]]
  importance_matrix = xgb.importance(names, model=xgbModel)
  
  # Get concordance
  #con <- Concordance(actuals, predictedScores)
  #som <- somersD(actuals, predictedScores)
  
  cm <- caret::confusionMatrix(relevel(factor(predictedClasses),"1"),relevel(factor(actuals),'1'))
}




ui <- (
  fluidPage(
    verticalLayout(
      titlePanel("Filter Analysis"),
      wellPanel(
        sliderInput("n", "Number of points", 10, 200,
          value = 50, step = 10)
      ),
      renderPrint("cm")
    )
  )
)

server <- function(input, output, session) { 
  output$cm <- renderText({
    print(cm)
  })
}


shinyApp(ui = ui, server = server)


