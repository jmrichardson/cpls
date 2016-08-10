options(scipen=999)

# Load libraries
library('shiny')
library('InformationValue')
library('xgboost')
library('caret')
library('plotly')
library('xgboost')
library('dplyr')
library('arules')
library('smbinning')
library('lubridate')

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

  # Load model and stats data
  load('data/model.rda')
  load('data/stats.rda')
  
  # Model only complete notes
  stats = subset(stats,(loan_status=='Fully Paid' | loan_status=='Charged Off'))
  
  # Observation label based on loan_status
  stats$label <- ifelse(stats$loan_status=='Fully Paid',1,0)
  
  # Maturity
  stats$mature <- ifelse(stats$issue_d %m+% months(stats$term) <= as.Date(now()),TRUE,FALSE)
  
  stats$id <- NULL
  stats$member_id <- NULL
  stats$funded_amnt <- NULL
  stats$funded_amnt_inv <- NULL
  stats$empTitle <- NULL
  stats$issue_d <- NULL
  stats$url <- NULL
  stats$desc <- NULL
  stats$out_prncp <- NULL
  stats$out_prncp_inv <- NULL
  stats$total_pymnt <- NULL
  stats$total_pymnt_inv <- NULL
  stats$total_rec_prncp <- NULL
  stats$total_rec_int <- NULL
  stats$total_rec_late_fee <- NULL
  stats$recoveries <- NULL
  stats$collection_recovery_fee <- NULL
  stats$stats$last_pymnt_d <- NULL
  stats$last_pymnt_amnt <- NULL
  stats$next_pymnt_d <- NULL
  stats$next_pymnt_d <- NULL
  stats$last_credit_pull_d <- NULL
  stats$last_fico_range_high <- NULL
  stats$policy_code <- NULL
  stats$completeDate <- NULL
  stats$remPrncp <- NULL
  stats$total_int <- NULL
  stats$fees <- NULL
  stats$prnPaid <- NULL
  stats$loss <- NULL
  stats$title <- NULL
  stats$ficoRangeHigh <- NULL
  stats$pymnt_plan <- NULL
  stats$last_pymnt_d <- NULL
  # stats$loan_status <- NULL
  stats$memberId <- NULL
  stats$fundedAmount <- NULL


  # Get model prediction
  stats$model <- predict(xgbModel, data.matrix(predict(dmy, newdata=stats[,featureNames])), missing=NA)
  
  # Assume prediction class is always fully paid
  stats$class <- as.factor('Fully Paid')
  levels(stats$class) <- c('Fully Paid','Charged Off')
  stats$class <- factor(stats$class,c('Charged Off','Fully Paid'))
  stats$loan_status <- droplevels(stats$loan_status)

}

server <- function(input, output, session) { 
  
  # Filter test notes using filter text given
  data <- reactive ({
    filterStr <- input$filter
    df <- if (filterStr != '') {
      stats %>%
        filter_(filterStr)
    } else {
      stats
    }
    # if (input$test == TRUE) {
    #   df=df[-inTrain]
    # }
    df
  })



  output$test <- renderPrint({
    nrow(data())
  })
  # output$plotROC <- renderPlot({
  #   plotROC(actuals=data()$label,predictedScores=data()$model)
  # })
  output$cm <- renderPrint({
    caret::confusionMatrix(data()$class,data()$loan_status,'Fully Paid')
  })
}

textareaInput <- function(inputId, label, value="", placeholder="", rows=2){
  tagList(
    div(strong(label), style="margin-top: 5px;"),
    tags$style(type="text/css", "textarea {width:100%; margin-top: 5px;}"),
    tags$textarea(id = inputId, placeholder = placeholder, rows = rows, value))
}

ui <- (
  fluidPage(
    tags$head(
      tags$style(type="text/css","

        .row {
          margin-left: 10px;
          margin-right: 10px;
margin-top: 10px;
        }
        .inline {
          display: inline-block;
        }
        .alert {
          margin-left: 20px;
        }
        textarea {
          width: 100%; 
          margin-top: 5px;
          border-radius: 4px;
          border: 1px solid #cccccc; 
          outline: none; 
          resize: none; 
        }
      ")
    ),
    verticalLayout(
      fluidRow(
        wellPanel(
          fluidRow(
              textareaInput("filter", "Filter", rows=4,'term==36')
          ),
          fluidRow(
              submitButton("Submit")
          )
        )
      ),
      
      mainPanel(width=12,
        tabsetPanel(type = "tabs", 
          tabPanel("Model",
            fluidRow(
              verbatimTextOutput ("test")
            ),
            fluidRow(
              verbatimTextOutput ("cm")
            )
            # fluidRow(
            #   plotOutput("plotROC")
            # )
          ), 
          tabPanel("Filter Builder", verbatimTextOutput("tesasdft")),
          tabPanel("Loan Statistics", verbatimTextOutput("tesadfsdft"))
        )
      )
      
      
      
      

    )
  )
)


shinyApp(ui = ui, server = server)


