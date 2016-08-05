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
if (!exists('test')) { 
  # Load saved model with stats
  load('data/modelStats.rda')
  statsModel <- stats
  
  # Load model and stats data
  load('data/model.rda')
  load('data/stats.rda')
  
  # Model only complete notes
  stats = subset(stats,(loan_status=='Fully Paid' | loan_status=='Charged Off'))
  
  # Quick error checking
  if ( nrow(stats) != nrow(statsModel)) stop('Row count not the same')
  
  # Get model prediction
  stats$label <- statsModel$label
  
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
  stats$loan_status <- NULL
  stats$memberId <- NULL
  stats$fundedAmount <- NULL
  stats$last_fico_range_low <- NULL

  # Only use test set (data model has not seen)
  test <- stats[-inTrain,]
  

  
  
  
  # Obtain model predictions
  test$model <- predict(xgbModel, data.matrix(predict(dmy, newdata=test[,featureNames])), missing=NA)
  
  # Release memory
  rm(stats)
  rm(statsModel)
  
  # Available features of test data set
  features <- names(test)
  features <- features[!features %in% c("model","label","id","memberId")]
  
  # discretize features with respect to loan status
  df <- test
  for(col in features) {
    if ( ! is.factor(df[[col]]) ) {
      
      if ( length(unique(df[[col]])) < 10 ) {
        df[[col]]=discretize(df[[col]],method="cluster",categories=4)
        next
      }
      
      x=smbinning(df,y="label",x=col,p=0.05)
      if ( x[1]=="No Bins") {
        df[[col]]=discretize(df[[col]],method="cluster",categories=4)
        next
      }
      
      df[[col]]=cut(df[[col]],unique(c(min(df[[col]]),x$cuts,max(df[[col]]))),dig.lab=10,include.lowest=TRUE,right=TRUE)
    }
  }
  
  
}




server <- function(input, output, session) { 
  
  # Filter test notes using filter text given
  data <- reactive ({
    if (input$filter != '') {
      test %>%
        filter_(input$filter)
    } else {
      test
    }
  })

  # Get optimal threshold  
  opt <- reactive ({
    round(optimalCutoff(data()$label, data()$model, optimiseFor = "Both", returnDiagnostics = F),2)
  })
 
  # Obtain predicted class based on probability and optimal cutoff   
  predClass <- reactive ({
    ifelse(data()$model>opt,1,0)
  })
    
  # Predicted Classes
  class <- reactive ({
    ifelse(data()$model>as.numeric(input$thresh),1,0)
  })
  
  # Confusion matrix
  cm <- reactive({
    # Obtain predicted class based on probability and optimal cutoff
    caret::confusionMatrix(relevel(factor(class()),"1"),relevel(factor(data()$label),'1'))
  })
  
  
  output$test <- renderPrint({
    nrow(data())
  })
  output$plotROC <- renderPlot({
    plotROC(actuals=data()$label,predictedScores=data()$model)
  })
  output$cm <- renderPrint({
    cm()
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
            column(10,
              textareaInput("filter", "Filter", rows=4,'term==36')
            ),
            column(2,
              selectInput("thresh", label = 'Threshold', 
                choices = as.list((1:99)/100),
                selected = .5),
              submitButton("Submit")
            )
          )
        )
      ),
      
      mainPanel(width=12,
        tabsetPanel(type = "tabs", 
          tabPanel("Model",
            fluidRow(
              verbatimTextOutput ("cm")
            ),
            fluidRow(
              plotOutput("plotROC")
            ),
            fluidRow(
              verbatimTextOutput ("test")
            )
          ), 
          tabPanel("Filter Builder", verbatimTextOutput("tesasdft")),
          tabPanel("Loan Statistics", verbatimTextOutput("tesadfsdft"))
        )
      )
      
      
      
      

    )
  )
)


shinyApp(ui = ui, server = server)


