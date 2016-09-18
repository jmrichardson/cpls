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
library('data.table')

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

# Load helper functions
source('scripts/funcs.R')

server <- function(input, output, session) { 
  
  # Load init scripts
  output$loadInit = reactive({
     # Initialize
    source('scripts/initTool.R')
    return(1)
  })
  outputOptions(output, 'loadInit', suspendWhenHidden=FALSE)
  
  base <- reactive ({
    if (input$base != '') {
      stats[eval(parse(text=input$base))]
    } else {
      stats
    }
  })
  
  # Filter test notes using filter text given
  data <- reactive ({
    # User filter
    if (input$filter != '') {
      base()[eval(parse(text=input$filter))]
    } else {
      base()
    }
  })
  
  totalNotes <- reactive ({
    nrow(base())
  })
  
  cash <- reactive ({
     merge(x = ph, y = data()[,.(id)], by = "id")
  })
  
  xirr <- reactive ({
    cashOut <- cash()[, .(Month=min(Month),Payment=-max(Principal)), keyby = id]
    cashIn <- cash()[,.(Month,Payment=Payment*.99)]
    cashFlow <- rbind(cashIn,cashOut[,.(Month,Payment)])
    xIRR(cashFlow[, .(Payment=sum(Payment)), keyby = Month]) * 100
  })


  
  output$summary <-renderUI({
    filteredNotes <- nrow(data())
    pct <- round(filteredNotes/totalNotes()*100,2)
    co <- round(prop.table(table(data()$loan_status))[1],2)*100
    age <- round(mean(data()$aol),2)
    rate <- round(mean(data()$intRate),2)
    fluidRow(
      wellPanel(
        fluidRow(
          column(4,paste('Filtered Notes:',printNumber(filteredNotes))),
          column(4,paste('Total Notes:',printNumber(totalNotes()))),
          column(4,paste('Filtered Percent: ',pct,'%',sep=''))
        ),
        fluidRow(
          column(4,paste('XIRR: ',xirr(),'%',sep='')),
          column(4,paste('Average Rate: ',rate,'%',sep='')),
          column(4,paste('Age:',age,' Months'))
        ),
        fluidRow(
          column(4,paste('Charged Off: ',co,'%',sep='')),
          column(4,paste('Late 16: ','l16','%',sep='')),
          column(4,paste('Late 31: ','l31','%',sep=''))
        )
      )       
    )
    
  })

  # output$noteCount <- renderText({
  #   nrow(data())
  # })
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
        .col-sm-6 {
          padding-left: 0px;
          padding-right: 10px;
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
          conditionalPanel(
            condition = "output.loadInit != 1",
            h5('Loading data. Please be patient...')
          ),
          fluidRow(
            column(6,textareaInput("base", "Base", rows=4)),
            column(6,textareaInput("filter", "Filter", rows=4))
          ),
          fluidRow(
            submitButton("Submit")
          )
        )
      ),
      
      mainPanel(width=12,
        tabsetPanel(type = "tabs", 
          tabPanel("Performance",
            uiOutput('summary'),
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


