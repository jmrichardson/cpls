library('shiny')
library('readr')


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

ui <- fluidPage(
  titlePanel("System Log Viewer"),
  wellPanel(fluidRow(
    column(12,
      dataTableOutput(outputId="log"),
      br()
    )
  ))
)
server <- function(input, output, session) {
  
  data <- reactiveFileReader(1000, 
    session, 
    'store/system.log',
    read_fwf,
    skip=0,
    fwf_positions( c(1,25,30),c(23,29,NA),c('Date','Priority','Message')))
  
  output$log <- renderDataTable({
    log <- data()
    log$Date <- gsub('(\\[|\\])','',log$Date)
    log
  },options = list(pageLength = 100,
    lengthMenu = c(100, 250, 1000),
    order = c(0,'desc')
  ))
}
shinyApp(ui = ui, server = server)






