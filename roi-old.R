# Download the payment history file to (/var/tmp or c:/temp)
# Update the name of the file below 

impFile <- 'PMTHIST_all_20160715.csv'
debug=TRUE
cores=2
tmpDir='C:/temp'
logFile=paste(tmpDir,"/roi.log",sep="")

# Load required packages
require(zoo)
library(lubridate)
library(MASS)
library(dplyr)
library(survival)
library(rms)
library(parallel)
library(tvm)
library(financial)
library(data.table)
library(doParallel)

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


##########
# Start import of payment history **************************************************
##########

# Import payment history
ph=read.csv(file=paste(tmpDir,"/",impFile,sep=''))
ph$RECEIVED_D <- dmy(paste("01", ph$RECEIVED_D , sep=""))
ph$MONTH <- dmy(paste("01", ph$MONTH , sep =""))
ph$RECEIVED_D <- ifelse(is.na(ph$RECEIVED_D),ph$MONTH,ph$RECEIVED_D)
ph$IssuedDate <- dmy(paste("01", ph$IssuedDate , sep=""))
ph$APPL_FICO_BAND <- as.numeric(substring(ph$APPL_FICO_BAND,1,3))
ph$PCO_RECOVERY=NULL
ph$PCO_COLLECTION_FEE=NULL
ph$MonthsSinceLastRec=NULL
ph$MonthsSinceDQ=NULL

# Collapse rows grouped by loanid and summarize
phCol <- group_by(ph, LOAN_ID) %>%
  summarise(
    periods=as.integer(ifelse(all(RECEIVED_AMT==0),1,max(which(RECEIVED_AMT>0))+1)),
    event=ifelse('Charged Off' %in% PERIOD_END_LSTAT,1,0),
#    nid=head(LOAN_ID,1),
    InterestRate=head(InterestRate,1),
#     mature=ifelse('Charged Off' %in% PERIOD_END_LSTAT |
#       'Fully Paid' %in% PERIOD_END_LSTAT |
#       min(PBAL_END_PERIOD) <= 0,1,0),
    MONTHLYCONTRACTAMT=head(MONTHLYCONTRACTAMT,1),
    PERIOD_END_LSTAT=tail(PERIOD_END_LSTAT,1),
    DUE_AMT=min(DUE_AMT),
    LoanAmount=max(PBAL_BEG_PERIOD),
    PBAL_END_PERIOD=min(PBAL_END_PERIOD),
    dti=head(dti,1),
    MOB=max(MOB),
    VINTAGE=head(VINTAGE,1),
    State=head(State,1),
    IssuedDate=min(IssuedDate),
    HomeOwnership=head(HomeOwnership,1),
    MonthlyIncome=head(MonthlyIncome,1),
    OpenCREDITLines=head(OpenCREDITLines,1),
    TotalCREDITLines=head(TotalCREDITLines,1),
    RevolvingCREDITBalance=head(RevolvingCREDITBalance,1),
    RevolvingLineUtilization=head(RevolvingLineUtilization,1),
    Inquiries6M=head(Inquiries6M,1),
    DQ2yrs=head(DQ2yrs,1),
    PublicRec=head(PublicRec,1),
    EmploymentLength=head(EmploymentLength,1),
    grade=head(grade,1),
    term=head(term,1),
    installmentIncomeRatio=ifelse(head(MonthlyIncome,1) == 0,head(MONTHLYCONTRACTAMT,1),head(MONTHLYCONTRACTAMT,1)/head(MonthlyIncome,1)),
    APPL_FICO_BAND=head(APPL_FICO_BAND,1)
  )

# Normalize MOB 0 to 1.  Events are now percentages instead of months
norm <- function(x,xMin,xMax) {
  round(((x-xMin)/(xMax-xMin)),2)
}
phCol$normPeriods <- 0
phCol$normPeriods[which(phCol$term==36)] <- norm(phCol$periods[which(phCol$term==36)],0,36)
phCol$normPeriods[which(phCol$term==60)] <- norm(phCol$periods[which(phCol$term==60)],0,60)

# Survival analysis
# summary(phCol)
cox <- coxph(Surv(normPeriods,event) ~ InterestRate + MONTHLYCONTRACTAMT + 
    State + HomeOwnership + MonthlyIncome + OpenCREDITLines + LoanAmount +
    TotalCREDITLines + RevolvingCREDITBalance + RevolvingLineUtilization +
    Inquiries6M + DQ2yrs + PublicRec +
    EmploymentLength + grade +
    installmentIncomeRatio + APPL_FICO_BAND,
    data=na.omit(phCol), x=TRUE,y=TRUE)
fit=survfit(cox,newdata=phCol,se.fit=FALSE)
save(ph,cox,fit,phCol,file=paste(tmpDir,"/roiPh.rda",sep=''))


# load(paste(tmpDir,"/roiPh.rda",sep=''))

curves=as.data.frame(fit$surv)
cTime=fit$time



# # Function to fit smoothed curve
# regFit <- function (curve,len=110) {
#   x <- seq(1:len)
#   fit <- lm(curve~poly(x,3,raw=TRUE))
#   data.frame(t(coef(fit)))
# }

# Get ph data for payment history shiny app
payments=subset(ph,select=c("LOAN_ID","PBAL_BEG_PERIOD","RECEIVED_AMT","RECEIVED_D"))
names(payments) <- c("NoteID","Principal","Payment","Month")
phSummary=subset(phCol,select=c("LOAN_ID","MONTHLYCONTRACTAMT","PERIOD_END_LSTAT","LoanAmount",
  "term","InterestRate","MOB","IssuedDate","PBAL_END_PERIOD","VINTAGE"))
names(phSummary) <- c("NoteID","Installment","Status","LoanAmount",
  "Term","IntRate","Age","IssuedDate","Balance","Vintage")
phSummary <- as.data.frame(phSummary)

curves=data.frame(t(curves))
curves$NoteID=phCol$LOAN_ID
curves=curves[,c(ncol(curves),1:(ncol(curves)-1))]

# Rather work with data tables than data frames from now on
# Need to update code above to start with data tables rather
# than converting now...
payments <- data.table(payments)
setkey(payments,NoteID,Payment)
probSum <- merge(phSummary,curves,by='NoteID')
setkey(probSum,NoteID)

# Save work up to now
save(payments,phSummary,curves,probSum,cTime,file=paste(tmpDir,"/roiFiles.rda",sep=''))
rm(ph,cox,fit,phCol)
gc()



##########
# Process each note and calculate ROIs *************************************************
##########

# load(paste(tmpDir,"/roiFiles.rda",sep=''))
# Load noteInfo from above.  Run this if starting from here
# load("data/noteInfo.rda")

# Function to calculate ROI for LC note
# Making vars global for debugging purposes
lcROI <- function(nid,wLog=FALSE) {

  library(data.table)
  library(lubridate)
  library(tvm)
  library(financial)  
  
  # Log note id before start. 
  if(wLog) write(paste("\n",noteId,"\n",sep=""),file=logFile,append=TRUE)
  
  pHis <<- as.data.frame(payments[.(nid)])
  p <<- as.data.frame(probSum[.(nid)])

  # Obtain summary data
  intRate <<- p$IntRate
  issueDate <<- as.POSIXlt(p$IssuedDate,tz="UTC")
  term <<- p$Term
  status <<- as.character(p$Status)
  balance <<- p$Balance
  installment <<- p$Installment
  age <<- p$Age
  loanAmount <<- p$LoanAmount
  vintage <<- p$Vintage
  
  curve <- as.numeric(p[,11:length(p)])
  nTime <- round(cTime*term)

  if(anyNA(curve)) {
    probCurve <- NA
  } else {
    timeCurve <- data.frame(cbind(nTime,curve))
    names(timeCurve) <- c('index','prob')
    saCurve <- aggregate(prob~index,timeCurve,mean)
    probCurve <- saCurve$prob
  }
  
  numPayments <<- nrow(pHis)
  if (numPayments > 0 ) {
    pHis$Month <- as.POSIXlt(pHis$Month,tz="UTC")
    pHis$Payment <- pHis$Payment*.99
    if (issueDate == min(pHis$Month)) {
      issueDate=issueDate - months(1)
    }    
  }

  if (status == 'Charged Off') {
    remPay = 0
  } else {
    remPay = balance
  }

  # Create complete cash flow including initial investment and remaining principle
  dfCash <<- subset(pHis,select=c("Month","Payment"))
  dfCash <<- rbind(data.frame(Month=issueDate,Payment=-loanAmount),dfCash)
  
  # Probabilities to discount cash flows
  if (status == "Fully Paid") {
    probPaid <<- 1
  } else if (status == "In Grace Period") {
    probPaid <<- .75
  } else if (status == "Late (16-30 days)") {
    probPaid <<- .43
  } else if (status == "Late (31-60 days)") {
    probPaid <<- .34
  } else if (status == "Late (61-90 days)") {
    probPaid <<- .18
  } else if (status == "Late (91-120 days)") {
    probPaid <<- .11
  } else if (status == "Default") {
    probPaid <<- .09
  } else if (status == 'Charged Off') {
    probPaid <<- 0
  } else {
    probPaid <<- 1
  }
  

  if (status == "Fully Paid" | status == 'Charged Off' | balance < .02) {
    # No more payments expected at this point
    if (numPayments==0) {
        ROI <<- -1
    } else {
      ROI <<- xIRR(dfCash)
    }
    roiCash <<- dfCash
    adjROI <<- ROI
    adjCash <<- dfCash
    projROI <<- ROI
    projCash <<- dfCash
    
  } else {
    # Future payments expected at this point
    
    # Build amortization table
    TVM <<- tvm(pv=-balance,i=intRate*100,n=NA,pmt=installment,pyr=12)
    remPayments <<- TVM[,2]
    
    # Create remaining cash flow from amort table
    Cash <<- round(append(rep(installment*.99,floor(remPayments)),
      installment*.99*(remPayments-floor(remPayments))),2)

    # Create payment dates for cash flow
    if(numPayments==0) {
      lastPaymentDate = issueDate + months(1)
    } else {
      lastPaymentDate <<- max(dfCash$Month) + months(1)        
    }
    Month <<- seq(lastPaymentDate, by = "month", length.out = ceiling(remPayments))
    
    # Instantaneous cash flow
    roiCash <<- rbind(dfCash,data.frame(Month=Month,Payment=Cash))
    roiCash$Payment <- round(roiCash$Payment,2)
    ROI <<- xIRR(roiCash)    
    
    # Adjusted cash flow
    adjDisCash <<- Cash*probPaid
    adjCash <<- rbind(dfCash,data.frame(Month=Month,Payment=adjDisCash))
    adjCash$Payment <- round(adjCash$Payment,2)
    adjROI <<- xIRR(adjCash)

    if(is.na(probCurve)){
      projCash <<- NA
      projROI <<- NA
    } else {
      newCurve <<- probCurve[-seq(1:(age-1))]
      delt <<- 1-newCurve[1]
      scaleCurve <<- head((newCurve[-1] + delt),ceiling(remPayments))
      disCash <<- Cash*scaleCurve*probPaid
        
      projCash <<- rbind(dfCash,data.frame(Month=Month,Payment=disCash))
      projCash$Payment <- round(projCash$Payment,2)
      projROI <<- xIRR(projCash)
    }
      
  }
  
  # Log noteid on finish.  If noteid has no pair in log, then error on this id
  if(wLog) write(paste("\n",noteId,"\n",sep=""),file=logFile,append=TRUE)
  
  # data.frame(ROI,adjROI,projROI)
  # c(list(roiCash),list(ROI),list(adjCash),list(adjROI),list(projCash),list(projROI))
  list(roiCash,ROI,adjCash,adjROI,projCash,projROI,age,vintage)
}

# Process all notes in parallel
nids <- head(probSum$NoteID)
unlink(logFile)
cl <- makeCluster(cores)
registerDoParallel(cl)
clusterExport(cl, c("payments","probSum","cTime","xIRR","npv","sppv","logFile"))
ptm <- proc.time()
noteROI <- parLapply(cl,nids,lcROI)
proc.time() - ptm
stopCluster(cl)

# Save data frame of ROI per note
ROI <- as.numeric(lapply(noteROI, `[[`, 2))
adjROI <- as.numeric(lapply(noteROI, `[[`, 4))
projROI <- as.numeric(lapply(noteROI, `[[`, 6))
age <- as.numeric(lapply(noteROI, `[[`, 7))
vintage <- as.character(unlist(lapply(noteROI, `[[`, 8)))
roi <- data.frame(nids,ROI,adjROI,projROI,age,vintage)
# roi <- subset(roi,ROI <= .35 & ROI>= -1 & adjROI <= .35 & adjROI >= -1 & projROI <= .35 & projROI >= -1)
roi$ROI=round(roi$ROI*100,2)
roi$adjROI=round(roi$adjROI*100,2)
roi$projROI=round(roi$projROI*100,2)

# Store in dev because it will eventually be added to filteredNotes in data dir
save(roi,file="../dev/roi.rda")

# Save lcROI function to be used by note browser app
save(lcROI,xIRR,sppv,npv,file='../lcROI.rda')


# Some sanity tests
summary(roi)
head(subset(roi,adjROI>ROI))





















a=lapply(head(noteInfo,200000), `[[`, 2)
b=do.call("rbind", a)
subset(b,Status=="Charged Off" & Age < 5)

