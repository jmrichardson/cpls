# This model depends upon the following external stats (Be sure to get latest available):
# Lending club statistics:  lendingclub.com
# Zip code statistics: https://www.irs.gov/uac/soi-tax-stats-individual-income-tax-statistics-zip-code-stats-soi

# Load required libraries
library('parallel')
library('plyr')
library('dplyr')
library('lubridate')
library('stringr')
library('caret')
library('xgboost')
library('NMOF')
library('snow')
library('quantmod')
library('InformationValue')
library('data.table')

# No scientific
options("scipen"=10, "digits"=10)

# Seed for model comparisons
set.seed(1)

# Function to set relative home directory (requires latest Rstudio)
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

# Load LC stats data
load('data/stats.rda')

# Model only charged off or fully paid notes
stats=stats[loan_status=='Fully Paid' | loan_status=='Charged Off'][order(id)]

# Record label and id
id <- stats$id
label <- stats$label

# Select available model fields
featureNames <- c("loanAmount",
  "empLength",
  "homeOwnership",
  "annualInc",
  "isIncV",
  "purpose",
  "addrState",
  "dti",           
  "delinq2Yrs",
  "ficoRangeLow",
  "inqLast6Mths",
  "mthsSinceLastDelinq",       
  "mthsSinceLastRecord",
  "openAcc",
  "pubRec",
  "revolBal",                  
  "revolUtil",
  "totalAcc",
  "initialListStatus",
  "collections12MthsExMed",
  "mthsSinceLastMajorDerog",
  "applicationType",
  "annualIncJoint",
  "dtiJoint",
  "isIncVJoint",
  "accNowDelinq",
  "totCollAmt",
  "totCurBal",                 
  "openAcc6m",
  "openIl6m",
  "openIl12m",
  "openIl24m",
  "mthsSinceRcntIl",
  "totalBalIl",
  "iLUtil",
  "openRv12m",
  "openRv24m",
  "maxBalBc",
  "allUtil",
  "totalRevHiLim",
  "inqFi",
  "totalCuTl",
  "inqLast12m",
  "accOpenPast24Mths",
  "avgCurBal",
  "bcOpenToBuy",
  "bcUtil",
  "chargeoffWithin12Mths",
  "delinqAmnt",
  "moSinOldIlAcct",
  "moSinOldRevTlOp",
  "moSinRcntRevTlOp",
  "moSinRcntTl",
  "mortAcc",
  "mthsSinceRecentBc",
  "mthsSinceRecentBcDlq",
  "mthsSinceRecentInq",
  "mthsSinceRecentRevolDelinq",
  "numAcctsEver120Ppd",
  "numActvBcTl",
  "numActvRevTl",
  "numBcSats",
  "numBcTl",
  "numIlTl",
  "numOpRevTl",
  "numRevAccts",
  "numRevTlBalGt0",
  "numSats",
  "numTl120dpd2m",
  "numTl30dpd",
  "numTl90gDpd24m",
  "numTlOpPast12m",            
  "pctTlNvrDlq",
  "percentBcGt75",
  "pubRecBankruptcies",
  "taxLiens",
  "totHiCredLim",
  "totalBalExMort",
  "totalBcLimit",
  "totalIlHighCreditLimit",
  "earliestCrLineMonths",
  "amountTerm",
  "amountTermIncomeRatio",
  "revolBalAnnualIncRatio",
  "population",
  "avgWage")
stats <- stats[,featureNames,with=FALSE]  

# Create empty data frame to record structure of independent variables
featureDF=subset(as.data.frame(head(stats,1)),select=featureNames)

# Convert to numeric (xgboost requires numeric)
stats <- data.frame(stats)
dmy <- dummyVars(" ~ .", data = stats)
stats <- data.frame(predict(dmy, newdata = stats))

# Add back label
stats$label <- label

#
# Stop here if prep'ing for model testing
#

# Create stratified train and test partition
inTrain <- createDataPartition(stats$label,p=0.75, list=FALSE)
train <- stats[inTrain,]
test <- stats[-inTrain,]

# train = train[0:500,]

# Create outcome vector for xgboost and remove from training set
trainLabel <- train$label
train$label <- NULL

# xgboost task parameters
nrounds <- 3000
folds <- 5
obj <- 'binary:logistic'
eval <- 'logloss'

# Parameter grid to search
params <- list(
  eval_metric = eval,
  objective = obj,
  # eta = c(1, 0.01, 0.1, 0.01),
  eta = 0.1,
  # max_depth = c(4, 6, 8, 10, 12),
  max_depth = 4,
  # max_delta_step = c(0, 1),
  max_delta_step = 1,
  # colsample_bytree = c(0.5, 0.7),
  colsample_bytree = .7,
  # gamma = c(0, 0.5, 0.7, 1)
  gamma = .7
)

# Table to track performance from each worker node
res <- data.frame()

# Simple cross validated xgboost training function (returning minimum error for grid search)
xgbCV <- function (params) {
  fit <- xgb.cv(
    data = data.matrix(train), 
    label = trainLabel, 
    param =params, 
    missing = NA, 
    nfold = folds, 
    prediction = FALSE,
    early.stop.round = 5,
    maximize = FALSE,
    nrounds = nrounds
  )
  rounds <- nrow(fit)
  metric = paste('test.',eval,'.mean',sep='')
  idx <- which.min(fit[,fit[[metric]]]) 
  val <- fit[idx,][[metric]]
  res <<- rbind(res,c(idx,val,rounds))
  colnames(res) <<- c('idx','val','rounds')
  return(val)
}

# Find minimal testing error in parallel
cl <- makeCluster(round(detectCores()/2)) 
clusterExport(cl, c("xgb.cv",'train','trainLabel','nrounds','res','eval','folds'))
sol <- gridSearch(
  fun = xgbCV,
  levels = params,
  method = 'snow',
  cl = cl,
  keepNames = TRUE,
  asList = TRUE
)

# Combine all model results
comb=clusterEvalQ(cl,res)
stopCluster(cl)
results <- ldply(comb,data.frame)
df <- suppressWarnings(data.frame(Reduce(rbind, sol$levels)))
df <- cbind(val=sol$values,df)
results <- arrange(merge(results,df,by='val'),val)

# Train model with appropriate parameters
params <- c(sol$minlevels,objective = obj, eval_metric = eval)
xgbModel <- xgboost(
  data = xgb.DMatrix(data.matrix(train),missing=NaN, label = trainLabel),
  param = params,
  nrounds = head(results$idx,1)
)

# Show cv model results
print(results)

# Quick plot of AUC
actual <- test$label
test$label <- NULL
pred <- predict(xgbModel, data.matrix(test), missing=NA) 
AUROC(actual, pred)

actual <- trainLabel
pred <- predict(xgbModel, data.matrix(train), missing=NA)
AUROC(actual, pred)

# Save model and train/test data
save(xgbModel,results,params,inTrain,featureNames,featureDF,dmy, file='data/model.rda')

# Create sets data frame to record which loans were in train and test set
sets = as.data.frame(id)
trainIdx <- as.vector(inTrain)
sets$set <- 'test'
sets[trainIdx,]$set <- 'train'
sets$set <- as.factor(sets$set)
names(sets)<-c('id','set')

# trainIdx <- as.vector(inTrain)
# testIdx <- as.vector(-inTrain)
# stats$set <- 'test'
# stats[trainIdx,]$set <- 'train'
# stats$set <- as.factor(stats$set)
# sets <- stats[,c('id','set')]

# Add set label to stats data frame
load('data/stats.rda')
stats <- merge(x = stats, y = sets, by = "id", all.x=TRUE)

# Model stats data
stats$model <- predict(xgbModel, data.matrix(predict(dmy, newdata=stats[,featureNames, with=FALSE])), missing=NA)

# Save stats
save(stats, file='data/stats.rda')

stop()

















































### Add ROI data ###

# Get duration of each note
# End of Loan for fully paid is last_pymnt_d
# End of Loan for charged off is last_pymnt_d + 5 months
# End of Loan for active loans is current day (current age)
stats$eolDays <- ifelse(stats$loan_status == 'Fully Paid',
  as.numeric(difftime(stats$last_pymnt_d,stats$issue_d,units="days")),
  ifelse(stats$loan_status == 'Charged Off',
    ifelse(is.na(stats$last_pymnt_d),
      150,
      as.numeric(difftime(stats$last_pymnt_d + months(5),stats$issue_d,units="days"))),
    as.numeric(difftime(now(),stats$issue_d,units="days"))
  ))
stats$eolMths=round(stats$eolDays/30)
stats$eolMths <- ifelse(stats$eolMths==0,1,stats$eolMths)

# Age of loan based on current day for all loans
stats$ageDays=as.numeric(difftime(now(),stats$issue_d,units="days"))
stats$ageMths=round(stats$ageDays/30)

# Obtain remaining principal
stats$remPrncp=stats$fundedAmount-stats$total_rec_prncp

# Obtain total interest paid by borrower
stats$total_int=stats$remPrncp + stats$total_pymnt - stats$fundedAmount

# Obtain fees from LC
stats$fees= stats$total_pymnt * .01

# Remove loans that have payments but no interest earned
stats=stats[!stats$total_int==0 & stats$total_pymnt>0,]

# Effective Principal by investor (not borrower) to obtain the total received interest
# P=I/r for a simple interest loan
# Based on the interest received and rate of loan, you can obtain the amount of principal paid
# to obtain the interest received.  Cannot use all of the origination principal because all of the 
# interest may not have been paid
stats$prnPaid <- stats$total_int/(stats$intRate/100)

# other Income
stats$otherIncome = stats$recoveries + stats$collection_recovery_fee

# Set loss to remaining principal * loss factor
stats$loss=round(ifelse ( stats$loan_status == 'Charged Off', stats$remPrncp,
  ifelse ( stats$loan_status == 'Default', stats$remPrncp * .98,
    ifelse ( stats$loan_status == 'Late (31-120 days)', stats$remPrncp * .82,
      ifelse ( stats$loan_status == 'Late (16-30 days)', stats$remPrncp * .59,
        ifelse ( stats$loan_status == 'In Grace Period', stats$remPrncp  * .27, stats$remPrncp * 0))))),2)

# ROI
stats$ROI <- ( stats$total_int + stats$otherIncome -stats$fees - stats$loss ) / ( stats$prnPaid + stats$loss ) * 100
# Don't annualize loans less than year and complete
stats$ROI <- ifelse(stats$eolMths<12 & (stats$loan_status=='Fully Paid' | stats$loan_status=='Charged Off'),
  round(( stats$total_int + stats$otherIncome + stats$total_rec_prncp - stats$fundedAmount - stats$loss ) / ( stats$fundedAmount + stats$loss)*100,2),
  stats$ROI)
stats$ROI <- round(stats$ROI - 1,2)
stats$ROI <- ifelse(stats$ROI <= -100, -100,stats$ROI)
summary(stats$ROI)



# Projected ROI
stats$projROI = round(ifelse(stats$loan_status == 'Current' | stats$loan_status == 'Issued', 
  stats$model * (stats$intRate - 1),
  stats$ROI),2)




# End



























stop()

# Feature selection logic
# Used after model trained to determine non important fields

df <- as.data.frame(importance_matrix)
df <- df[df$Gain>=.001,]
features <- df$Feature




#----------------------------------------- model performance testing

# Load model
load('data/model.rda')
load('data/modelStats.rda')

# Use same data sets
train <- stats[inTrain,]
test <- stats[-inTrain,]

# Get important features and plot
names = dimnames(data.matrix(train))[[2]]
importance_matrix = xgb.importance(names, model=xgbModel)
gp = xgb.plot.importance(importance_matrix)
print(gp) 

# Get optimal threshold
opt <- optimalCutoff(actuals, predictedScores, optimiseFor = "Both", returnDiagnostics = F)

# Compute and plot AuC
actuals <- test$label
predictedScores <- predict(xgbModel, data.matrix(test), missing=NA) 
AUROC(actuals, predictedScores)

kappaCohen(actuals, predictedScores, threshold = 0.5)
ks_plot(actuals, predictedScores)
ks_stat(actuals, predictedScores, returnKSTable = F)

plotROC(actuals=test$label,predictedScores=pred)








postResample(pred,actual)

library(Metrics)
rmse(actual,pred)
mae(actual,pred)




plotROC(actuals=test$label,predictedScores=pred)






#### Classification

library('ROCR')




actual <- ifelse(test$percPaid<1,0,1)

opt <- optim.thresh(actual,pred)$'sensitivity=specificity'



predObj <- prediction(pred,actual)
perfObj = performance(predObj, measure = 'tpr', x.measure = 'fpr')
plot(perfObj)

opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(perfObj, predObj))

perfObj = performance(predObj, 'cost', cost.fp = 3, cost.fn = 1)
predObj@cutoffs[[1]][which.min(perfObj@y.values[[1]])]





pred <- ifelse(pred>opt,1,0)

confusionMatrix(pred,actual)



postResample(pred,actual)
 sensitivity(pred,actual)
