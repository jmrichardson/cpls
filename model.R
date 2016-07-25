# This model depends upon the following external stats (Be sure to get latest available):
# Lending club statistics:  lendingclub.com
# Zip code statistics: https://www.irs.gov/uac/soi-tax-stats-individual-income-tax-statistics-zip-code-stats-soi
# St Louis Fred:  https://fred.stlouisfed.org/
# * Change in labor market conditinos
# * Consumer price index for all urban consumers
# * Effective federal funds rate
# * Leading index for the united states
# * Smoothed US Recession probabilities
# * S&P 500
# * St Louis Fed Financial Stress Index

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

load('data/stats.rda')

# Model only complete notes
stats = subset(stats,(loan_status=='Fully Paid' | loan_status=='Charged Off'))

# Create regression target (percent of principal paid)
percPaid <- stats$total_rec_prncp/stats$loanAmount
# Shouldn't happen, but just in case
percPaid[percPaid < 0] <- 0
percPaid[percPaid > 1] <- 1

# Remove unnecessary fields
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
stats$earliestCrLine <- NULL
stats$addrZip <- NULL
stats$loan_status <- NULL
stats$memberId <- NULL
stats$fundedAmount <- NULL
stats$last_fico_range_low <- NULL


# Remove LC influenced fields - All of these values are LC dependent and changes over time
stats$intRate <- NULL    
stats$grade <- NULL
stats$subGrade <- NULL
stats$installment <- NULL

# Remove unimportant features based on model importance

# Save feature names
featureNames <- colnames(stats)
# featureNames <- featureNames[! featureNames %in% 'percPaid']

# Convert to numeric (xgboost requires numeric)
dmy <- dummyVars(" ~ .", data = stats)
stats <- data.frame(predict(dmy, newdata = stats))

# Add target percPaid after obtaining feature names and creating dmy model
stats$percPaid <- percPaid

#
# Stop here if prep'ing for model testing
#

# Create stratified train and test partition
inTrain <- createDataPartition(stats$percPaid,p=0.75, list=FALSE)
train <- stats[inTrain,]
test <- stats[-inTrain,]

#train = train[0:10000,]

# Create outcome vector for xgboost
trainLabel <- train$percPaid
train$percPaid <- NULL

# Maximum number of xgboost trees
nrounds <- 10000

# Table to track performance and create final model
res <- data.frame()

# Function to cross validate train set with params and early stopping
xgbMSE <- function (params) {
  fit <- xgb.cv(
    data = data.matrix(train), 
    label = trainLabel, 
    param =params, 
    missing = NA, 
    nfold = 5, 
    prediction = FALSE,
    early.stop.round = 3,
    maximize = FALSE,
    nrounds = nrounds
  )
  rounds <- nrow(fit)
  minIdx <- which.min(fit[,test.rmse.mean]) 
  minRMSE <- fit[minIdx,]$test.rmse.mean
  res <<- rbind(res,c(minIdx,minRMSE,rounds))
  colnames(res) <<- c('minIdx','minRMSE','rounds')
  return(minRMSE)
}

# Parameter grid to search
params <- list(
  objective = 'reg:linear',
  #eta = .3,
  eta = c(0.3,0.1,0.01),
  max_depth = c(2,4,6,8,10),
  gamma = 0,
  #gamma = c(0,1),
  colsample_bytree = 0.8,
  min_child_weight = 0
)

# Find best model parameters in parallel
# cl <- makeCluster(detectCores()-1) 
cl <- makeCluster(4) 
clusterExport(cl, c("xgb.cv",'train','trainLabel','nrounds','res'))
sol <- gridSearch(
  fun = xgbMSE,
  levels = params,
  method = 'snow',
  cl = cl,
  keepNames = TRUE,
  asList = TRUE
)
# Combine all model results
comb=clusterEvalQ(cl,res)
results <- ldply(comb,data.frame)
stopCluster(cl)

# xgbMSE(sol$minlevels)

xgbModel <- xgboost(
  data = xgb.DMatrix(data.matrix(train),missing=NaN, label = trainLabel),
  param = sol$minlevels,
  nrounds = results[which.min(results[,2]),1]
)

save(xgbModel,sol,inTrain,featureNames,dmy, file='data/xgbModel.rda')


stop()


#----------------------------------------- model performance testing

# Load model
load('data/xgbModel.rda')

# Use same data sets
train <- stats[inTrain,]
test <- stats[-inTrain,]

# get the feature real names
names = dimnames(data.matrix(train))[[2]]
# compute feature importance matrix
importance_matrix = xgb.importance(names, model=xgbModel)

# plot
gp = xgb.plot.importance(importance_matrix)
print(gp) 


actual <- test$percPaid
pred <- predict(xgbModel, data.matrix(test), missing=NA) 
head(pred, 10) 
head(actual,10)


postResample(pred,actual)

library(Metrics)
rmse(actual,pred)
mae(actual,pred)


plotROC(actuals=test$percPaid,predictedScores=pred)



#### Classification
library('SDMTools')
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
