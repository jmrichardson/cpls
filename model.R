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

# Model only complete notes
stats = subset(stats,(loan_status=='Fully Paid' | loan_status=='Charged Off'))

# Get true label class
label <- ifelse(stats$loan_status=='Fully Paid',1,0)

# Remove unnecessary fields
stats$model <- NULL
stats$label <- NULL
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

# Remove term
stats$term <- NULL

# Remove unimportant features based on model importance

# Save feature names
featureNames <- colnames(stats)
# featureNames <- featureNames[! featureNames %in% 'percPaid']

# Convert to numeric (xgboost requires numeric)
dmy <- dummyVars(" ~ .", data = stats)
stats <- data.frame(predict(dmy, newdata = stats))

# Remove unimportant features based on trained model
# if(exists('features')) {
#   stats <- stats[,features]
# }

# Add label
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
pred <- predict(xgbModel, data.matrix(test), missing=NA) 
AUROC(actual, pred)

# Save model and train/test data
save(xgbModel,results,params,inTrain,featureNames,dmy, file='data/model.rda')

# Model stats data
load('data/stats.rda')
stats$label <- as.factor(ifelse(stats$loan_status=='Fully Paid',1,0))
stats$model <- predict(xgbModel, data.matrix(predict(dmy, newdata=stats[,featureNames])), missing=NA)

# Save stats
save(stats, file='data/stats.rda')

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
