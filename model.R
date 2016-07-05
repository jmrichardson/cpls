
# This model depends upon the following external data (Be sure to get latest available):
# Lending club statistics:  lendingclub.com
# Zip code statistics: https://www.irs.gov/uac/soi-tax-stats-individual-income-tax-statistics-zip-code-data-soi
# St Louis Fred:  https://fred.stlouisfed.org/
# * Change in labor market conditinos
# * Consumer price index for all urban consumers
# * Effective federal funds rate
# * Leading index for the united states
# * Smoothed US Recession probabilities
# * S&P 500
# * St Louis Fed Financial Stress Index


library(parallel)
# library(MASS)
library(plyr)
library(dplyr)
library(lubridate)
library('stringr')
# library('Matrix')
# library('MatrixModels')
library('caret')
library('xgboost')
# require(compiler)
# library(zoo)
# library(ggplot2)
# library(rms)
# library(e1071)
# library(pROC)
# # library(caret)
# library(party)
# library(rpart)
# library(gbm)
# # library(InformationValue)


# Enable parallel processing for caret to train GBM
# library(doMC)
# registerDoMC(cores = 6)


# Function to set home directory
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
setwd(dirname(csf()))

# LC date conversion function
dateConv <- function(x, year=1925){
  m <- year(x) %% 100
  year(x) <- ifelse(m > year %% 100, 1900+m, 2000+m)
  x
}


# Load helper functions
# source('funcs.R')


# Set seed for model comparison if needed
set.seed(1)


dataDir <- 'C:/temp'

# Read in Lending Club statistics
data1=read.csv(paste(dataDir,'LoanStats3a_securev1.csv',sep='/'),header=TRUE,skip=1)
data2=read.csv(paste(dataDir,'LoanStats3b_securev1.csv',sep='/'),header=TRUE,skip=1)
data3=read.csv(paste(dataDir,'LoanStats3c_securev1.csv',sep='/'),header=TRUE,skip=1)
data4=read.csv(paste(dataDir,'LoanStats3d_securev1.csv',sep='/'),header=TRUE,skip=1)
data5=read.csv(paste(dataDir,'LoanStats_securev1_2016Q1.csv',sep='/'),header=TRUE,skip=1)

# Update field name
names(data1)[names(data1)=="is_inc_v"] <- "verification_status"
names(data2)[names(data2)=="is_inc_v"] <- "verification_status"

# Remove fields that aren't present in historical data
data3[,57:200] <- list(NULL)
data4[,57:200] <- list(NULL)
data5[,57:200] <- list(NULL)

# Combine historical notes
data=rbind(data1,data2,data3,data4,data5)

# Remove notes withouth loan amount
data <- data[!is.na(data$loan_amnt),]

# Model only complete loans (notes that have had time to mature)
data$issue_d<-dateConv(dmy(paste("01", data$issue_d, sep = "-")))
data$term <-as.integer(as.character(gsub(" months", "", data$term)))
data$completeDate <- data$issue_d %m+% months(data$term)
data$complete <- ifelse(data$completeDate <= date(now()-months(1)),TRUE,FALSE)
data <- data[data$complete==TRUE,]

# Rename columns to match API
names(data)[names(data)=="loan_amnt"] <- "loanAmount"
names(data)[names(data)=="int_rate"] <- "intRate"
names(data)[names(data)=="sub_grade"] <- "subGrade"
names(data)[names(data)=="emp_title"] <- "empTitle"
names(data)[names(data)=="emp_length"] <- "empLength"
names(data)[names(data)=="home_ownership"] <- "homeOwnership"
names(data)[names(data)=="annual_inc"] <- "annualInc"
names(data)[names(data)=="zip_code"] <- "addrZip"
names(data)[names(data)=="addr_state"] <- "addrState"
names(data)[names(data)=="delinq_2yrs"] <- "delinq2Yrs"
names(data)[names(data)=="earliest_cr_line"] <- "earliestCrLine"
names(data)[names(data)=="fico_range_low"] <- "ficoRangeLow"
names(data)[names(data)=="fico_range_high"] <- "ficoRangeHigh"
names(data)[names(data)=="inq_last_6mths"] <- "inqLast6Mths"
names(data)[names(data)=="mths_since_last_delinq"] <- "mthsSinceLastDelinq"
names(data)[names(data)=="mths_since_last_record"] <- "mthsSinceLastRecord"
names(data)[names(data)=="open_acc"] <- "openAcc"
names(data)[names(data)=="pub_rec"] <- "pubRec"
names(data)[names(data)=="revol_bal"] <- "revolBal"
names(data)[names(data)=="revol_util"] <- "revolUtil"
names(data)[names(data)=="total_acc"] <- "totalAcc"
names(data)[names(data)=="mths_since_last_major_derog"] <- "mthsSinceLastMajorDerog"
names(data)[names(data)=="verification_status"] <- "isIncV"
names(data)[names(data)=="initial_list_status"] <- "initialListStatus"
names(data)[names(data)=="collections_12_mths_ex_med"] <- "collections12MthsExMed"

# Data cleansing  and feature engineering
data$empLength=as.integer(as.character(revalue(data$empLength,c("< 1 year"="0", "1 year"="12", "10+ years"="120", 
  "2 years"="24", "3 years"="36", "4 years"="48", "5 years"="60", "6 years"="72", 
  "7 years"="84", "8 years"="96", "9 years"="108"))))

data$intRate <-as.numeric(as.character(gsub("%", "", data$intRate)))
data$revolUtil <-as.numeric(as.character(gsub("%", "", data$revolUtil)))
data$loan_status=as.factor(gsub(" ", '_', data$loan_status))
data$isIncV=as.factor(toupper(gsub(" ", '_', data$isIncV)))
data$annualInc=round(data$annualInc)
data$initialListStatus=as.factor(toupper(data$initialListStatus))


data$earliestCrLine<-dateConv(dmy(paste("01", data$earliestCrLine, sep = "-")))


data$last_pymnt_d<-dateConv(dmy(paste("01", data$last_pymnt_d, sep = "-")))
data$revolBal <- as.numeric(data$revolBal)

data$n=ymd(Sys.Date())
data$earliestCrLineMonths=as.integer(round((data$n - data$earliestCrLine)/30.4375)-1)
data$n=NULL
data$installmentIncomeRatio=round(data$installment/(data$annualInc/12)*100)
data$revolBalAnnualIncRatio=round(data$revolBal/data$annualInc*100)

data$dtiInstallmentRatio=round(data$dti/data$installment,2)

data$amountTerm <- data$loanAmount/data$term
data$term <- as.factor(data$term)



### Add in zip code data
zip <- read.csv(paste(dataDir,'zip_codes.csv',sep='/'))
zip <- zip[,c('Zipcode','EstimatedPopulation','TotalWages','TaxReturnsFiled')]
zip <- na.omit(zip)
zip$Zipcode <- str_pad(zip$Zipcode, 5, pad = "0", side="left")
zip$addrZip <- substr(zip$Zipcode,1,3)
zip$addrZip <- as.factor(str_pad(zip$addrZip, 5, pad = "x", side="right"))
zip <- zip %>% 
  group_by(addrZip) %>%
  summarise(
    population=sum(as.numeric(EstimatedPopulation)),
    Wages=sum(as.numeric(TotalWages)),
    Returns=sum(as.numeric(TaxReturnsFiled)),
    avgWage=round(Wages/Returns)
  ) %>% 
  select (addrZip,population,avgWage)
data <- merge(x=data,y=zip,by="addrZip",all.x=TRUE)


### Add in FRED data
df <- read.csv(paste(dataDir,'PLS_Daily.txt',sep='/'),sep='\t')
df$SP500 <- as.numeric(as.character(df$SP500))
issue_d = as.Date(strftime(df$DATE, "%Y-%m-01"))
SP500 = aggregate(SP500 ~ issue_d, FUN = mean, data=df)
data <- merge(x=data,y=SP500,by="issue_d",all.x=TRUE)

df <- read.csv(paste(dataDir,'PLS_Weekly_Ending_Friday.txt',sep='/'),sep='\t')
df$STLFSI <- as.numeric(as.character(df$STLFSI))
issue_d = as.Date(strftime(df$DATE, "%Y-%m-01"))
STLFSI = aggregate(STLFSI ~ issue_d, FUN = mean, data=df)
data <- merge(x=data,y=STLFSI,by="issue_d",all.x=TRUE)

df <- read.csv(paste(dataDir,'PLS_Weekly_Ending_Wednesday.txt',sep='/'),sep='\t')
df$FF <- as.numeric(as.character(df$FF))
issue_d = as.Date(strftime(df$DATE, "%Y-%m-01"))
FF = aggregate(FF ~ issue_d, FUN = mean, data=df)
data <- merge(x=data,y=FF,by="issue_d",all.x=TRUE)

df <- read.csv(paste(dataDir,'PLS_Monthly.txt',sep='/'),sep='\t')
df$issue_d = as.Date(strftime(df$DATE, "%Y-%m-01"))
df$DATE <- NULL
data <- merge(x=data,y=df,by="issue_d",all.x=TRUE)

# Add regression target
data$percPaid <- data$total_rec_prncp/data$loanAmount
# Shouldn't happen, but just in case
data$percPaid[data$percPaid < 0] <- 0
data$percPaid[data$percPaid > 1] <- 1


# Remove unnecessary fields
data$id <- NULL
data$member_id <- NULL
data$funded_amnt <- NULL
data$funded_amnt_inv <- NULL
data$empTitle <- NULL
data$issue_d <- NULL
data$url <- NULL
data$desc <- NULL
data$out_prncp <- NULL
data$out_prncp_inv <- NULL
data$total_pymnt <- NULL
data$total_pymnt_inv <- NULL
data$total_rec_prncp <- NULL
data$total_rec_int <- NULL
data$total_rec_late_fee <- NULL
data$recoveries <- NULL
data$collection_recovery_fee <- NULL
data$data$last_pymnt_d <- NULL
data$last_pymnt_amnt <- NULL
data$next_pymnt_d <- NULL
data$next_pymnt_d <- NULL
data$last_credit_pull_d <- NULL
data$last_fico_range_high <- NULL
data$last_fico_range_low <- NULL
data$policy_code <- NULL
data$completeDate <- NULL
data$remPrncp <- NULL
data$total_int <- NULL
data$fees <- NULL
data$prnPaid <- NULL
data$loss <- NULL
data$title <- NULL
data$ficoRangeHigh <- NULL
data$pymnt_plan <- NULL
data$last_pymnt_d <- NULL
data$earliestCrLine <- NULL
data$addrZip <- NULL  # Too many factors/codes

# Remove LC influenced fields 
data$intRate <- NULL    
data$grade <- NULL
data$subGrade <- NULL
data$installment <- NULL
data$complete <- NULL
data$loan_status <- NULL

save(data,file='C:/temp/data.rda')


############################## Modeling phase ###############################


# Convert to numeric (xgboost requires numeric)
dmy <- dummyVars(" ~ .", data = data)
data <- data.frame(predict(dmy, newdata = data))

# Create data train and data partition
inTrain <- createDataPartition(data$percPaid,p=0.75, list=FALSE)
train <- data[inTrain,]
test <- data[-inTrain,]

# Create outcome vector
trainLabel <- train$percPaid
train$percPaid <- NULL


bst <- xgboost(data = data.matrix(train), label = trainLabel, max_depth = 4, missing = NaN,
               eta = 1, nthread = 4, nrounds = 100,objective = "reg:linear")






# notes=na.omit(notes)

# Save notes 
save(notes,file='dev/notes.rda')

#########
# MODEL #
#########

# load notes data if not already loaded
# load('dev/notes.rda')


# Only model complete, fully paid or charged off data
# Complete means that the loan has had enough time to mature
data = subset(notes,(complete==TRUE & (loan_status=='Fully_Paid' | loan_status=='Charged_Off')))
data$loan_status=droplevels(data$loan_status)

# Loan Status: 0 for bad notes, 1 for fully paid
data$loan_status=ifelse(data$loan_status == 'Charged_Off' | 
    data$loan_status == 'Default' |
    data$loan_status == 'Late_(31-120_days)', 0, 1)

features=c("loanAmount", "installment", "empLength", "homeOwnership", "annualInc", "purpose", 
  "intRate", "addrStateDiv", "dti", "delinq2Yrs", "ficoRangeLow", "inqLast6Mths", 
  "openAcc", "pubRec", "revolBal", "revolUtil", "totalAcc", 
  "avgWage", "population", "avgWageIncRatio", "dtiInstallmentRatio",
  "earliestCrLineMonths", "installmentIncomeRatio", "revolBalAnnualIncRatio")

# Addfeatures is just adding in data but not used in model.  Used for testing purposes
addFeatures=c("term","grade","subGrade")

data=data[,c(features,addFeatures,"loan_status")]
data=na.omit(data)

# Stratified split data into train and test
inTrain <- createDataPartition(data$loan_status,p=0.75, list=FALSE)
train <- data[inTrain,]
test <- data[-inTrain,]
trainX <- train[,features]
trainy <- train[,c("loan_status")]
train <- cbind(trainX,trainy)
colnames(train)[length(train)] <- "loan_status"
fmla <- as.formula(paste("loan_status ~ ",paste(names(trainX),collapse=" + ")))


# # Fit gbm model
fitGbm <- gbm(fmla, 
  data=train,
  distribution="bernoulli", 
  n.trees=1800,
  shrinkage=0.005, 
  interaction.depth=6,
  cv.folds=10,
  n.minobsinnode = 50, 
  keep.data=FALSE,               
  verbose=TRUE,
  n.cores=6)

# summary.gbm(fitGbm)

# test$loan_status_bin=ifelse(test$loan_status == 'Charged_Off' | 
#     test$loan_status == 'Default' |
#     test$loan_status == 'Late_(31-120_days)', 0, 1)
best.iter <- gbm.perf(fitGbm,method="cv")

# test$model=-1

test$model=predict.gbm(fitGbm,newdata=test,n.trees=best.iter,type="response")
# test$model = 1/(1 + exp(-predict.gbm(fitGbm,newdata=test,n.trees=best.iter,type="link")))
plotROC(actuals=test$loan_status,predictedScores=test$model)

#
#best.iter <- gbm.perf(fitGbm,method="OOB")
#fitGbm <- gbm.more(fitGbm,n.new.trees=1000,verbose=TRUE)

# 
# notes$loan_status_bin=ifelse(notes$loan_status == 'Charged_Off' | 
#     notes$loan_status == 'Default' |
#     notes$loan_status == 'Late_(31-120_days)', 0, 1)
# notes$model=predict(fitGbm,newdata=notes,n.trees=best.iter)
# plotROC(actuals=notes$loan_status_bin,predictedScores=notes$model)


# best.iter <- gbm.perf(fitGbm,method="cv")
# notes$model=predict(fitGbm,newdata=test,n.trees=300)



# Train neural network
# my.grid <- expand.grid(.decay = c(0.5, 0.1), .size = c(5, 6, 7))
# fit <- train(fmla, data = train, method = "nnet", maxit = 1000, tuneGrid = my.grid, trace = F, linout = 1)   


## show model performance
# best.iter <- gbm.perf(fitGbm,method="cv")
# print(best.iter)

#fitGbm2 <- gbm.more(fitGbm,200)
#fitGbm2 <- fitGbm

# load('dev/notes.rda')
# load('models/fitGbm.rda')

# Model prediction
# data$model=predict.train(fitGbm$finalModel,newdata=data,n.trees=fitGbm$finalModel$tuneValue$n.trees,type="prob")
notes$model=predict(fitGbm,newdata=notes,n.trees=best.iter,type="response")

save(notes,file='dev/notesFit.rda')
# save(train,file='dev/train.rda')
# save(test,file='dev/test.rda')

# Save model
save(fitGbm,file='data/fitGbm.rda')
