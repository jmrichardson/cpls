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


# 
#   # Load saved model with stats
#   load('data/modelStats.rda')
#   statsModel <- stats

# Load model and stats data
load('data/model.rda')
load('data/stats.rda')

# Model only complete notes
stats = subset(stats,(loan_status=='Fully Paid' | loan_status=='Charged Off'))
stats$label <- ifelse(stats$loan_status=='Fully Paid',1,0)

# Only use test set (data model has not seen)
test <- stats[-inTrain,]

# Obtain model predictions
test$model <- predict(xgbModel, data.matrix(predict(dmy, newdata=test[,featureNames])), missing=NA)

test$id <- NULL
test$member_id <- NULL
test$issue_d <- NULL
test$url <- NULL
test$desc <- NULL
test$out_prncp <- NULL
test$out_prncp_inv <- NULL
test$total_pymnt <- NULL
test$total_pymnt_inv <- NULL
test$total_rec_prncp <- NULL
test$total_rec_int <- NULL
test$total_rec_late_fee <- NULL
test$recoveries <- NULL
test$collection_recovery_fee <- NULL
test$last_pymnt_d <- NULL
test$last_pymnt_amnt <- NULL
test$next_pymnt_d <- NULL
test$next_pymnt_d <- NULL
test$last_credit_pull_d <- NULL
test$last_fico_range_high <- NULL
test$policy_code <- NULL
test$completeDate <- NULL
test$remPrncp <- NULL
test$total_int <- NULL
test$fees <- NULL
test$prnPaid <- NULL
test$loss <- NULL
test$title <- NULL
test$pymnt_plan <- NULL
test$last_pymnt_d <- NULL
test$loan_status <- NULL
test$memberId <- NULL


# discretize features with respect to loan status
df <- test
df$earliestCrLine <- NULL
df$model <- NULL
df$label <- ifelse(df$label=1,"Fully_Paid","Charged_Off")

for(col in names(df)) {
  print(col)
  if ( ! is.factor(df[[col]]) ) {
    
    if ( length(unique(df[[col]])) <=5  ) {
      print("  less than 4")
      df[[col]]=as.factor(df[[col]])
      next
    }
    
    if ( length(unique(df[[col]])) < 10 ) {
      print("  less than 10")
      df[[col]]=discretize(df[[col]],method="cluster",categories=5)
      next
    }
    
    x=smbinning(df,y="label",x=col,p=0.3)
    if ( x[1]=="No significant splits") {
      print('  no significant')
      df[[col]]=discretize(df[[col]],method="frequency",categories=5)
      next
    }
    
    print('  binning')
    df[[col]]=cut(df[[col]],unique(x$bands),dig.lab=10,include.lowest=T,right=T)
  }
}

df=test
col='pubRec'
len=length(df[[col]][!is.na(df[[col]])])
pct=(len*.1)
df[[col]]=cut2(df[[col]],g=500)
summary(df[[col]])


x=smbinning(df,y="label",x=col,p=0.00001)
df[[col]]=discretize(df[[col]],method="cluster",categories=5)
summary(df$pubRec)

for(col in names(df)) {
  print(col)
  
  # col='dtiJoint'
  
  lvls <- levels(df[[col]])
  if (any(grepl('^\\[', lvls))) {
    cnt <- 1
    len <- length(levels(df[[col]]))
    for (l in levels(df[[col]])) {
      
      
      if(!grepl('(\\[|\\]|\\(|\\))',l)) {
        val = gsub('[^0-9.]','',l)
        op = '=='
        nl = paste(col,op,val)
      } else {
          if (cnt==1) {
          op <- ifelse(grepl('\\]',l),'<=','<')
          val <- gsub('[^0-9.]','',gsub("^.*, *","", l))
          nl <- paste(col,op,val)
        } else if (cnt==len) {
          op <- ifelse(grepl('\\[',l),'>=','>')
          val <- gsub('[^0-9.]','',gsub(",.*","", l))
          nl <- paste(col,op,val)
        } else {
          if(grepl('\\[',l)) {
            op1 = '>='
            op2 = '<'
          } else {
            op1 = '>'
            op2 = '<='
          }
          val1 <- gsub('[^0-9.]','',gsub(",.*","", l))
          val2 <- gsub('[^0-9.]','',gsub("^.*, *","", l))
          nl <- paste(col,op1,val1,'&',col,op2,val2)
        }
      }
      
      # print(l)
      # print(nl)
      levels(df[[col]])[[cnt]] <- nl
      
      cnt = cnt+1
    }
  }

}


df$label = as.factor(ifelse(df$label==1,"Fully_Paid","Charged_Off"))

save(df,test,file='df.rda')

label=df$label
df=df[,c("loanAmount","term","dti","population")]
df$label=label



rules <- apriori(df, parameter = list(supp=0.05, conf=0.85),
  appearance = list(rhs=c("label=Charged_Off", "label=Fully_Paid"), default="lhs"))


rules <- apriori(df,
                 parameter = list(supp=0.05, conf=0.85),
                 appearance = list(rhs=c("label=1", "label=0"),
                                     default="lhs"),
                 control = list(verbose=F))


subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned

rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)







