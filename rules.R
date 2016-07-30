options(scipen=999)

# Load required libraries
library(shiny)
library(arules)
library(smbinning) 

# Load historical statistics
load('data/stats.rda')

# Only model complete, fully paid or charged off data
data = subset(notes,(complete==TRUE & (loan_status=='Fully_Paid' | loan_status=='Charged_Off')))
data$loan_status <- factor(droplevels(data$loan_status))


data=subset(data, intRate > 15)


# Select on features id data needed to create rules
features=c("loanAmount", "term", "intRate", "installment", "empLength", "homeOwnership", "annualInc", "purpose", "addrState", 
           "dti", "delinq2Yrs", "ficoRangeLow",
           "inqLast6Mths", "openAcc", "pubRec", "revolBal", "revolUtil", "totalAcc", 
           "earliestCrLineMonths", "installmentIncomeRatio", "revolBalAnnualIncRatio")
data=data[,c(features,"loan_status")]
data=na.omit(data)

# Create fmla
fmla <- as.formula(paste("loan_status ~ ",paste(features,collapse=" + ")))


# Loan Status: 0 for bad notes, 1 for fully paid
data$lsFlag=ifelse(data$loan_status == 'Charged_Off' | 
                          data$loan_status == 'Default', 0, 1)
data$lsFlag=as.integer(data$lsFlag)




# discretize features with respect to loan status
for(col in features) {
  if ( ! is.factor(data[[col]]) ) {
    
    if ( length(unique(data[[col]])) < 10 ) {
      data[[col]]=discretize(data[[col]],method="cluster",categories=4)
      next
    }

    x=smbinning(data,y="lsFlag",x=col,p=0.05)
    if ( x[1]=="No Bins") {
      data[[col]]=discretize(data[[col]],method="cluster",categories=4)
      next
    }
    
    data[[col]]=cut(data[[col]],unique(c(min(data[[col]]),x$cuts,max(data[[col]]))),dig.lab=10,include.lowest=TRUE,right=TRUE)
  }
}

data$lsFlag<-NULL


# rules <- apriori(data)
# inspect(rules)
rules <- apriori(data,
                 parameter = list(supp=0.05, conf=0.85),
                 appearance = list(rhs=c("loan_status=Charged_Off", "loan_status=Fully_Paid"),
                                     default="lhs"),
                 control = list(verbose=F))


subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned

rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)




