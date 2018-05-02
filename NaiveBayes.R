#################################################
#  Company     : Stevens 
#  Project     : CS 513 Final Project-NYC Buildings Data
#  Purpose     : Naive Bayes
#  Names of the group members:
#  Name 1      : Mrunmayee Salunke (10429075)
#  Name 2      : Heli Choksi       (10430926)
#  Name 3      : Weronika Zamlynny (10397182)
#  Date        : 05/02/2018
#  Comments    : 
#################################################
rm(list=ls())
#################################################
# Load Data
setwd("C:/Users/wzaml/Documents/Stevens/11_Spring2018/CS513_DataMining/CS513")
filename<-"nyc-buildings/BK_processed.csv"
df<-read.csv(filename, na.strings="")
#################################################
# Load Packages
library(class) 
library(e1071)
#################################################
# Naive Bayes 

features<-c('UnitsTotal','AssessTot','NumFloors','AssessLand','BldgArea','BuiltFAR','StrgeArea','GarageArea','BldgDepth','ExemptTot')

## Normalize the data
for (i in features) {
  df[i] <- (df[,i] - mean(df[,i])) / sd(df[,i])
}

## Split into training and test set
index<-sort(sample(nrow(df),round(.25*nrow(df))))
training<-df[-index,]
test<-df[index,]

nBayes <- naiveBayes(Residential ~ UnitsTotal+AssessTot+NumFloors+AssessLand+BldgArea+BuiltFAR+StrgeArea+GarageArea+BldgDepth+ExemptTot, data=training)

## Naive Bayes classification using all variables 
category<-predict(nBayes, test[features])

table(NBayes=category,Residential=test$Residential)
NB_wrong<-sum(category!=test$Residential)
NB_error_rate<-NB_wrong/length(category)
NB_error_rate
