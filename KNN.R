#########################################################
#  Company     : Stevens 
#  Project     : CS 513 Final Project-NYC Buildings Data
#  Purpose     : Preliminary Work/Preprocessing
#  Names of the group members:
#  Name 1      : Mrunmayee Salunke (10429075)
#  Name 2      : Heli Choksi       (10430926)
#  Name 3      : Weronika Zamlynny (10397182)
#  Date        : 05/02/2018
#  Comments    : 
##
#########################################################
##  Step 0: Clear the environment
rm(list=ls())

##  Step 1: Load the relavent packages
#install.packages("class")
library(class)

##  Step 2:  Loading the data
rm(list=ls())
setwd("C:/Users/wzaml/Documents/Stevens/11_Spring2018/CS513_DataMining/CS513")
filename<-"nyc-buildings/BK_processed.csv"
df<-read.csv(filename)
#View(df)


## List features
features<-c('UnitsTotal','AssessTot','NumFloors','AssessLand','BldgArea','BuiltFAR','StrgeArea','GarageArea','BldgDepth','ExemptTot')

## Normalize the data
for (i in features) {
  df[i] <- (df[,i] - mean(df[,i])) / sd(df[,i])
}

## Split into training and test set
df$Residential<-as.factor(df$Residential)
index<-sort(sample(nrow(df),round(.25*nrow(df))))
training<-df[-index,]
test<-df[index,]

##Step 3: Run KNN

errors<-c()
ks <- seq(from = 1, to = 50, by = 1)
for (i in ks) {
  # Predicting 
  predict<-knn(training[,features],test[,features],training$Residential,k=i)
  #print(i)
  #print(table(Prediction=predict,Actual=test$Residential))
  # Finding the Error Rate
  wrong<- (test$Residential!=predict)
  error_rate<-sum(wrong)/length(wrong)
  #print(error_rate)
  
  # append for graph
  errors <- append(errors, error_rate)
}
plot(ks, errors, type='l', main="Error rate vs K", xlab="k", ylab="error rate")

min(errors)
which.min(errors)
# K = 9 gives minimum
predict<-knn(training[,features],test[,features],training$Residential,k=9)
table(Prediction=predict,Actual=test$Residential)
# Finding the Error Rate
wrong<- (test$Residential!=predict)
error_rate<-sum(wrong)/length(wrong)
error_rate

#################################################
# Predict on different boroughs
filename_BX<-"nyc-buildings/BX_processed.csv"
df_BX<-read.csv(filename_BX)
filename_MN<-"nyc-buildings/MN_processed.csv"
df_MN<-read.csv(filename_MN)
filename_QN<-"nyc-buildings/QN_processed.csv"
df_QN<-read.csv(filename_QN)
filename_SI<-"nyc-buildings/SI_processed.csv"
df_SI<-read.csv(filename_SI)

# BX Bronx
# Prediction
Prediction <- knn(training[,features],df_BX[,features],training$Residential,k=9)
table(actual=df_BX$Residential,Prediction)

# Finding the Error Rate
wrong<- (df_BX$Residential!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate 

# MN Manhattan
# Prediction
Prediction <- knn(training[,features],df_MN[,features],training$Residential,k=9)
table(actual=df_MN$Residential,Prediction)

# Finding the Error Rate
wrong<- (df_MN$Residential!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate

# QN Queens
# Prediction
Prediction <- knn(training[,features],df_QN[,features],training$Residential,k=9)
table(actual=df_QN$Residential,Prediction)

# Finding the Error Rate
wrong<- (df_QN$Residential!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate

# SI Staten Island
# Prediction
Prediction <- knn(training[,features],df_SI[,features],training$Residential,k=9)
table(actual=df_SI$Residential,Prediction)

# Finding the Error Rate
wrong<- (df_SI$Residential!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate

