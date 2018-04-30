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

## Split into training and test set
df$Residential<-as.factor(df$Residential)
index<-sort(sample(nrow(df),round(.25*nrow(df))))
training<-df[-index,]
test<-df[index,]

##Step 3: Run KNN
features<-c('UnitsTotal','AssessTot','NumFloors','AssessLand','BldgArea','BuiltFAR','StrgeArea','GarageArea','BldgDepth','ExemptTot')

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
