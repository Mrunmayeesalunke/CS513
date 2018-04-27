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

#########################################################
##  Step 0: Clear the environment
##           
##
#########################################################
rm(list=ls())

#########################################################
##  Step 1: Load the relavent packages
##           
##
#########################################################
install.packages("class")
library(class)

#########################################################
##  Step 2:  Loading the data

rm(list=ls())
df<- read.csv("C:/Users/mrunm/Desktop/CS513 Project/BK_processed.csv")
#View(df)

df$Residential<-as.factor(df$Residential)
index<-sort(sample(nrow(df),round(.25*nrow(df))))
training<-df[-index,]
test<-df[index,]

##Step 3: Run KNN
#Remove everything that is non-numerical
features<-c('UnitsTotal','AssessTot','NumFloors','AssessLand','BldgArea','BuiltFAR','StrgeArea','GarageArea','BldgDepth','ExemptTot')
predict<-knn(training[,features],test[,features],training$Residential,k=3)
predict
table(Prediction=predict,Actual=test$Residential)


#Step 4: Finding the Error Rate
wrong<- (test$Residential!=predict)
error_rate<-sum(wrong)/length(wrong)
error_rate 

#Step 5: Repeat
##Step 3: Run KNN
#Remove everything that is non-numerical
#features<-c('UnitsTotal','AssessTot','NumFloors','AssessLand','BldgArea','BuiltFAR','StrgeArea','GarageArea','BldgDepth','ExemptTot')
predict_k10<-knn(training[,features],test[,features],training$Residential,k=10)
table(Prediction=predict_k10,Actual=test$Residential)
#Step 4: Finding the Error Rate
wrong_k10<- (test$Residential!=predict_k10)
error_rate_k10<-sum(wrong_k10)/length(wrong_k10)
error_rate_k10 

#Step 6: Run again
##Step 3: Run KNN
#Remove everything that is non-numerical
#features<-c('UnitsTotal','AssessTot','NumFloors','AssessLand','BldgArea','BuiltFAR','StrgeArea','GarageArea','BldgDepth','ExemptTot')
predict_k25<-knn(training[,features],test[,features],training$Residential,k=25)
table(Prediction=predict_k25,Actual=test$Residential)
#Step 4: Finding the Error Rate
wrong_k25<- (test$Residential!=predict_k25)
error_rate_k25<-sum(wrong_k25)/length(wrong_k25)
error_rate_k25

#Step 7: Run again(4th time)
##Step 3: Run KNN
#Remove everything that is non-numerical
#features<-c('UnitsTotal','AssessTot','NumFloors','AssessLand','BldgArea','BuiltFAR','StrgeArea','GarageArea','BldgDepth','ExemptTot')
predict_k50<-knn(training[,features],test[,features],training$Residential,k=50)
table(Prediction=predict_k50,Actual=test$Residential)
#Step 4: Finding the Error Rate
wrong_k50<- (test$Residential!=predict_k50)
error_rate_k50<-sum(wrong_k50)/length(wrong_k50)
error_rate_k50
