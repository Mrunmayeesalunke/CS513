#################################################
#  Company     : Stevens 
#  Project     : CS 513 Final Project-NYC Buildings Data
#  Purpose     : Preliminary Work/Preprocessing
#  Names of the group members:
#  Name 1      : Mrunmayee Salunke (10429075)
#  Name 2      : Heli Choksi       (10430926)
#  Name 3      : Weronika Zamlynny (10397182)
#  Date        : 05/02/2018
#  Comments    : 
#################################################
rm(list=ls())
#################################################
#Load Data
?read.csv
setwd("C:/Users/wzaml/Documents/Stevens/11_Spring2018/CS513_DataMining/CS513")
filename<-"nyc-buildings/BK_processed.csv"
df<-read.csv(filename, na.strings="")
df

#deleting variables that predicted too well and/or which had more variables for RF
df<-subset(df,select=-X)
df<-subset(df,select=-BldgClass) 
df<-subset(df,select=-Sanborn) 
df<-subset(df,select=-CT2010) 

df<-subset(df,select=-CB2010)
df<-subset(df,select=-ZipCode)
df<-subset(df,select=-FireComp)
df<-subset(df,select=-HealthArea)
df<-subset(df,select=-SanitSub)
df<-subset(df,select=-ZoneMap)
####################################
# Change to as factor
df$Residential <- as.factor(df$Residential)


index<-sort(sample(nrow(df),round(.05*nrow(df))))
training<-df[-index,]
test<-df[index,]

df2<-df[index,]

index2<-sort(sample(nrow(df2),round(.25*nrow(df2))))
training<-df2[-index2,]
test<-df2[index2,]

#install packages
#install.packages('randomForest')

#load the library
library(randomForest)

#calculating randomforest
fit <- randomForest(Residential~.,data=training, importance=TRUE, ntree=5000)

fit_importance <- importance(fit)
fit_importance

# get top 15 variables from importance
top_features <- sort(fit_importance[,3], decreasing=TRUE)[1:10]
top_features

varImpPlot(fit)
Prediction <- predict(fit, test)
?predict
table(actual=test$Residential,Prediction)

wrong<- (test$Residential!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate 
