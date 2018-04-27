#################################################
#  Company     : Stevens 
#  Project     : CS 513 Final Project-NYC Buildings Data
#  Purpose     : Random Forest
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
setwd("C:/Users/wzaml/Documents/Stevens/11_Spring2018/CS513_DataMining/CS513")
filename<-"nyc-buildings/BK_processed.csv"
df<-read.csv(filename)

# Delete index, not for prediction
df<-subset(df,select=-X)
####################################
# Change y to as factor
df$Residential <- as.factor(df$Residential)

# Split to training and test dataset
index<-sort(sample(nrow(df),round(.25*nrow(df))))
training<-df[-index,]
test<-df[index,]

# Install packages
#install.packages('randomForest')

# Load the library
library(randomForest)

# Calculating randomforest
fit <- randomForest(Residential~.,data=training, importance=TRUE, ntree=5000)

fit_importance <- importance(fit)
fit_importance

# Get top 15 variables from importance - these will be used in future algoritms
top_features <- sort(fit_importance[,3], decreasing=TRUE)[1:10]
top_features

# Check Results
varImpPlot(fit)
Prediction <- predict(fit, test)
table(actual=test$Residential,Prediction)

wrong<- (test$Residential!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate 
