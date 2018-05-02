#################################################
#  Company     : Stevens 
#  Project     : CS 513 Final Project-NYC Buildings Data
#  Purpose     : CART Tree
#  Names of the group members:
#  Name 1      : Mrunmayee Salunke (10429075)
#  Name 2      : Heli Choksi       (10430926)
#  Name 3      : Weronika Zamlynny (10397182)
#  Date        : 05/02/2018
#  Comments    : 
#################################################
rm(list=ls())
#################################################
##  Step 1: Load the relavent packages
#install.packages("rpart")
#install.packages("rpart.plot")     # Enhanced tree plots
#install.packages("rattle")         # Fancy tree plot
#install.packages("RColorBrewer")   # colors needed for rattle
library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle
#########################################################
##  Step 2:  Loading the data
setwd("C:/Users/wzaml/Documents/Stevens/11_Spring2018/CS513_DataMining/CS513")
filename<-"nyc-buildings/BK_processed.csv"
df<-read.csv(filename)
#View(df)

# Change y to as factor
df$Residential<-as.factor(df$Residential)

# Split to training and test dataset
index<-sort(sample(nrow(df),round(.25*nrow(df))))
training<-df[-index,]
test<-df[index,]

# Grow the tree 
CART_class<-rpart( Residential ~ UnitsTotal+AssessTot+NumFloors+AssessLand+BldgArea+BuiltFAR+StrgeArea+GarageArea+BldgDepth+ExemptTot, data=training)
CART_class

# Plot tree
prp(CART_class)
fancyRpartPlot(CART_class,cex=1)

# Prediction
Prediction <- predict(CART_class, test,type='class')
table(actual=test$Residential,Prediction)

# Finding the Error Rate
wrong<- (test$Residential!=Prediction )
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
Prediction <- predict(CART_class, df_BX, type='class')
table(actual=df_BX$Residential,Prediction)

# Finding the Error Rate
wrong<- (df_BX$Residential!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate 

# MN Manhattan
# Prediction
Prediction <- predict(CART_class, df_MN, type='class')
table(actual=df_MN$Residential,Prediction)

# Finding the Error Rate
wrong<- (df_MN$Residential!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate

# QN Queens
# Prediction
Prediction <- predict(CART_class, df_QN, type='class')
table(actual=df_QN$Residential,Prediction)

# Finding the Error Rate
wrong<- (df_QN$Residential!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate

# SI Staten Island
# Prediction
Prediction <- predict(CART_class, df_SI, type='class')
table(actual=df_SI$Residential,Prediction)

# Finding the Error Rate
wrong<- (df_SI$Residential!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate

