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
df<- read.csv("C:/Users/mrunm/Desktop/CS513 Project/BK_processed.csv")
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
