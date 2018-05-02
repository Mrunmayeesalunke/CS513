#################################################
#  Company     : Stevens 
#  Project     : CS 513 Final Project-NYC Buildings Data
#  Purpose     : C 5.0 Tree
#  Names of the group members:
#  Name 1      : Mrunmayee Salunke (10429075)
#  Name 2      : Heli Choksi       (10430926)
#  Name 3      : Weronika Zamlynny (10397182)
#  Date        : 05/02/2018
#  Comments    : 
#################################################
rm(list=ls())
setwd("C:/Users/wzaml/Documents/Stevens/11_Spring2018/CS513_DataMining/CS513")
filename<-"nyc-buildings/BK_processed.csv"
df<-read.csv(filename)

df$Residential<-as.factor(df$Residential)
index<-sort(sample(nrow(df),round(.25*nrow(df))))
training<-df[-index,]
test<-df[index,]

#install.packages("C50")
library('C50')

# C50  classification 
df$Residential<-as.factor(df$Residential)
C50_class1 <- C5.0( Residential~UnitsTotal+AssessTot+NumFloors+AssessLand+BldgArea+BuiltFAR+StrgeArea+GarageArea+BldgDepth+ExemptTot,data=training )
summary(C50_class1 )
C50_class2 <- C5.0( Residential~UnitsTotal+AssessTot+NumFloors+BuiltFAR+StrgeArea+GarageArea+BldgDepth+ExemptTot,data=training )
summary(C50_class2)

plot(C50_class1)
plot(C50_class2)

#Step 4: Finding the Error Rate

C50_predict<-predict( C50_class2 ,test , type="class" )
table(actual=test$Residential,C50=C50_predict)
wrong<- (test$Residential!=C50_predict)
c50_rate<-sum(wrong)/length(test$Residential)
c50_rate

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
Prediction <- predict(C50_class2, df_BX, type='class')
table(actual=df_BX$Residential,Prediction)

# Finding the Error Rate
wrong<- (df_BX$Residential!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate 

# MN Manhattan
# Prediction
Prediction <- predict(C50_class2, df_MN, type='class')
table(actual=df_MN$Residential,Prediction)

# Finding the Error Rate
wrong<- (df_MN$Residential!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate

# QN Queens
# Prediction
Prediction <- predict(C50_class2, df_QN, type='class')
table(actual=df_QN$Residential,Prediction)

# Finding the Error Rate
wrong<- (df_QN$Residential!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate

# SI Staten Island
# Prediction
Prediction <- predict(C50_class2, df_SI, type='class')
table(actual=df_SI$Residential,Prediction)

# Finding the Error Rate
wrong<- (df_SI$Residential!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate

