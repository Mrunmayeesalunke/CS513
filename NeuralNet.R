#################################################
#  Company     : Stevens 
#  Project     : CS 513 Final Project-NYC Buildings Data
#  Purpose     : Artificial Neural Net
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
####################################

# Split to training and test dataset
sub_index<-sort(sample(nrow(df),round(.25*nrow(df))))
df <- df[sub_index,]
index<-sort(sample(nrow(df),round(.25*nrow(df))))
training<-df[-index,]
test<-df[index,]

features<-c('UnitsTotal','AssessTot','NumFloors','AssessLand','BldgArea','BuiltFAR','StrgeArea','GarageArea','BldgDepth','ExemptTot')

# Load Libraries
library("neuralnet")

# Neural Net
net_bc2  <- neuralnet(Residential ~ UnitsTotal+AssessTot+NumFloors+AssessLand+BldgArea+BuiltFAR+StrgeArea+GarageArea+BldgDepth+ExemptTot, 
                      data=training, hidden=10, threshold=0.01)

# Plot Neural Net
plot(net_bc2)


# Look at results
net_bc2_results <-compute(net_bc2, test[,features]) 
class(net_bc2_results$net.result)

str(net_bc2_results)

ANN_Residential=round(net_bc2_results$net.result[,1])

table(Actual=test$Residential,Prediction=ANN_Residential)

wrong<- (test$Residential!=ANN_Residential)
error_rate<-sum(wrong)/length(wrong)
error_rate

