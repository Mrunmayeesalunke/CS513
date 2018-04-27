#########################################################
##  Purpose: Create C50 classification tree
##  Developer: KD         
##
#########################################################

#########################################################
##  Step 0: Clear the environment and load the data
##           
##
#########################################################



rm(list=ls())
df<- read.csv("C:/Users/mrunm/Desktop/CS513 Project/BK_processed.csv")


df$Residential<-as.factor(df$Residential)
index<-sort(sample(nrow(df),round(.25*nrow(df))))
training<-df[-index,]
test<-df[index,]


#install.packages("C50")
#library('C50')


# C50  classification 
df$Residential<-as.factor(df$Residential)
C50_class1 <- C5.0( Residential~UnitsTotal+AssessTot+NumFloors+AssessLand+BldgArea+BuiltFAR+StrgeArea+GarageArea+BldgDepth+ExemptTot,data=training )
summary(C50_class1 )
C50_class2 <- C5.0( Residential~UnitsTotal+AssessTot+NumFloors+BuiltFAR+StrgeArea+GarageArea+BldgDepth+ExemptTot,data=training )
summary(C50_class2)

dev.off()

plot(C50_class1)
plot(C50_class2)


#Step 4: Finding the Error Rate

C50_predict<-predict( C50_class2 ,test , type="class" )
table(actual=test$Residential,C50=C50_predict)
wrong<- (test$Residential!=C50_predict)
c50_rate<-sum(wrong)/length(test$Residential)
c50_rate


