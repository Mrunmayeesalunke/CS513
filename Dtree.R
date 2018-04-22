#########################################################
##  Purpose  : Create pretty classification tree
##  Developer: Project Group       
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

install.packages("rpart")
install.packages("rpart.plot")     # Enhanced tree plots
install.packages("rattle")         # Fancy tree plot
install.packages("RColorBrewer")   # colors needed for rattle
library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle

#########################################################
##  Step 2:  Loading the data

rm(list=ls())
df<- read.csv("C:/Users/mrunm/Desktop/CS513 Project/BK_processed.csv")
View(df)

df<-subset(df,select=-X)
df<-subset(df,select=-BldgClass) # delete the building class
df<-subset(df,select=-Sanborn)
df<-subset(df,select=-CT2010)

index<-sort(sample(nrow(df),round(.25*nrow(df))))
training<-df[-index,]
test<-df[index,]

?rpart()


#Grow the tree 
CART_class<-rpart( Residential~.,data=training)
CART_class
prp(CART_class) #Successful tree

#?prp
#Delete successful parameters
df<-subset(df,select=-c(UnitsTotal,AssessLand,GarageArea,StrgeArea,NumFloors))
training<-subset(training,select=-c(UnitsTotal,AssessLand,GarageArea,StrgeArea,NumFloors))
test<-subset(test,select=-c(UnitsTotal,AssessLand,GarageArea,StrgeArea,NumFloors))

#Grow the tree 
CART_class<-rpart( Residential~.,data=training)
CART_class
prp(CART_class) #Successful tree



fancyRpartPlot(CART_class,cex=1)



