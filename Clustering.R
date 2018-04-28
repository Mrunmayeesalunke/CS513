#################################################
#  Company     : Stevens 
#  Project     : CS 513 Final Project-NYC Buildings Data
#  Purpose     : Clustering: hclust + kmeans
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
df<- read.csv("C:/Users/mrunm/Desktop/CS513 Project/BK_processed.csv")
df
install.packages('clue')
library('clue')

#df$Residential<-as.factor(df$Residential)
index<-sort(sample(nrow(df),round(.25*nrow(df))))
training<-df[-index,]
test<-df[index,]

features<-c('UnitsTotal','AssessTot','NumFloors','AssessLand','BldgArea','BuiltFAR','StrgeArea','GarageArea','BldgDepth','ExemptTot')
bc2_dist<-dist( df[,features])
hclust_resutls<-hclust(bc2_dist)
hclust_2<-cutree(hclust_resutls,2)
table(hclust_2,df[,'Residential'])


kmeans_2<- kmeans(training[,features],2,nstart = 10)
kmeans_2$cluster
table(kmeans_2$cluster,training[,'Residential'])

kmeans_predict<-cl_predict( kmeans_2, test[,features], type='class' )
table(actual=test$Residential,kmeans=kmeans_predict)
