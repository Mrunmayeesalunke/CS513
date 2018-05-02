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
setwd("C:/Users/wzaml/Documents/Stevens/11_Spring2018/CS513_DataMining/CS513")
filename<-"nyc-buildings/BK_processed.csv"
df<-read.csv(filename)
#install.packages('clue')
library('clue')

## List features
features<-c('UnitsTotal','AssessTot','NumFloors','AssessLand','BldgArea','BuiltFAR','StrgeArea','GarageArea','BldgDepth','ExemptTot')

## Normalize the data
for (i in features) {
  df[i] <- (df[,i] - mean(df[,i])) / sd(df[,i])
}

# Split into training and test
index<-sort(sample(nrow(df),round(.25*nrow(df))))
training<-df[-index,]
test<-df[index,]

# H clust
bc2_dist<-dist( df[,features])
hclust_resutls<-hclust(bc2_dist)
hclust_2<-cutree(hclust_resutls,2)
table(hclust=hclust_2,actual=df[,'Residential'])

# Kmeans
kmeans_2<- kmeans(training[,features],2,nstart = 10)
#kmeans_2$cluster
table(kmeans_2$cluster,training[,'Residential'])

kmeans_predict<-cl_predict( kmeans_2, test[,features], type='class' )
table(kmeans=kmeans_predict,actual=test$Residential)
