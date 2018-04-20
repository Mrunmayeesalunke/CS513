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


rm(list=ls())

#Load Data
df<-read.csv("C:/Users/mrunm/Desktop/CS513 Project/BK_sample.csv")
df

#Summarizing each column
summary(df)

#Modifying columns
#Replacing NA with 0 in all of the areas excluding LotArea and BldgArea
#column_na<-is.na(df[,"ResArea"])
#df[column_na,"ResArea"]<-0
#df

#Make new column for Majority_Res
df["Majority_Res"]<-df[,"ResArea"]>(df[,"ComArea"]+df[,"OfficeArea"]+df[,"RetailArea"]+df[,"GarageArea"]+df[,"StrgeArea"]+df[,"FactryArea"])
df

#Combine Year Alter 1 and 2
df["YearAlter"]<-ifelse(df[,"YearAlter1"]>df[,"YearAlter2"],df [,"YearAlter1"],df[,"YearAlter2"])
