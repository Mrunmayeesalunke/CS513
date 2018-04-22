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
#################################################
rm(list=ls())
#################################################

#Load Data
?read.csv
filename<-"C:/Users/wzaml/Documents/Stevens/11_Spring2018/CS513_DataMining/CS513/nyc-buildings/BK_sample.csv"
df<-read.csv(filename, na.strings="")
df

#Summarizing each column
summary(df)

#################################################
#Modifying columns
#Replacing NA with 0 in all of the areas excluding LotArea and BldgArea
#column_na<-is.na(df[,"ResArea"])
#df[column_na,"ResArea"]<-0
#df

# TODO use LandUse insteado Majority Area
# Make new column Residential if Land Use <= 4
df["Residential"]<- ifelse(df[,"LandUse"]=='1'|df[,"LandUse"]=='2'|df[,"LandUse"]=='3'|df[,"LandUse"]=='4',TRUE,FALSE)
df <- subset(df, select=-LandUse)

#################################################
#Combine Year Alter 1 and 2
df["YearAlter"]<-ifelse(df[,"YearAlter1"]>df[,"YearAlter2"],df [,"YearAlter1"],df[,"YearAlter2"])
df <- subset(df, select=-c(YearAlter1,YearAlter2))

# Make HistDist into Boolean
df[,"HistDist"]<-!is.na(df[,"HistDist"])

# Make Landmark into Boolean
df[,"Landmark"]<-!is.na(df[,"Landmark"])

# Combine and Make Special District Boolean
df[,"SPDist"]<-(!is.na(df[,"SPDist1"]))|(!is.na(df[,"SPDist2"]))|(!is.na(df[,"SPDist3"]))
df <- subset(df, select=-c(SPDist1,SPDist2,SPDist3))

# Make HeightLimit LtdHeight Boolean 
df[,"LtdHeight"]<-!is.na(df[,"LtdHeight"])
df <- subset(df, select=-LtdHeight)

# Make whether there is an extention Boolean Ext
df[,"Ext"]<-!is.na(df[,"Ext"])
df <- subset(df, select=-Ext)

#################################################
# Delete Address - not useful string, most in other
df <-subset(df, select=-Address)
# Delete Owner Name, not useful string
df <- subset(df, select=-OwnerName)

# Delete X-cord y-cord, not useful
df <- subset(df, select=-c(XCoord, YCoord))

# Delete Dataset Version Number
df <- subset(df, select=-Version)

#################################################
# Delete variables that directly mean Residential

# Areas give away if Residential
df <- subset(df, select=-c(ResArea,ComArea,OfficeArea,RetailArea,FactryArea,OtherArea))
# Zone Dist may directly tell us residential / had many NA
df <- subset(df, select=-c(ZoneDist1,ZoneDist2,ZoneDist3,ZoneDist4))
# Delete Commercial Overlay
df <- subset(df, select=-c(Overlay1,Overlay2))
# Delete Number of Residential Units
df <- subset(df, select=-UnitsRes)
# Delete CondoNo, only residential have non 0
df <- subset(df, select=-CondoNo)

#################################################
# Dealing with NA

# OwnerType According to specs NA usually means Privately Own 17005
df[is.na(df[,"OwnerType"]), "OwnerType"] <- "P"


# Removing (not useful) Columns with Majority NA
#ZMCode 19671, not useful
df <- subset(df, select=-ZMCode)

#Tax Map has alot of other
df <- subset(df, select=-TaxMap)

#EDesigNum 19771
df <- subset(df, select=-EDesigNum)

#APPDate 18645
df <- subset(df, select=-APPDate)

# Removing other NA's
df <- na.omit(df)

#################################################
# Remove duplicate Borough indicators
df <- subset(df, select=-Borough)

#################################################
# Print to file
write.csv(df, "nyc-buildings/BK_processed.csv")
