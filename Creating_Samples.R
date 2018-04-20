#################################################
#  Company    : Stevens 
#  Project    : CS 513 Final Project - NYC Buidling Data
#  Purpose    : Creating Samples
#  Name 1     : Mrunmayee Salunke(10429075)
#  Name 2     : Heli Choksi (10430926)
#  Name 3     : Weronika Zamlynny (10397182)
#  Date       : 05/02/2018
#  Comments   : Creates samples of 20,000 datapoints to work with.
# I pledge my honor that I have abided by the Stevens Honor System.

rm(list=ls())
#################################################
# NOTE: DATA IS BEING LOADED FROM A LOCAL FILE IN CURRENT WORKING DIRECTORY
setwd("C:/Users/wzaml/Documents/Stevens/11_Spring2018/CS513_DataMining/CS513")
#################################################

data_files <- c("nyc-buildings/BK.csv", "nyc-buildings/BX.csv", 
                "nyc-buildings/MN.csv", "nyc-buildings/QN.csv", 
                "nyc-buildings/SI.csv")
sample_files <- c("nyc-buildings/BK_sample.csv", "nyc-buildings/BX_sample.csv", 
                "nyc-buildings/MN_sample.csv", "nyc-buildings/QN_sample.csv", 
                "nyc-buildings/SI_sample.csv")

for(i in 1:length(data_files)){
  # Load data
  df <- read.csv(data_files[i])
  
  # Takes a random sample of selected file
  idx <- sample(seq(nrow(df)), 20000)
  df_sample <- df[idx,]
  write.csv(df_sample, sample_files[i])
}
