setwd("C:/Users/jmolirol/Desktop/NYCABS")
install.packages(c("FactoMineR","car","knitr"))
library(car)
library(FactoMineR)
library(knitr)

#Load data
#if you have defined tha path you don't need specify it in the read.table
df<-read.table("green_tripdata_2016-01.csv",header=T, sep=",")
dim(df) #size of data.frame
str(df) #Object class and description
names(df) #List of variable names
set.seed(28061963) #Create a seed for the sample 
sam<-as.vector(sort(sample(1:nrow(df),5000)))
head(df) #Take a look to the firs rows/instances
df<-df[sam,] # Subset of rows from my df
summary(df) #Data summary
save(list=c("df"), file="MyFile.RData") #Save a RData from the Environment
