---
output: html_document
editor_options: 
  chunk_output_type: console
---


# Load data

```{r}
setwd("C:/Users/jmolirol/Desktop/NYCABS")
rm(list=ls())

library(AER)
library(car)
library(FactoMineR)
data("SwissLabor")
```

# Take a look

```{r}
df<-SwissLabor
head(df)
summary(df)

# EDA - Univariant - Factor
barplot(table(df$participation))
barplot(table(df$participation),col="cyan",main="Diagram a de Barres")

# EDA - Univariant - Numèrica

hist(df$income)
hist(df$income,15,col=rainbow(8),main="Histograma Income")
hist(df$income,15,freq=F,col=rainbow(8),main="Histograma Income")
boxplot(df$income)

```
September 23rd, 2020

#Examlple for Imputation, Outlier detection
```{r}
install.packages("mvoutlier")
library(mvoutlier)
vout<-aq.plot(df[,2:4], delta=qchisq(0.95, df=3),alpha=0.05)
#Si son normals multivariants funciona correctamente, llavors funciona
#la deteccio de outliers multivariants funciona correctament.
#Normalment es sol eliminar aquelles columnes que tenen poc valors numerics.

```

```{r}
summary(df)
llista<-sample(1:nrow(SwissLabor),40);llista
df<-SwissLabor
df[llista,"age"]<-NA
summary(df) #podem veure com en 

install.packages("missMDA")
library(missMDA)
# Numeric imputation only explanatory variables - never forget 
vars_con<-names(df)[2:6]
summary(df[,vars_con])
res.input<-imputePCA(df[,vars_con],ncp=4)
summary(res.input$completeObs)

#Validation COMPULSORY
par(mfrow=c(1,3))
hist(df$age,col="red")
hist(SwissLabor$age,col="green")
hist(res.input$completeObs[,2],col="blue")
quantile(df$age,seq(0,1,0.1),na.rm=T)
quantile(SwissLabor$age,seq(0,1,0.1),na.rm=T)
round(quantile(res.input$completeObs[,2],seq(0,1,0.1),na.rm=T),dig=1)
```

```{r}
llista<-sample(1:nrow(SwissLabor),40);llista
df<-SwissLabor
df[llista,"participation"]<-NA
summary(df)
install.packages(missMDA)
library(missMDA)
# Categorical imputation
vars_dis<-names(df)[c(1,7)]
summary(df[,vars_dis])
nb <-estim_ncpMCA(df[, vars_dis],ncp.max=25)
res.input<-imputeMCA(df[,vars_dis],ncp=10)
summary(res.input$completeObs)

#Validation is COMPULSORY
par(mfrow=c(1,3))
barplot(table(df$participation), color = "red")
barplot(table(SwissLabor~participation), color = "green")
barplot(table(res.input$completeObs), color = "blue")
```

