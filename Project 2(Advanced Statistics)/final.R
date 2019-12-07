#libraries Required
library(tidyverse)
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(corrplot)
library(car)
library(Metrics)
library(GPArotation)
library(MASS)
library(psych)
#Setting the Working Directory
setwd("E:/000GL/000 0Projects/003 Factor Hair Revised")
getwd()

# Importing Data
myData = read.csv("Factor-Hair-Revised.csv")
myData
myData$ID = as.factor(myData$ID)

# General Analysis
#Variable Identification
##Check the Class of Data
class(myData)

## First Inspection of Dataset using str
str(myData)

## Find the name of variable
names(myData)

## find the dimension of Data
dim(myData)

## find first 6 elements of Data
head(myData)

## find last 5 elements of Data
tail(myData)

## find summary of myData to get Min,median,Mean and Max with First and 3rd quartile.
summary(myData)

## plot the missing value
plot_missing(myData)

##removig id from data
#Removing id variable
myDataM <- subset(myData, select = -c(1))
dim(myDataM)



##UnivarientAnalysis

###Summary
summary(myDataM)

##Histogram 
plot_histogram(myData,nrow = 4,ncol = 4)

##Box Plot
par(mar=c(4,10,4,4))
boxplot(myDataM,
        horizontal = TRUE
        ,las =2
        ,main = "Box Plot"
        ,col = "orange"
        ,border = "brown")


##Calculate SD of Each Data
myDataM %>%
  summarise_each(funs(sd(., na.rm=TRUE)))


##Bivarient Analysis

##Scatterplot
plot(myDataM, col="blue", cex.axis=0.75,cex.lab=5, pch=20)

##Corelation
Data_cor <- cor(myDataM)
cex.before <- par("cex")
par(cex = 0.6)
corrplot(Data_cor)


##Corelation
corrplot(Data_cor, method = "number", number.digits = 2)
par(cex = cex.before)

##Variables having high corelation of more than 0.6 or -0.6
for (i in 1:nrow(Data_cor)){
  correlations <-  which((Data_cor[i,] > 0.6 | Data_cor[i,] < -0.6) & (Data_cor[i,] != 1))
  if(length(correlations)> 0){
    print(colnames(myDataM)[i])
    print(correlations)
  }
}



#Simple Linear Regression of Dependent with each Idpendent Variable
myDataSLR = myDataM

view(myDataSLR)
dim(myDataSLR)


#SLR Model for ProdQual & Satisfaction
qplot(myDataSLR$ProdQual,myDataSLR$Satisfaction, main = "Plot between Satisfaction & Product Quality",
      xlab = "Product Quality", ylab = "Satisfaction")
cor(myDataSLR$ProdQual,myDataSLR$Satisfaction)
modSat_PQ =  lm(Satisfaction ~ ProdQual, data = myDataSLR)
summary(modSat_PQ)

#SLR Model for Ecom & Satisfaction
qplot(myDataSLR$Ecom,myDataSLR$Satisfaction, main = "Plot between Satisfaction & Ecom",
      xlab = "Ecom", ylab = "Satisfaction")
cor(myDataSLR$Ecom,myDataSLR$Satisfaction)
modSat_Ecom =  lm(Satisfaction ~ Ecom, data = myDataSLR)
summary(modSat_Ecom)


#SLR Model for TechSup & Satisfaction
qplot(myDataSLR$TechSup,myDataSLR$Satisfaction, main = "Plot between Satisfaction & TechSup",
      xlab = "TechSup", ylab = "Satisfaction")
cor(myDataSLR$TechSup,myDataSLR$Satisfaction)
modSat_TechSup =  lm(Satisfaction ~ TechSup, data = myDataSLR)
summary(modSat_TechSup)

#SLR Model for CompRes & Satisfaction
qplot(myDataSLR$CompRes,myDataSLR$Satisfaction, main = "Plot between Satisfaction & CompRes",
      xlab = "CompRes", ylab = "Satisfaction")
cor(myDataSLR$CompRes,myDataSLR$Satisfaction)
modSat_CompRes =  lm(Satisfaction ~ CompRes, data = myDataSLR)
summary(modSat_CompRes)

#SLR Model for Advertising & Satisfaction
qplot(myDataSLR$Advertising,myDataSLR$Satisfaction, main = "Plot between Satisfaction & Advertising",
      xlab = "Advertising", ylab = "Satisfaction")
cor(myDataSLR$Advertising,myDataSLR$Satisfaction)
modSat_Advertising =  lm(Satisfaction ~ Advertising, data = myDataSLR)
summary(modSat_Advertising)

#SLR Model for ProdLine & Satisfaction
qplot(myDataSLR$ProdLine,myDataSLR$Satisfaction, main = "Plot between Satisfaction & ProdLine",
      xlab = "ProdLine", ylab = "Satisfaction")
cor(myDataSLR$ProdLine,myDataSLR$Satisfaction)
modSat_ProdLine =  lm(Satisfaction ~ ProdLine, data = myDataSLR)
summary(modSat_ProdLine)

#SLR Model for SalesFImage & Satisfaction
qplot(myDataSLR$SalesFImage,myDataSLR$Satisfaction, main = "Plot between Satisfaction & SalesFImage",
      xlab = "SalesFImage", ylab = "Satisfaction")
cor(myDataSLR$SalesFImage,myDataSLR$Satisfaction)
modSat_SalesFImage =  lm(Satisfaction ~ SalesFImage, data = myDataSLR)
summary(modSat_SalesFImage)

#SLR Model for ComPricing & Satisfaction
qplot(myDataSLR$ComPricing,myDataSLR$Satisfaction, main = "Plot between Satisfaction & ComPricing",
      xlab = "ComPricing", ylab = "Satisfaction")
cor(myDataSLR$ComPricing,myDataSLR$Satisfaction)
modSat_ComPricing =  lm(Satisfaction ~ ComPricing, data = myDataSLR)
summary(modSat_ComPricing)

#SLR Model for WartyClaim & Satisfaction
qplot(myDataSLR$WartyClaim,myDataSLR$Satisfaction, main = "Plot between Satisfaction & WartyClaim",
      xlab = "WartyClaim", ylab = "Satisfaction")
cor(myDataSLR$WartyClaim,myDataSLR$Satisfaction)
modSat_WartyClaim =  lm(Satisfaction ~ WartyClaim, data = myDataSLR)
summary(modSat_WartyClaim)

#SLR Model for OrdBilling & Satisfaction
qplot(myDataSLR$OrdBilling,myDataSLR$Satisfaction, main = "Plot between Satisfaction & OrdBilling",
      xlab = "OrdBilling", ylab = "Satisfaction")
cor(myDataSLR$OrdBilling,myDataSLR$Satisfaction)
modSat_OrdBilling =  lm(Satisfaction ~ OrdBilling, data = myDataSLR)
summary(modSat_OrdBilling)

#SLR Model for DelSpeed & Satisfaction
qplot(myDataSLR$DelSpeed,myDataSLR$Satisfaction, main = "Plot between Satisfaction & DelSpeed",
      xlab = "DelSpeed", ylab = "Satisfaction")
cor(myDataSLR$DelSpeed,myDataSLR$Satisfaction)
modSat_DelSpeed =  lm(Satisfaction ~ DelSpeed, data = myDataSLR)
summary(modSat_DelSpeed)


