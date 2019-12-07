#libraries Required
library(tidyverse)
library(dplyr)
library(ggplot2)
library(DataExplorer)

#Setting the Working Directory
setwd("E:/000GL/000 0Projects/002 Project Cold Storage")
getwd()

# Importing Data
## Import the Cold_Storage_Temp_Data.csv
Cold_Storage_Temp = read.csv("02 Cold_Storage_Temp_Data.csv")

Cold_Storage_Temp$Date = as.factor(Cold_Storage_Temp$Date)

Cold_Storage_Temp$Month = factor(Cold_Storage_Temp$Month  ,levels = c("Jan", "Feb","Mar","Apr","May", "Jun", "Jul", "Aug","Sep", "Oct","Nov","Dec"))

# General Analysis
#Variable Identification
##Check the Class of Data
class(Cold_Storage_Temp)

## First Inspection of Dataset using str
str(Cold_Storage_Temp)

## Find the name of variable
names(Cold_Storage_Temp)

## find the dimension of Data
dim(Cold_Storage_Temp)

## find first 6 elements of Data
head(Cold_Storage_Temp)

## find last 5 elements of Data
tail(Cold_Storage_Temp)

## find summary of myData to get Min,median,Mean and Max with First and 3rd quartile.
summary(Cold_Storage_Temp)

## plot the missing value
plot_missing(Cold_Storage_Temp)

##check NA
is.na(Cold_Storage_Temp)

#Univarient analysis
##Season
summary(Cold_Storage_Temp$Season)

##Month
summary(Cold_Storage_Temp$Month)

##TEMPERATURE
summary(Cold_Storage_Temp$Temperature)
sd(Cold_Storage_Temp$Temperature)

boxplot(Cold_Storage_Temp$Temperature,las =2
        ,main = "Box Plot of Temperature"
        ,col = "orange"
        ,border = "brown")

hist(Cold_Storage_Temp$Temperature,
     main="Histogram for Temperature", 
     xlab="Temperature", 
     border="brown", 
     col="orange",
     )

#Bivariate Analysis
## Analysis of Cold_Storage_Temp$Season
###Table of Season vs DaysCount , Aveerage Temp , Month Count
Season_Table  = Cold_Storage_Temp %>% group_by(Season) %>% 
                                      summarise(DaysCount = n(), 
                                                AverageTemp = mean(Temperature),
                                                monthcount = n_distinct(Month)) 

Season_Table

### Bar Plot of Average Season Temperature
ggplot(data=Season_Table, aes(x=Season, y=AverageTemp ) ) +
  geom_bar(stat="identity", fill="orange", width=0.9)+
  geom_text(aes(label=sprintf("%0.2f", round(AverageTemp, digits = 2))),color="white", vjust=1.6, size=4) +
  labs(title = "Season Vs Avg Temp")
  

### Box Plot of Season wise Temperature
ggplot(Cold_Storage_Temp, aes(x=Season, y=Temperature)) + 
  geom_boxplot(color="red", fill="orange", alpha=0.2)+
  labs(title = "Season Vs Temp")


## Analysis of Cold_Storage_Temp$Month
### Table of Month vs, Day, Average Temp
Month_Table = Cold_Storage_Temp %>%   group_by(Month) %>% 
                                      summarise(DaysCount = n(),
                                                AverageTemp = mean(Temperature))

Month_Table

### BarPlot of Month vs Average Temp
ggplot(data=Month_Table, aes(x=Month, y=AverageTemp ) ) +
  geom_bar(stat="identity", fill="orange", width=0.9)+
  geom_text(aes(label=sprintf("%0.2f", round(AverageTemp, digits = 2))),color="white", vjust=1.6, size=4) +
  labs(title = "Month Vs Avg Temp")


### BoxPlot of Month wise Temperature
ggplot(Cold_Storage_Temp, aes(x=Month, y=Temperature)) + 
  geom_boxplot(color="red", fill="orange", alpha=0.2)+ 
  labs(title = "Month wise Temp")


### Day wise Temperature of each month
ggplot(data = Cold_Storage_Temp, aes(x= as.numeric(Date),y=Temperature)) + 
  geom_line(aes(colour=Month))+
  facet_wrap(~Month) +
  labs(title = "Month wise Temp", x = "Date")


#Problem 1
## Q1. Mean cold storage temperature for Summer, Winter and Rainy Season
mean_temp_of_seasons = Cold_Storage_Temp %>% 
  group_by(Season) %>% 
  summarize(average.Temp = mean(Temperature))
mean_temp_of_seasons


## Q2.overall mean for the full year 
Mean_Temp = mean(Cold_Storage_Temp$Temperature)
Mean_Temp

## Q3. Standard Deviation for the full year 
SD_of_Temp = sd(Cold_Storage_Temp$Temperature)
SD_of_Temp

## Q4. probability of temperature having fallen below 2 deg C
Prob_for_less_than_2 = pnorm(2, mean=Mean_Temp, sd=SD_of_Temp, lower.tail=TRUE)
Prob_for_less_than_2

## Q5. probability of temperature having gone above 4 deg C 
Prob_for_more_than_4 = pnorm(4, mean=Mean_Temp, sd=SD_of_Temp, lower.tail=FALSE)
Prob_for_more_than_4

## Q6. penalty for the AMC Company
Probibilty_Temp_Outside_2and4 = Prob_for_less_than_2 + Prob_for_more_than_4

if (Probibilty_Temp_Outside_2and4 <= 0.025) {
  print("No Penalty")
} else if (Probibilty_Temp_Outside_2and4 > 0.025 &&  Probibilty_Temp_Outside_2and4 <= 0.05) {
  print("Penalty is 10% of the AMC fee")
} else{
  print("Penalty is 25% of the AMC fee")
}

#Problem 2.1 Z test 

#H0 Hypothsis : Mu ??? 3.9
#H1 Hypothsis : Mu > 3.9
#Read the "01 Cold_Storage_Mar2018.csv" file
Cold_Storage_Mar = read.csv("01 Cold_Storage_Mar2018.csv")
Cold_Storage_Mar
summary(Cold_Storage_Mar)

Cold_Storage_Mar = read.csv("01 Cold_Storage_Mar2018.csv")

#  H0 Hypothsis : Mu ??? 3.9
# H1 Hypothsis : Mu > 3.9
# Find Mean of Temperature of sample data
Mean_Mar = mean(Cold_Storage_Mar$Temperature)
Mean_Mar
# Find SD of Sample Temperature
SD_of_Mar = sd(Cold_Storage_Mar$Temperature)
SD_of_Mar
# Mean Value
Mean_Val = 3.9
#No. of Observation
n=35

#Calculate Z value 
zval = (Mean_Mar - Mean_Val)/(SD_of_Temp/n^0.5)
zval

#Calculate Pvalue
Pval = pnorm(zval)
Pval

# Find The Z critical
zcrtical = qnorm(0.90)
zcrtical

# Find the Standard Error
sd_err = SD_of_Temp/(n^0.5)
sd_err

# Find the Critical Temprature
Tempcrit = (zcrtical*sd_err)+Mean_Val
Tempcrit


##Q2 
t.test(Cold_Storage_Mar$Temperature,mu=3.9,alternative ="greater",conf.level = 0.9)

