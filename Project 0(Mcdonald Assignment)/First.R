setwd("E:/000GL/000 0Projects/001 Mcdonals/office")
getwd()

myData = read.csv("Mcdonald.csv")
myData

summary(myData)
attach(myData)

install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)

library(ggplot2)

##Problem 1
Table1 = myData %>%   group_by(Category) %>% summarise(counts = n())
Table1
Graph1 = ggplot(Table1, aes(x = reorder(Category, -counts), y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3)
Graph1

names(myData)


#Problem 2

par(mar=c(4,15,4,4),mfrow=c(1,1))

attach(myData)
boxplot(Total.Fat                    
        ,Total.Fat....Daily.Value.
        ,Saturated.Fat                
        ,Saturated.Fat....Daily.Value.
        ,Trans.Fat                    
        ,Cholesterol....Daily.Value.       
        ,Carbohydrates
        ,Carbohydrates....Daily.Value.
        ,Dietary.Fiber
        ,Dietary.Fiber....Daily.Value.
        ,Sugars
        ,Protein    
        ,Calcium....Daily.Value.
        ,Iron....Daily.Value.
        ,names = c("Total.Fat"                    
                  ,"Total.Fat....Daily.Value."
                  ,"Saturated.Fat"                
                  ,"Saturated.Fat....Daily.Value."
                  ,"Trans.Fat"                    
                  ,"Cholesterol....Daily.Value."  
                  ,"Carbohydrates"
                  ,"Carbohydrates....Daily.Value."
                  ,"Dietary.Fiber"
                  ,"Dietary.Fiber....Daily.Value."
                  ,"Sugars"
                  ,"Protein"                      
                  ,"Calcium....Daily.Value."
                  ,"Iron....Daily.Value.")
        ,horizontal = TRUE
        ,las =2
        ,main = "Box Plot of Variables",
        xlab = "gms",
        col = "orange",
        border = "brown"
        )

boxplot(Sodium
        ,Sodium....Daily.Value.  
        ,Calories
        ,Calories.from.Fat
        ,Cholesterol
        ,Vitamin.A....Daily.Value.
        ,Vitamin.C....Daily.Value.
        ,names = c("Sodium"
                   ,"Sodium....Daily.Value."       
                   ,"Calories"
                   ,"Calories.from.Fat"
                   ,"Cholesterol"
                   ,"Vitamin.A....Daily.Value."
                   ,"Vitamin.C....Daily.Value.")
        ,horizontal = TRUE
        ,las =2
        ,main = "Box Plot of Variables",
        xlab = "gms",
        col = "orange",
        border = "brown"
)




###Problem 4

Table2 = myData %>%   group_by(Category) %>% summarise(Cholesterol.per.Value. = sum(Cholesterol....Daily.Value.))
Table2


Graph2 = ggplot(Table2, aes(x = reorder(Category, -Cholesterol.per.Value.), y = Cholesterol.per.Value.)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = Cholesterol.per.Value.), vjust = -0.3)
Graph2


##Problem 5
max(myData$Sodium)


x= toString(myData[myData$Sodium == max(myData$Sodium),2])
x



##Problem 6

myData %>%  select(Item,Saturated.Fat) %>%
  arrange(desc(Saturated.Fat)) %>%
  slice(1:4) 


##
install.packages("corrplot")
library(corrplot)

str(myData)
Test3 <- myData [, 4:24]
str(Test3)

Data_cor <- cor(Test3)
Data_cor

for (i in 1:nrow(Data_cor)){
  correlations <-  which((Data_cor[i,] > 0.85) & (Data_cor[i,] != 1))
  if(length(correlations)> 0){
    print(colnames(Test3)[i])
    print(correlations)
  }
}

par(mar=c(2,2,2,2),mfrow=c(1,1))
mar=c(0,0,0,0)

corrplot(Data_cor, method = "ellipse")
corrplot(Data_cor, method = "number")


cex.before <- par("cex")
par(cex = 0.5)
corrplot(Data_cor, method = "number")
par(cex = cex.before)


