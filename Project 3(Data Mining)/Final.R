# Load the required libraries

library(tidyverse)
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(caTools)
library(caret)
library(randomForest)
library(data.table)
library(ROCR)
library(ineq)
library(corrplot)
library(InformationValue)

#Setting the Working Directory
setwd ("E:/000GL/000 0Projects/004/Project/Final")
getwd()

#Importing the data
TheraData <- read.csv("Thera Bank_Personal_Loan_Modelling-data.csv")

summary(TheraData)

names(TheraData)
names(TheraData) <- c("ID"
                   ,"Age"
                   ,"Experience"
                   ,"Income"
                   ,"ZIPCode"
                   ,"FamilyMembers"
                   ,"CCAvg"
                   ,"Education"
                   ,"Mortgage"             
                   ,"PersonalLoan"
                   ,"SecuritiesAccount"
                   ,"CDAccount"           
                   ,"Online"
                   ,"CreditCard"
)

summary(TheraData)

#EDA
myData = TheraData


myData$ID = as.factor(myData$ID)
myData$FamilyMembers = as.factor(myData$FamilyMembers)
myData$Education = as.factor(myData$Education)
myData$PersonalLoan = as.factor(myData$PersonalLoan)
myData$SecuritiesAccount = as.factor(myData$SecuritiesAccount)
myData$CDAccount = as.factor(myData$CDAccount)
myData$Online = as.factor(myData$Online)
myData$CreditCard = as.factor(myData$CreditCard)


summary(myData)

##Check NA
plot_missing(myData)
sum(is.na(myData))

##Also treating the negetive work experience by removing them
myData$Experience[myData$Experience < 0] = NA
sum(is.na(myData))

##70 values are missing which is less than 3% of the data so we can remove the NA Data
MainData = na.omit(myData)

dim(MainData)

summary(MainData)

class(MainData)

str(MainData)
#SUMMARY OF Main Data

names(MainData)
head(MainData)
tail(MainData)


##Univariant Analysis
##Histogram of Continious Variable
plot_histogram(MainData)
names(MainData)

summary(MainData$Age)
sd(MainData$Age)
boxplot(MainData$Age
        ,horizontal = TRUE
        ,las =2
        ,main = "Age"
        ,col = "orange"
        ,border = "brown")

summary(MainData$Experience)
sd(MainData$Experience)
boxplot(MainData$Experience
        ,horizontal = TRUE
        ,las =2
        ,main = "Experience"
        ,col = "orange"
        ,border = "brown")

summary(MainData$Income)
sd(MainData$Income)
boxplot(MainData$Income
        ,horizontal = TRUE
        ,las =2
        ,main = "Income"
        ,col = "orange"
        ,border = "brown")

summary(MainData$CCAvg)
sd(MainData$CCAvg)
boxplot(MainData$CCAvg
        ,horizontal = TRUE
        ,las =2
        ,main = "CCAvg"
        ,col = "orange"
        ,border = "brown")

summary(MainData$Mortgage)
sd(MainData$Mortgage)
boxplot(MainData$Mortgage
        ,horizontal = TRUE
        ,las =2
        ,main = "Mortgage"
        ,col = "orange"
        ,border = "brown")

summary(MainData$Education)
barplot(table(MainData$Education), main="Education",
        xlab="Education Level",
        names.arg=c("Undergrad","Graduate", "Professional"),
        ylab = "Frequency")

summary(MainData$FamilyMembers)
barplot(table(MainData$FamilyMembers), main="FamilyMembers",
        xlab="FamilyMembers",
        ylab = "Frequency")

summary(MainData$PersonalLoan)
barplot(table(MainData$PersonalLoan), main="Customer accept the personal loan offered in the last campaign?",
        xlab="Personal Loan",
        names.arg=c("NO","YES"),
        ylab = "Count")

summary(MainData$SecuritiesAccount)
barplot(table(MainData$SecuritiesAccount), main="Customer have a securities account with the bank",
        xlab="Education Level",
        names.arg=c("NO","YES"),
        ylab = "COUNT")

summary(MainData$CDAccount)
barplot(table(MainData$CDAccount), main="Customer have CD account with the bank.",
        xlab="CD account with the bank",
        names.arg=c("NO","YES"),
        ylab = "Frequency")

summary(MainData$Online)
barplot(table(MainData$Online), main="Online Banking",
        xlab="Education Level",
        names.arg=c("NO","YES"),
        ylab = "Frequency")

summary(MainData$CreditCard)
barplot(table(MainData$CreditCard), main="Uses Bank Credit Card",
        xlab="Education Level",
        names.arg=c("NO","YES"),
        ylab = "Frequency")

ZipTemp = MainData
ZipTemp$ZIPCode = as.factor(ZipTemp$ZIPCode)
summary(ZipTemp)

##Bivarient Analysis

###PersonalLoan vs Age
ggplot(MainData, aes(x=PersonalLoan, y=MainData$Age)) + 
  geom_boxplot(color="orange", fill="orange", alpha=0.2) +
  scale_x_discrete(labels = c('NO','YES'))+
  labs(title = "PersonalLoan Acceptance Vs Age")

###PersonalLoan vs Experience
ggplot(MainData, aes(x=PersonalLoan, y=MainData$Experience)) + 
  geom_boxplot(color="orange", fill="orange", alpha=0.2) +
  scale_x_discrete(labels = c('NO','YES'))+
  labs(title = "PersonalLoan Acceptance Vs Experience")

###PersonalLoan vs Income
ggplot(MainData, aes(x=PersonalLoan, y=MainData$Income)) + 
  geom_boxplot(color="orange", fill="orange", alpha=0.2) + 
  scale_x_discrete(labels = c('NO','YES'))+
  labs(title = "PersonalLoan Acceptance Vs Income")

###PersonalLoan vs CCAvg
ggplot(MainData, aes(x=PersonalLoan, y=MainData$CCAvg)) + 
  geom_boxplot(color="orange", fill="orange", alpha=0.2) + 
  scale_x_discrete(labels = c('NO','YES'))+
  labs(title = "PersonalLoan Acceptance Vs CCAvg")

###PersonalLoan vs Mortgage
ggplot(MainData, aes(x=PersonalLoan, y=MainData$Mortgage)) + 
  geom_boxplot(color="orange", fill="orange", alpha=0.2) +
  scale_x_discrete(labels = c('NO','YES'))+
  labs(title = "PersonalLoan Acceptance Vs Mortgage")


##PersonalLoan vs Family
table(MainData$PersonalLoan, MainData$FamilyMembers)
###BarPlot
ggplot(MainData, aes(x = PersonalLoan, fill = FamilyMembers))+
  geom_bar(position = 'stack')+
  scale_x_discrete(labels = c('NO','YES'))+
  scale_fill_discrete(name = "Family Members Count")

##PersonalLoan vs Education
table(MainData$PersonalLoan, MainData$Education)
###BarPlot
ggplot(MainData, aes(x = PersonalLoan, fill = Education))+
  geom_bar(position = 'stack')+
  scale_x_discrete(labels = c('NO','YES'))+
  scale_fill_discrete(name = "Education Level", labels = c("Undergrad", "Graduate","Advanced/Professional"))


##PersonalLoan vs Security Account
table(MainData$PersonalLoan, MainData$SecuritiesAccount)
###BarPlot
ggplot(MainData, aes(x = PersonalLoan, fill = SecuritiesAccount))+
  geom_bar(position = 'stack')+
  scale_x_discrete(labels = c('NO','YES'))+
  scale_fill_discrete(name = "Securities Account", labels = c("No", "Yes"))


##PersonalLoan vs CDAccount
table(MainData$PersonalLoan, MainData$CDAccount)
###BarPlot
ggplot(MainData, aes(x = PersonalLoan, fill = CDAccount))+
  geom_bar(position = 'stack')+
  scale_x_discrete(labels = c('NO','YES'))+
  scale_fill_discrete(name = "CDAccount", labels = c("No", "Yes"))

##PersonalLoan vs Online
table(MainData$PersonalLoan, MainData$Online)
###BarPlot
ggplot(MainData, aes(x = PersonalLoan, fill = Online))+
  geom_bar(position = 'stack')+
  scale_x_discrete(labels = c('NO','YES'))+
  scale_fill_discrete(name = "Internet Banking", labels = c("No", "Yes"))



##PersonalLoan vs CreditCard
table(MainData$PersonalLoan, MainData$CreditCard)
###BarPlot
ggplot(MainData, aes(x = PersonalLoan, fill = CreditCard))+
  geom_bar(position = 'stack')+ 
  scale_x_discrete(labels = c('NO','YES'))+
  scale_fill_discrete(name = "Credit Card", labels = c("No", "Yes"))



##Corelation Plot
Data_cor <- cor(TheraData)
cex.before <- par("cex")
par(cex = 0.6 )
corrplot(Data_cor)

corrplot(Data_cor, method = "number" , number.digits = 2 )
par(cex = cex.before)


####Cart Analaysis

LoanData = MainData[,c(-1)]
LoanData$ZIPCode = as.factor(LoanData$ZIPCode)
summary(LoanData)
names(LoanData)
str(LoanData)


##Creating Test and Train Data
split <- sample.split(LoanData$PersonalLoan, SplitRatio = 0.7)
train<- subset(LoanData, split == TRUE)
test<- subset( LoanData, split == FALSE)

prop.table(table(train$PersonalLoan))
prop.table(table(test$PersonalLoan))
table(train$PersonalLoan)
table(test$PersonalLoan)


attach(train)
##Viewing the Development sample

View(train)
str(train)
dim(train)
names(train)

##Setting the control parameters for rpart
#minsplit: if the number of records in a given node falls below a threshold, the node will not be split further.
#minbucket: minimum records in a terminal node. if the records are less, that bucket will not be created.
#Terminal node (minbucket) should not be less than 2-3% of starting population.
0.02*3451
0.03*3451

#minsplit = 3(minbucket)  
#xval divides the entire dataset into mutually exclusive and collectively exhaustive segments.
#Model is built on xval-1 segments and 1 is used for testing.
#cp = cost complexity parameter
r.ctrl = rpart.control(minsplit=210, minbucket = 70, cp = 0, xval = 10)

#Using rpart to build the tree

train.t <- rpart(formula = PersonalLoan ~ ., data = train[,-c(9)], method = "class", control = r.ctrl)
#train.t <- rpart(formula = PersonalLoan ~ ., data = train[,-9], method = "class")
#LoanData.t <- rpart(formula = PersonalLoan ~ ., data = LoanData.dev[,-1], method = "class")

train.t
fancyRpartPlot(train.t)

##To see how the tree performs
printcp(train.t)
plotcp(train.t)

##Since Vlaue of x error start increasing we have to prune the tree at cp = 0.030
train.tree<- prune(train.t, cp=  0.030  ,"CP")
train.tree
printcp(train.tree)
fancyRpartPlot(train.tree, uniform=TRUE,  main="Pruned Classification Tree")



##Scoring
train$predict.class = predict(train.t, train, type="class")
train$predict.score = predict(train.t, train)

## We can use the confusionMatrix function of the caret package 

library(caret)
train$predict.class =as.factor(train$predict.class)
train$PersonalLoan=as.factor(train$PersonalLoan)
confusionMatrix(train$PersonalLoan,train$predict.class)

#Scoring the test sample
test$predict.class <- predict(train.t, test, type="class")
test$predict.score <- predict(train.t, test)

## Confusion Matrix for Test Data

test$predict.class <-as.factor(test$predict.class)
test$PersonalLoan<-as.factor(test$PersonalLoan)
confusionMatrix(test$PersonalLoan,test$predict.class)


###Perfomance Matrix for Cart

##### Performance Matrix of CART Train 
### Deciling and Rank Order Table
str(train)
train$prob1 <-train$predict.score[,2]
test$prob1 <-test$predict.score[,2]


probsCart=seq(0,1,length=11)
probsCart
qsCart=quantile(train$prob1, probsCart)
qsCart
train$deciles=cut(train$prob1, unique(qsCart),include.lowest = TRUE,right=FALSE)
table(train$deciles)

train$PersonalLoan <-ifelse(train$PersonalLoan == "1", 1,0)
train$PersonalLoan <-as.numeric(train$PersonalLoan)
test$PersonalLoan <-ifelse(test$PersonalLoan == "1", 1,0)
test$PersonalLoan <-as.numeric(test$PersonalLoan)

trainDT = data.table(train)

rankTbl = trainDT[, list(
  cnt = length(PersonalLoan), 
  cnt_tar1 = sum(PersonalLoan == 1), 
  cnt_tar0 = sum(PersonalLoan == 0)
), 
by=deciles][order(-deciles)]

print(rankTbl)

rankTbl$rrate = round(rankTbl$cnt_tar1 / rankTbl$cnt,4)*100;
rankTbl$cum_resp = cumsum(rankTbl$cnt_tar1)
rankTbl$cum_non_resp = cumsum(rankTbl$cnt_tar0)
rankTbl$cum_rel_resp = round(rankTbl$cum_resp / sum(rankTbl$cnt_tar1),4)*100;
rankTbl$cum_rel_non_resp = round(rankTbl$cum_non_resp / sum(rankTbl$cnt_tar0),4)*100;
rankTbl$ks = abs(rankTbl$cum_rel_resp - rankTbl$cum_rel_non_resp);

print(rankTbl)

### ROCR and ineq packages to compute AUC, KS and gini

predObj = prediction(train$prob1, train$PersonalLoan)
perf = performance(predObj, "tpr", "fpr")
plot(perf)
KS = max(perf@y.values[[1]]-perf@x.values[[1]])
KS
auc = performance(predObj,"auc"); 
auc = as.numeric(auc@y.values)
auc
gini = ineq(train$prob1, type="Gini")
gini


### Concordance and discordcance ratios:
Concordance(actuals=train$PersonalLoan, predictedScores=train$prob1)


##### Performance Matrix of CART Test
### Deciling and Rank Order Table
probs=seq(0,1,length=11)
probs
qs=quantile(test$prob1, probs)
qs
test$deciles=cut(test$prob1, unique(qs),include.lowest = TRUE,right=FALSE)
table(test$deciles)


testDT = data.table(test)
rankTbl = testDT[, list(
  cnt = length(PersonalLoan), 
  cnt_tar1 = sum(PersonalLoan), 
  cnt_tar0 = sum(PersonalLoan == 0)
), 
by=deciles][order(-deciles)]

rankTbl$rrate = round(rankTbl$cnt_tar1 / rankTbl$cnt,4)*100;
rankTbl$cum_resp = cumsum(rankTbl$cnt_tar1)
rankTbl$cum_non_resp = cumsum(rankTbl$cnt_tar0)
rankTbl$cum_rel_resp = round(rankTbl$cum_resp / sum(rankTbl$cnt_tar1),4)*100;
rankTbl$cum_rel_non_resp = round(rankTbl$cum_non_resp / sum(rankTbl$cnt_tar0),4)*100;
rankTbl$ks = abs(rankTbl$cum_rel_resp - rankTbl$cum_rel_non_resp);

print(rankTbl)

### ROCR and ineq packages to compute AUC, KS and gini


predObj = prediction(test$prob1, test$PersonalLoan)
perf = performance(predObj, "tpr", "fpr")
plot(perf)
KS = max(perf@y.values[[1]]-perf@x.values[[1]])
KS
auc = performance(predObj,"auc"); 
auc = as.numeric(auc@y.values)
auc
gini = ineq(test$prob1, type="Gini")
gini



### Concordance and discordcance ratios:
library("InformationValue")
Concordance(actuals=test$PersonalLoan, predictedScores=test$prob1)
































############## Random Forests ########


RFData = MainData[,c(-1)]
summary(RFData)
names(RFData)
str(RFData)


##Creating Test and Train Data
RFsplit <- sample.split(RFData$PersonalLoan, SplitRatio = 0.7)
RFtrain<- subset(RFData, split == TRUE)
RFtest<- subset( RFData, split == FALSE)
names(RFtrain)

prop.table(table(RFtrain$PersonalLoan))
prop.table(table(RFtest$PersonalLoan))
table(RFtrain$PersonalLoan)
table(RFtest$PersonalLoan)
attach(RFtrain)

##Creating random forest
#install.packages("randomForest")
#ntree: number of trees to grow
#mtry: number of variables to be considered for split
#nodesize: minimum size of terminal nodes

set.seed(123)
Loan.RF <- randomForest(as.factor(RFtrain$PersonalLoan) ~ ., data = RFtrain[,-c(9)], 
                        ntree=101, mtry=5, importance=TRUE)
print(Loan.RF)

------------
  
## Importance function
## type is either 1 or 2, specifying the type of importance measure 
### (1=mean decrease in accuracy, 2=mean decrease in node impurity). 
  
importance(Loan.RF, type=2)
varImpPlot(Loan.RF, type=2)

### Higher value of Mean Decrease in Ginni indicates high importance of the variable

#To choose optimum value of ntree
plot(Loan.RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates")

Loan.RF$err.rate
#choose ntree=30

#stepFactor is a magnitude by which the chosen mtry gets deflated or inflated.
### Set mtry to the default value of sqrt(p) for classification, and p/3 for regression (where p = total number of variables)
### Look to the left is mtry value/step factor
### Look to the right is mtry value * step factor

#Tuning Random Forest
tRF <- tuneRF(x = RFtrain[,-c(9)], 
              y=as.factor(RFtrain$PersonalLoan),
              ntreeTry=30, 
              mtry=5,	
              stepFactor = 1.2, 
              improve = 0.0001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 100, 
              importance=TRUE
)



Loan.RF <- randomForest(as.factor(RFtrain$PersonalLoan) ~ ., data = RFtrain[,-c(9)], 
                        ntree=30, mtry=6, importance=TRUE)

print(Loan.RF)

## Scoring syntax
RFtrain$predict.class <- predict(Loan.RF, RFtrain, type="class")
RFtrain$predict.score <- predict(Loan.RF, RFtrain, type="prob")


### Checking the model accuracy
library(caret)
RFtrain$predict.class <-as.factor( RFtrain$predict.class)
RFtrain$PersonalLoan <-as.factor( RFtrain$PersonalLoan)

confusionMatrix(RFtrain$PersonalLoan , RFtrain$predict.class )


#Scoring the holdout sample
#####
#####
RFtest$predict.class <- predict(Loan.RF, RFtest, type="class")
RFtest$predict.score <- predict(Loan.RF, RFtest, type="prob")

RFtest$predict.class <-as.factor( RFtest$predict.class)
RFtest$PersonalLoan <-as.factor( RFtest$PersonalLoan)

confusionMatrix(RFtest$PersonalLoan , RFtest$predict.class )





##### Performance Matrix
### Deciling and Rank Order Table
RFtrain$prob1 <-RFtrain$predict.score[,2]
RFtest$prob1 <-RFtest$predict.score[,2]
head(RFtrain)


probs=seq(0,1,length=11)
probs
qs=quantile(RFtrain$prob1, probs)
qs
RFtrain$deciles=cut(RFtrain$prob1, unique(qs),include.lowest = TRUE,right=FALSE)
table(RFtrain$deciles)


RFtrain$PersonalLoan <-ifelse(RFtrain$PersonalLoan == "1", 1,0)
RFtrain$PersonalLoan <-as.numeric(RFtrain$PersonalLoan)
RFtest$PersonalLoan <-ifelse(RFtest$PersonalLoan == "1", 1,0)
RFtest$PersonalLoan <-as.numeric(RFtest$PersonalLoan)

trainDT = data.table(RFtrain)

rankTbl = trainDT[, list(
  cnt = length(PersonalLoan), 
  cnt_tar1 = sum(PersonalLoan == 1), 
  cnt_tar0 = sum(PersonalLoan == 0)
), 
by=deciles][order(-deciles)]

print(rankTbl)

rankTbl$rrate = round(rankTbl$cnt_tar1 / rankTbl$cnt,4)*100;
rankTbl$cum_resp = cumsum(rankTbl$cnt_tar1)
rankTbl$cum_non_resp = cumsum(rankTbl$cnt_tar0)
rankTbl$cum_rel_resp = round(rankTbl$cum_resp / sum(rankTbl$cnt_tar1),4)*100;
rankTbl$cum_rel_non_resp = round(rankTbl$cum_non_resp / sum(rankTbl$cnt_tar0),4)*100;
rankTbl$ks = abs(rankTbl$cum_rel_resp - rankTbl$cum_rel_non_resp);

print(rankTbl)

### ROCR and ineq packages to compute AUC, KS and gini

predObj = prediction(RFtrain$prob1, RFtrain$PersonalLoan)
perf = performance(predObj, "tpr", "fpr")
plot(perf)
KS = max(perf@y.values[[1]]-perf@x.values[[1]])
KS
auc = performance(predObj,"auc"); 
auc = as.numeric(auc@y.values)
auc
gini = ineq(RFtrain$prob1, type="Gini")
gini


### Concordance and discordcance ratios:


Concordance(actuals=RFtrain$PersonalLoan, predictedScores=RFtrain$prob1)

#############
#########
#############
########




### Deciling and Rank Order Table

probs=seq(0,1,length=11)
probs
qs=quantile(RFtest$prob1, probs)
qs
RFtest$deciles=cut(RFtest$prob1, unique(qs),include.lowest = TRUE,right=FALSE)
table(RFtest$deciles)


testDT = data.table(RFtest)
rankTbl = testDT[, list(
  cnt = length(PersonalLoan), 
  cnt_tar1 = sum(PersonalLoan), 
  cnt_tar0 = sum(PersonalLoan == 0)
), 
by=deciles][order(-deciles)]

rankTbl$rrate = round(rankTbl$cnt_tar1 / rankTbl$cnt,4)*100;
rankTbl$cum_resp = cumsum(rankTbl$cnt_tar1)
rankTbl$cum_non_resp = cumsum(rankTbl$cnt_tar0)
rankTbl$cum_rel_resp = round(rankTbl$cum_resp / sum(rankTbl$cnt_tar1),4)*100;
rankTbl$cum_rel_non_resp = round(rankTbl$cum_non_resp / sum(rankTbl$cnt_tar0),4)*100;
rankTbl$ks = abs(rankTbl$cum_rel_resp - rankTbl$cum_rel_non_resp);

print(rankTbl)

### ROCR and ineq packages to compute AUC, KS and gini


predObj = prediction(RFtest$prob1, RFtest$PersonalLoan)
perf = performance(predObj, "tpr", "fpr")
plot(perf)
KS = max(perf@y.values[[1]]-perf@x.values[[1]])
auc = performance(predObj,"auc"); 
auc = as.numeric(auc@y.values)
auc
gini = ineq(RFtest$prob1, type="Gini")
gini



### Concordance and discordcance ratios:
library("InformationValue")
Concordance(actuals=RFtest$PersonalLoan, predictedScores=RFtest$prob1)






