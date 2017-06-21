# Description: Credit Card Evaluation Model in R
# Author: Ancheng Leanrt from: http://www.17bigdata.com/%E4%BF%A1%E7%94%A8%E5%8D%A1%E8%AF%84%E5%88%86%E6%A8%A1%E5%9E%8B%EF%BC%88r%E8%AF%AD%E8%A8%80%EF%BC%89.html
# Source: http://blog.csdn.net/csqazwsxedc/article/details/51225156
# Date: 2017/06/12

# Objective: Evaluation Card, predict the possibility of someone encounter a financial crisis in 
# the next 2 years, help the lender make better choice.

# borrower: market, society, individual, company.
# lender: banks

# Background: Based on Basel II Capital Accord, > 90 will be called as violation
# the value will be chosen from the longest violation day in history

## Time Window: the predictor is the last 2 years, the response is in the next 2 years

# ---- Datasets Descirption: Basic Attr; Repayment Ability; Credit; Financial Status; Loan Attr;
# others: Family Num

# --- Read in 
traindata <- read.csv("cs-training.csv", header = TRUE, row.names = 1, stringsAsFactors = FALSE)
str(traindata)
# reset col names
colnames(traindata) <- c("y", paste0("x", 1:10))

if(require(VIM) == 0){
  install.packages("VIM")
}
library(VIM)
# observe the missing data: many models are sensitive to missing data 
matrixplot(traindata) # reds for missing data

# use MICE package to imputing missing data
if(require(mice) == 0){
  install.packages("mice")
}
library(mice)

md.pattern(traindata)

if(require(DMwR) == 0){
  install.packages("DMwR")
}
library(DMwR)

traindata<-knnImputation(traindata,k=10,meth = "weighAvg") # Use KNN to imput
# this knn Impute takes a lot of time, from below we use the saved RData 
#########################
# --- load
load("creditEval_afterknnImpute.RData")
# ---

boxplot(traindata)
x5.outlier <- boxplot.stats(traindata$x5)$out
length(x5.outlier) # the number of outliers

# ---- LOF for local outlier factor: based on density difference: lofactor() in DMwR
# also, clustering methods like DBSCAN and kmeans can be used 

unique(traindata$x2)
traindata<-traindata[-which(traindata$x2==0),]  # extract age == 0 as outliers
# also for x3,7,9
traindata<-traindata[-which(traindata$x3==96),]
traindata<-traindata[-which(traindata$x3==98),]

# ---- Var Analaysis
library(ggplot2)
# check the distribution of 'Age'
ggplot(traindata, aes(x = x2, y = ..density..)) + geom_histogram(fill = "blue", colour = "grey60", size = 0.2, alpha = 0.2) + geom_density()
# check the distribution of 'Income'
ggplot(traindata, aes(x = x5, y = ..density..)) + geom_histogram(fill = "blue", colour = "grey60", size = 0.2, alpha = 0.2) + geom_density() + xlim(1, 20000)
# approximately normal distribution

# --- correlation analysis
library(corrplot)
cor1<-cor(traindata[,1:11])
corrplot(cor1)
corrplot(cor1,method = "number")

#### Before Logistic Regression, multi-linearity needs to be excluded
#### from the corrplot, the correlation between vars are appropriately small

# ---- divide datasets: SMOTE algorithm for super sampling OR caret package
table(traindata$y)

library(caret)
set.seed(1234) 
splitIndex<-createDataPartition(traindata$y,time=1,p=0.5,list=FALSE) 
train<-traindata[splitIndex,] 
test<-traindata[-splitIndex,] 
# check the balance of the divided dataset
prop.table(table(train$y)) 
prop.table(table(test$y))
# 6.6% representatives means OK, we can use this datasets for further analysis

# ---- Logistic Regression: Key Model, the result can be exact the credit evaluation card
# model building
fit<-glm(y~.,train,family = "binomial")
summary(fit)
# x1, x4, x6 failed the test, should be excluded
fit2<-glm(y~x2+x3+x5+x7+x8+x9+x10,train,family = "binomial")
summary(fit2)
# the AIC is even better

# --- prediction and AOC curve
pre <- predict(fit2,test)
# package pROC: compare 2 classifier and auto-annotate the best point, pretty plot
if(!require(pROC)){
  install.packages("pROC")
}
library(pROC)
modelroc <- roc(test$y,pre)
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)
# AUC = 80%, good predictor! 
modelroc

# --- Weight of Evidence (WOE) transformation: from Logistic to Evaluation Card Model
## it's not for model quality, the performance is just like Logistic, but with fewer var to process
## WOE() = Ln((delay)/(total delay) / (normal)/(total normal))

# binning: Var 2-- Age
cutx2= c(-Inf,30,35,40,45,50,55,60,65,75,Inf)
plot(cut(train$x2,cutx2))

# Var 3-- NumberOfTime30-59DaysPastDueNotWorse
cutx3 = c(-Inf,0,1,3,5,Inf)
plot(cut(train$x3,cutx3))

# Var 5 -- MonthlyIncome
cutx5 = c(-Inf,1000,2000,3000,4000,5000,6000,7500,9500,12000,Inf)
plot(cut(train$x5,cutx5))

# Var 7 -- NumberOfTimes90DaysLate
cutx7 = c(-Inf,0,1,3,5,10,Inf)
plot(cut(train$x7,cutx7))

# Var 8 -- NumberRealEstateLoansOrLines
cutx8= c(-Inf,0,1,2,3,5,Inf)
plot(cut(train$x8,cutx8))
 
# Var 9 -- NumberOfTime60-89DaysPastDueNotWorse
cutx9 = c(-Inf,0,1,3,5,Inf)
plot(cut(train$x9,cutx9))

# Var 10 -- NumberOfDependents
cutx10 = c(-Inf,0,1,2,3,5,Inf)
plot(cut(train$x10,cutx10))

# Calculate WOE
totalgood = as.numeric(table(train$y))[1]
totalbad = as.numeric(table(train$y))[2]
getWOE <- function(a,p,q){
  Good <- as.numeric(table(train$y[a > p & a <= q]))[1]
  Bad <- as.numeric(table(train$y[a > p & a <= q]))[2]
  WOE <- log((Bad/totalbad)/(Good/totalgood),base = exp(1))
  return(WOE)
  }

# for Age ( Var 2)
Agelessthan30.WOE=getWOE(train$x2,-Inf,30)
Age30to35.WOE=getWOE(train$x2,30,35)
Age35to40.WOE=getWOE(train$x2,35,40)
Age40to45.WOE=getWOE(train$x2,40,45)
Age45to50.WOE=getWOE(train$x2,45,50)
Age50to55.WOE=getWOE(train$x2,50,55)
Age55to60.WOE=getWOE(train$x2,55,60)
Age60to65.WOE=getWOE(train$x2,60,65)
Age65to75.WOE=getWOE(train$x2,65,75)
Agemorethan.WOE=getWOE(train$x2,75,Inf)
age.WOE=c(Agelessthan30.WOE,Age30to35.WOE,Age35to40.WOE,Age40to45.WOE,Age45to50.WOE,
          Age50to55.WOE,Age55to60.WOE,Age60to65.WOE,Age65to75.WOE,Agemorethan.WOE)
age.WOE

# WOE transformation on Age (var 2)
tmp.age <- 0
for(i in 1:nrow(train)) {
  if(train$x2[i] <= 30)
    tmp.age[i] <- Agelessthan30.WOE
  else if(train$x2[i] <= 35)
    tmp.age[i] <- Age30to35.WOE
  else if(train$x2[i] <= 40)
    tmp.age[i] <- Age35to40.WOE
  else if(train$x2[i] <= 45)
    tmp.age[i] <- Age40to45.WOE
  else if(train$x2[i] <= 50)
    tmp.age[i] <- Age45to50.WOE
  else if(train$x2[i] <= 55)
    tmp.age[i] <- Age50to55.WOE
  else if(train$x2[i] <= 60)
    tmp.age[i] <- Age55to60.WOE
  else if(train$x2[i] <= 65)
    tmp.age[i] <- Age60to65.WOE
  else if(train$x2[i] <= 75)
    tmp.age[i] <- Age65to75.WOE
  else
    tmp.age[i] <- Agemorethan.WOE
}

table(tmp.age)

# the same for other vars
# Var 3-- NumberOfTime30-59DaysPastDueNotWorse
NOTPlessthan0.WOE=getWOE(train$x3,-Inf,0)
NOTP0to1.WOE=getWOE(train$x3,0,1)
NOTP1to3.WOE=getWOE(train$x3,1,3)
NOTP3to5.WOE=getWOE(train$x3,3,5)
NOTPmorethan5.WOE=getWOE(train$x3,5,Inf)

age.WOE=c(NOTPlessthan0.WOE, NOTP0to1.WOE, NOTP1to3.WOE, NOTP3to5.WOE, NOTPmorethan5.WOE)
age.WOE

#     
tmp.NOTP <- 0
for(i in 1:nrow(train)) {
  if(train$x3[i] <= 0)
    tmp.NOTP[i] <- NOTPlessthan0.WOE
  else if(train$x3[i] <= 1)
    tmp.NOTP[i] <- NOTP0to1.WOE
  else if(train$x3[i] <= 3)
    tmp.NOTP[i] <- NOTP1to3.WOE
  else if(train$x3[i] <= 5)
    tmp.NOTP[i] <- NOTP3to5.WOE
  else
    tmp.NOTP[i] <- NOTPmorethan5.WOE
 
}

table(tmp.NOTP)

# Var 5 --- MonthlyIncome -Inf,1000,2000,3000,4000,5000,6000,7500,9500,12000,Inf
incomelessthan1000.WOE=getWOE(train$x5,-Inf,1000)
income1000to2000.WOE=getWOE(train$x5,1000,2000)
income2000to3000.WOE=getWOE(train$x5,2000,3000)
income3000to4000.WOE=getWOE(train$x5,3000,4000)
income4000to5000.WOE=getWOE(train$x5,4000,5000)
income5000to6000.WOE=getWOE(train$x5,5000,6000)
income6000to7500.WOE=getWOE(train$x5,6000,7500)
income7500to9500.WOE=getWOE(train$x5,7500,9500)
income9500to12000.WOE=getWOE(train$x5,9500,12000)
incomemorethan12000.WOE=getWOE(train$x5,12000,Inf)

income.WOE=c(incomelessthan1000.WOE, income1000to2000.WOE, income2000to3000.WOE, income3000to4000.WOE, 
          income4000to5000.WOE, income5000to6000.WOE, income6000to7500.WOE, income7500to9500.WOE,
          income9500to12000.WOE, incomemorethan12000.WOE)
income.WOE

#     
tmp.income <- 0
for(i in 1:nrow(train)) {
  if(train$x5[i] <= 1000)
    tmp.income[i] <- incomelessthan1000.WOE
  else if(train$x5[i] <= 2000)
    tmp.income[i] <- income1000to2000.WOE
  else if(train$x5[i] <= 3000)
    tmp.income[i] <- income2000to3000.WOE
  else if(train$x5[i] <= 4000)
    tmp.income[i] <- income3000to4000.WOE
  else if(train$x5[i] <= 5000)
    tmp.income[i] <- income4000to5000.WOE
  else if(train$x5[i] <= 6000)
    tmp.income[i] <- income5000to6000.WOE
  else if(train$x5[i] <= 7500)
    tmp.income[i] <- income6000to7500.WOE
  else if(train$x5[i] <= 9500)
    tmp.income[i] <- income7500to9500.WOE
  else if(train$x5[i] <= 12000)
    tmp.income[i] <- income9500to12000.WOE
  else
    tmp.income[i] <- incomemorethan12000.WOE
  
}

table(tmp.income)


# Var 7 -- NumberOfTimes90DaysLate
# cutx7 = c(-Inf,0,1,3,5,10,Inf)
NOTLlessthan0.WOE=getWOE(train$x7,-Inf,0)
NOTL0to1.WOE=getWOE(train$x7,0,1)
NOTL1to3.WOE=getWOE(train$x7,1,3)
NOTL3to5.WOE=getWOE(train$x7,3,5)
NOTL5to10.WOE=getWOE(train$x7,5,10)
NOTLmorethan10.WOE=getWOE(train$x7,10,Inf)

NOTL.WOE=c(NOTLlessthan0.WOE, NOTL0to1.WOE, NOTL1to3.WOE, NOTL3to5.WOE, 
             NOTL5to10.WOE, NOTLmorethan10.WOE)
NOTL.WOE

#     
tmp.NOTL <- 0
for(i in 1:nrow(train)) {
  if(train$x7[i] <= 0)
    tmp.NOTL[i] <- NOTLlessthan0.WOE
  else if(train$x7[i] <= 1)
    tmp.NOTL[i] <- NOTL0to1.WOE
  else if(train$x7[i] <= 3)
    tmp.NOTL[i] <- NOTL1to3.WOE
  else if(train$x7[i] <= 5)
    tmp.NOTL[i] <- NOTL3to5.WOE
  else if(train$x7[i] <= 10)
    tmp.NOTL[i] <- NOTL5to10.WOE
  else
    tmp.NOTL[i] <- NOTLmorethan10.WOE
  
}

table(tmp.NOTL)


# Var 8 -- NumberRealEstateLoansOrLines
# cutx8= c(-Inf,0,1,2,3,5,Inf)
NRELlessthan0.WOE=getWOE(train$x8,-Inf,0)
NREL0to1.WOE=getWOE(train$x8,0,1)
NREL1to2.WOE=getWOE(train$x8,1,2)
NREL2to3.WOE=getWOE(train$x8,2,3)
NREL3to5.WOE=getWOE(train$x8,3,5)
NRELmorethan5.WOE=getWOE(train$x8,5,Inf)

NREL.WOE=c(NRELlessthan0.WOE, NREL0to1.WOE, NREL1to2.WOE, NREL2to3.WOE, 
           NREL3to5.WOE, NRELmorethan5.WOE)
NREL.WOE

#     
tmp.NREL <- 0
for(i in 1:nrow(train)) {
  if(train$x8[i] <= 0)
    tmp.NREL[i] <- NRELlessthan0.WOE
  else if(train$x8[i] <= 1)
    tmp.NREL[i] <- NREL0to1.WOE
  else if(train$x8[i] <= 2)
    tmp.NREL[i] <- NREL1to2.WOE
  else if(train$x8[i] <= 3)
    tmp.NREL[i] <- NREL2to3.WOE
  else
    tmp.NREL[i] <- NRELmorethan5.WOE
  
}

table(tmp.NREL)


# # Var 9 -- NumberOfTime60-89DaysPastDueNotWorse
# cutx9 = c(-Inf,0,1,3,5,Inf)
NOTDlessthan0.WOE=getWOE(train$x9,-Inf,0)
NOTD0to1.WOE=getWOE(train$x9,0,1)
NOTD1to3.WOE=getWOE(train$x9,1,3)
NOTD3to5.WOE=getWOE(train$x9,3,5)
NOTDmorethan5.WOE=getWOE(train$x9,5,Inf)

NOTD.WOE=c(NOTDlessthan0.WOE, NOTD0to1.WOE, NOTD1to3.WOE, NOTD3to5.WOE, 
           NOTDmorethan5.WOE)
NOTD.WOE

#     
tmp.NOTD <- 0
for(i in 1:nrow(train)) {
  if(train$x9[i] <= 0)
    tmp.NOTD[i] <- NOTDlessthan0.WOE
  else if(train$x9[i] <= 1)
    tmp.NOTD[i] <- NOTD0to1.WOE
  else if(train$x9[i] <= 3)
    tmp.NOTD[i] <- NOTD1to3.WOE
  else if(train$x9[i] <= 5)
    tmp.NOTD[i] <- NOTD3to5.WOE
  else
    tmp.NOTD[i] <- NOTDmorethan5.WOE
  
}

table(tmp.NOTD)

# Var 10 -- NumberOfDependents
# cutx10 = c(-Inf,0,1,2,3,5,Inf)
deplessthan0.WOE=getWOE(train$x10,-Inf,0)
dep0to1.WOE=getWOE(train$x10,0,1)
dep1to2.WOE=getWOE(train$x10,1,2)
dep2to3.WOE=getWOE(train$x10,2,3)
dep3to5.WOE=getWOE(train$x10,3,5)
depmorethan5.WOE=getWOE(train$x10,5,Inf)

dep.WOE=c(deplessthan0.WOE, dep0to1.WOE, dep1to3.WOE, dep3to5.WOE, 
          depmorethan5.WOE)
dep.WOE

# 

tmp.dep <- 0
for(i in 1:nrow(train)) {
  if(train$x10[i] <= 0)
    tmp.dep[i] <- deplessthan0.WOE
  else if(train$x10[i] <= 1)
    tmp.dep[i] <- dep0to1.WOE
  else if(train$x10[i] <= 2)
    tmp.dep[i] <- dep1to2.WOE
  else if(train$x10[i] <= 3)
    tmp.dep[i] <- dep2to3.WOE
  else if(train$x10[i] <= 5)
    tmp.dep[i] <- dep3to5.WOE
  else
    tmp.dep[i] <- depmorethan5.WOE
  
}

table(tmp.dep)

# and create the WOE Dataframe:
trainWOE =cbind.data.frame(tmp.age, tmp.NOTP,tmp.income, tmp.NOTL, tmp.NREL, tmp.NOTD, tmp.dep)





# ---- Create the Evaluation Card http://blog.csdn.net/csqazwsxedc/article/details/51225156
# SCORE = A - B * log(ratio) where ratio = p / (1-p)

trainWOE$y = train$y
glm.fit = glm(y~.,data = trainWOE,family = binomial(link = logit))
summary(glm.fit)
coe = (glm.fit$coefficients)
p <- 20/log(2)
q <- 600-20*log(15)/log(2)

# score rules
# Score=q + p*{as.numeric(coe[1])+as.numeric(coe[2])*tmp.age +as.numeric(coe[3])*
#     tmp.NumberOfTime30.59DaysPastDueNotWorse+p*as.numeric(coe[4])*
#     tmp.MonthlyIncome+p*as.numeric(coe[5])*tmp.NumberOfTime60.89DaysPastDueNotWorse+
#     p*as.numeric(coe[6])*tmp.NumberOfTimes90DaysLate+p*as.numeric(coe[7])*tmp.NumberRealEstateLoansOrLines+
#     p*as.numeric(coe[8])*tmp.NumberOfDependents

# base score
base <- q + p*as.numeric(coe[1])
base

# score different var
Agelessthan30.SCORE = p*as.numeric(coe[2])*Agelessthan30.WOE
Age30to35.SCORE = p*as.numeric(coe[2])*Age30to35.WOE
Age35to40.SCORE = p*as.numeric(coe[2])*Age35to40.WOE
Age40to45.SCORE = p*as.numeric(coe[2])*Age40to45.WOE
Age45to50.SCORE = p*as.numeric(coe[2])*Age45to50.WOE
Age50to55.SCORE = p*as.numeric(coe[2])*Age50to55.WOE
Age55to60.SCORE = p*as.numeric(coe[2])*Age55to60.WOE
Age60to65.SCORE = p*as.numeric(coe[2])*Age60to65.WOE
Age65to75.SCORE = p*as.numeric(coe[2])*Age65to75.WOE
Agemorethan.SCORE=p*as.numeric(coe[2])*Agemorethan.WOE
Age.SCORE =c(Age30to35.SCORE,Age35to40.SCORE,Age40to45.SCORE,Age45to50.SCORE,Age50to55.SCORE,Age55to60.SCORE,Age60to65.SCORE,Age65to75.SCORE,Agemorethan.SCORE)
Age.SCORE

getscore<-function(i,x){
  score = round(p*as.numeric(coe[i])*x,0)
  return(score)
}

Agelessthan30.SCORE = getscore(2,Agelessthan30.WOE)
Age30to35.SCORE = getscore(2,Age30to35.WOE)
Age35to40.SCORE = getscore(2,Age35to40.WOE)
Age40to45.SCORE = getscore(2,Age40to45.WOE)
Age45to50.SCORE = getscore(2,Age45to50.WOE)
Age50to55.SCORE = getscore(2,Age50to55.WOE)
Age55to60.SCORE = getscore(2,Age55to60.WOE)
Age60to65.SCORE = getscore(2,Age60to65.WOE)
Age65to75.SCORE = getscore(2,Age65to75.WOE)
Agemorethan.SCORE = getscore(2,Agemorethan.WOE)
Age.SCORE = c(Agelessthan30.SCORE,Age30to35.SCORE,Age35to40.SCORE,Age40to45.SCORE,Age45to50.SCORE,Age50to55.SCORE,Age55to60.SCORE,Age60to65.SCORE,Age65to75.SCORE,Agemorethan.SCORE)
Age.SCORE


