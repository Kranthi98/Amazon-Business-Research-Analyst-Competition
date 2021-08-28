##importing required packages....
library(tidyverse)
library(class)
library(caTools)
library(psych)
library(gmodels)
library(e1071)
library(randomForest)

install.load::install_load("gmodel")


##functions
normalise = function(x){
  x = as.numeric(x)
  x = (x-min(x))/(max(x)-min(x))
  return(x)
}
standardise = function(x){
  x = as.numeric(x)
  x = (x-mean(x))/sd(x)
  return(x)
}
clean_factors = function(x){
  
  x = as.factor(x)
  n = length(levels(x))-1
  y = 0:n
  levels(x) = y
  return(x)
  
}
accu = function(x,y){
  
  table_results = table(x,y)
  summ = sum(diag(table_results))
  accuu = summ/sum(table_results)
  return(round(accuu,2))
  
}

head(train)
##reading the data from the working directory
train = read.csv(choose.files(caption = "Choose the training dataset"))
test = read.csv(choose.files(caption = "Choose the testing dataset"))
head(train)

boxplot(train$Age)
boxplot(train$YearsOfExperince)
boxplot(train$CurrentCTC)
boxplot(train$ExpectedCTC)
boxplot(train$TotalLeavesTaken)
sum(train$BiasInfluentialFactor == "")
dim(train)


train$split = ifelse(train$BiasInfluentialFactor == "",1,0)

train = train %>% filter(train$split == 0)

head(train)


##feature engineering
train = train[c(-1,-2,-6)]
train$CTCratio = round(train$ExpectedCTC/train$CurrentCTC,2)
head(train)
train = train[c(-10,-11,-20)]
train$age1 = ifelse(train$Age >= 24 & train$Age < 27,1,
                    ifelse(train$Age >= 27 & train$Age < 30,2,
                           ifelse(train$Age >=30 & train$Age < 33,3,
                                  ifelse(train$Age >=33 & train$Age < 36,4,
                                         ifelse(train$Age >= 36 & train$Age < 39,5,6)))))

unique(train$age1)

##deleting the one of the dependent variable since our present model is to predict the bias infuential factor


##data preparation for modelling
train[c(1,3,4,5,6,10,12,13,14)] = apply(train[c(1,3,4,5,6,10,12,13,14)], MARGIN = 2, FUN = clean_factors)
str(train)

##storing the levels of dependent variable
target_levels = levels(train$BiasInfluentialFactor)


describe(train$Age)
train$Age = as.numeric(train$Age)

train$age1 = ifelse(train$Age >= 24 & train$Age < 27,1,
                    ifelse(train$Age >= 27 & train$Age < 30,2,
                           ifelse(train$Age >=30 & train$Age < 33,3,
                                  ifelse(train$Age >=33 & train$Age < 36,4,
                                         ifelse(train$Age >= 36 & train$Age < 39,5,6)))))

unique(train$age1)

train$YOE1 = ifelse(train$YearsOfExperince >= 1 & train$YearsOfExperince < 4,1,
                    ifelse(train$YearsOfExperince >= 4 & train$YearsOfExperince < 7,2,
                           ifelse(train$YearsOfExperince >= 7 &train$YearsOfExperince < 10,3,4)))

unique(train$YOE1)

train$CTC1 = (train$CTCratio*100)-100
describe(train$CTC1)

train$CTC2 = ifelse(train$CTC1 >= 10 & train$CTC1 < 27,1,
                    ifelse(train$CTC1 >= 28 & train$CTC1 < 45,2,
                           ifelse(train$CTC1 >= 45 & train$CTC1 < 62,3,
                                  ifelse(train$CTC1 >= 62 & train$CTC1 <79,4,
                                         ifelse(train$CTC1 >= 79 & train$CTC1 < 96,5,
                                                ifelse(train$CTC1 >= 96 & train$CTC1 < 113,6,
                                                       ifelse(train$CTC1 >= 113 & train$CTC1 < 130,7,
                                                              ifelse(train$CTC1 >= 130 & train$CTC1 < 147,8,
                                                                     ifelse(train$CTC1 >= 147 & train$CTC1 < 164,9,10)))))))))


unique(train$CTC2)


describe(train$YearsOfExperince)
head(train)

train_nb = train[-c(2,8,15,17,18,21)]
train_nb_classes = train_nb$BiasInfluentialFactor
head(train_nb)


splits = sample.split(train_nb_classes,SplitRatio = 0.7)
train_nb1 = train_nb[splits == TRUE,]
test_nb1 = train_nb[splits == FALSE,]
head(train_nb1)
head(test_nb1)
test_nb1_classes = test_nb1$BiasInfluentialFactor
train_nb1_classes = train_nb1$BiasInfluentialFactor

test_nb1 = test_nb1[-11]
train_nb1 = train_nb1[-11]


str(train_nb1)
train_nb1 = lapply(train_nb1, as.factor)




train_Jrip = as.data.frame(cbind(train_nb1,train_nb1_classes))
head(train_Jrip)

train_Jrip = lapply(train_Jrip, as.factor)


Model_Jrip = RWeka::JRip(train_Jrip$train_nb1_classes~.,data = train_nb1)

dim(train_Jrip)


train_Jrip = as.data.frame(train_Jrip)

test_nb1 = lapply(test_nb1, as.factor)
test_nb1 = as.data.frame(test_nb1)
head(test_nb1)

colnames(test_nb1) = c("v1","v2","v3","v4","v5","v6","v7","v8","v9","v10")

weka.filters.unsupervised.attribute.RemoveType(string)


predictions = predict(Model_Jrip,test_nb1)
length(test_nb1_classes)

sum(diag(table(predictions,test_nb1_classes)))/sum(table(predictions,test_nb1_classes))

accu(predictions,test_nb1_classes)
str(train_Jrip)
summary(NB_Models)

head(train)
