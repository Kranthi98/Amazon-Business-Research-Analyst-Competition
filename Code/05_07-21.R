##importing required packages....
library(tidyverse)
library(class)
library(caTools)
library(psych)
library(gmodels)
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


##reading the data from the working directory
train = read.csv("Train.csv")
test = read.csv("Test.csv")
head(train)


##feature engineering
train = train[c(-1,-2,-6)]
train$CTCratio = round(train$ExpectedCTC/train$CurrentCTC,2)
head(train)
train = train[c(-10,-11,-19)]
##deleting the one of the dependent variable since our present model is to predict the bias infuential factor


##replacing the null values in the dependent variable
sum(is.na(train$BiasInfluentialFactor))
train$BiasInfluentialFactor = ifelse(train$BiasInfluentialFactor == "","Other",train$BiasInfluentialFactor)
sum(is.na(train$BiasInfluentialFactor))


##data preparation for modelling
train[c(1,3,4,5,6,10,12,13,14)] = apply(train[c(1,3,4,5,6,10,12,13,14)], MARGIN = 2, FUN = clean_factors)
str(train)

##storing the levels of dependent variable
target_levels = levels(train_dataset$BiasInfluentialFactor)

##normalising the data
dim(train)
train[-16] = apply(train[-16], MARGIN = 2, FUN = standardise)
describe(train)
hist(train$Age)


##splitting the data into train and test
splits = sample.split(train$BiasInfluentialFactor,SplitRatio = 0.7)
train_dataset = train[splits == TRUE,]
test_dataset = train[splits == FALSE,]


##preparing the data to get fed into model
labels_train = train_dataset$BiasInfluentialFactor
labels_test = test_dataset$BiasInfluentialFactor
train_dataset = train_dataset[,c(1:15,17)]
test_dataset = test_dataset[,c(1:15,17)]
str(train_dataset)
str(test_dataset)

##modelling
k = 1:30
knn_list <- lapply(20:50, function(l){
  knn(train_dataset, test_dataset, cl = labels_train, k = l)
})

ok <- sapply(knn_list, '==', labels_test)
acc <- colMeans(ok)

which(acc == max(acc))
plot(acc, type = "b")



