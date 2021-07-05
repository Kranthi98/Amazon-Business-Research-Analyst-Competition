train = read.csv("Train.csv")
test = read.csv("Test.csv")
head(train)
train = train[c(-1,-2,-6)]
train$BiasInfluentialFactor = ifelse(train$BiasInfluentialFactor == "","Other",train$BiasInfluentialFactor)
train$CTCratio = round(train$ExpectedCTC/train$CurrentCTC,2)
head(train)
train = train[c(-10,-11,-19)]
head(train)
str(train)
library(pysch)

clean_factors = function(x){
  
  x = as.factor(x)
  n = length(levels(x))-1
  y = 0:n
  levels(x) = y
  return(x)
  
}

train[c(1,3,4,5,6,10,12,13,14)] = apply(train[c(1,3,4,5,6,10,12,13,14)], MARGIN = 2, FUN = clean_factors)
str(train$LanguageOfCommunication)
str(train)
head(train)
class(train)
normalise = function(x){
  x = as.numeric(x)
  x = (x-min(x))/(max(x)-min(x))
  return(x)
}

dim(train)
train[-16] = apply(train[-16], MARGIN = 2, FUN = normalise)
describe(train)
hist(train$Age)
sum(is.na(train$BiasInfluentialFactor))

splits = sample.split(train$BiasInfluentialFactor,SplitRatio = 0.7)

train_dataset = train[splits == TRUE,]
test_dataset = train[splits == FALSE,]

train$BiasInfluentialFactor = as.factor(train$BiasInfluentialFactor) 
train_dataset$BiasInfluentialFactor = as.factor(train_dataset$BiasInfluentialFactor)
test_dataset$BiasInfluentialFactor = as.factor(test_dataset$BiasInfluentialFactor)
labels_train = train_dataset$BiasInfluentialFactor
knn(train_dataset,test_dataset,cl = labels_train, k = 10)
