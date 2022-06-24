##Machine learning##

#install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data(AlzheimerDisease)





data(concrete)
#install.packages("Rcpp")
#install.packages("caret")
#install.packages("Hmisc package")
library(Hmisc)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

cutStrength<-cut2(training$CompressiveStrength,g=3)
table(cutStrength)

p2<-qplot(cutStrength,Age,data=training,fill=cutStrength,geom=c("boxplot"))
p2

p3<-qplot(cutStrength,FlyAsh,data=training,fill=cutStrength,geom=c("boxplot"))
p3

set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

modelFit<-train(type~.,data=training,preProcess=c("center","scale"),method="glm")

#QUiz3
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

install.packages("ElemStatLearn")
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

#quize#
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(3523)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
