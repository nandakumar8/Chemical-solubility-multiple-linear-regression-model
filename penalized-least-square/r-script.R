library(AppliedPredictiveModeling)
library(MASS)
library(elasticnet)
library(caret)
library(car)
data(solubility)
set.seed(2)
sample(names(solTrainX),8)
###solTrainX,solTestX data frames contain trainig and test predictors.Each data frame has number
###columns of data such fingerprint predictor values.solTrainxtrans ans solTestXtrans contain 
###transformed data frmae using box-cox.solTrainY and solTestY contain the solubility of each compound

###create data frame to include predictor and outcome in one data frame

trainingData<-solTrainXtrans
###add solTrainY
trainingData$Solubility<-solTrainY


###ridge regression
ridge<-lm.ridge(Solubility~.,data=trainingData,lambda =seq(0,100,by=1))
plot(ridge)
defaultsummary(ridge)
###to check the estimates
head(ridge)
head(ridge$coef)