
library(AppliedPredictiveModeling)
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

###fit all predictors in linear model
lmFitAllPredictors<-lm(Solubility~.,data=trainingData)

summary(lmFitAllPredictors)
plot(Solubility~.,data=trainingData)
abline(lm(Solubility~FP020,data=trainingData),col="red")
par(mfrow=c(2,2))
plot(lmFitAllPredictors)

###predict values 
lmPred1<-predict(lmFitAllPredictors,solTestXtrans)
head(lmPred1)
###create observed and predicted values data frame
lmvalues1<-data.frame(obs=solTestY,pred=lmPred1)
library(caret)
defaultSummary(lmvalues1)
###resampling
ctrl<-trainControl(method="cv",n=10)
set.seed(100)
lmFit1<-train(x=solTrainXtrans,y=solTrainY,method="lm",trControl =ctrl)
lmFit1
###plot diagnosis
xyplot(solTrainY~predict(lmFit1),type=c("p","g"),xlab="predicted",ylab="observed")
xyplot(resid(lmFit1)~predict(lmFit1),type=c("p","g"),xlab="predicted",ylab="residual")