
library(AppliedPredictiveModeling)
library(pls)
library(MASS)
library(caret)
library(lars)
library(elasticnet)

data(solubility)
set.seed(2)
sample(names(solTrainX),8)

trainingData<-solTrainXtrans

trainingData$Solubility<-solTrainY

###use pls package, plser function
###default algorithm is Dayal and Mcgregor kernel algorithm
plsFit<-plsr(Solubility~.,data=trainingData,validation="CV")

###predict first five solubilty values using 1 and 2 components
pls.pred<-predict(plsFit,solTestXtrans[1:5,],ncomp=1:2)

summary(plsFit)
validationplot(plsFit,val.type ="RMSEP")
pls.RMSEP<-RMSEP(plsFit,estimate="CV")
plot(pls.RMSEP,main="RMSEP PLS solubility",xlab="Components")
min<-which.min(pls.RMSEP$val)
points(min,min(pls.RMSEP$val),pch=1,col="red")

plot(plsFit, ncomp=10, asp=1, line=True)


###use lowest RMSEP at 10 components
pls.pred2<-predict(plsFit,solTestXtrans,ncomp=10)
plot(solTestY,pls.pred2,ylim=c(-11,2),xlim=c(-11,2),main="observed and predicted solubilites",xlab="Observed",ylab="PLS predicted")
abline(0,1,col="red")

pls.eval<-data.frame(obs=solTestY,pred=pls.pred2[,1,1])
defaultSummary(pls.eval)

