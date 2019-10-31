# Punto 8

library(randomForest)
library(tree)
library(pROC)
library(gbm)
library(caret)
library(e1071)
library(C50)
data(churn)

train = churnTrain
test = churnTest

car = train
High.test = test$churn

########################### Bagging ########################### 

bag.car = randomForest(churn~., car, mtry = (dim(car)[2]-2))

imp = importance(bag.car)
imp
varImpPlot(bag.car)

pred_bag = predict(bag.car, test, type="prob")

roc_bag=roc(High.test,pred_bag[,2])
plot(roc_bag)
auc_bag=auc(roc_bag)
auc_bag

########################### Boosting ########################### 

high = ifelse(car$churn=="yes", 1, 0)

car$churn = high[1:3333]

boost.car = gbm(churn~.,data = car, distribution="bernoulli",
                n.trees=1000, interaction.depth = 3, shrinkage=0.1)
summary(boost.car)

gbm.perf(boost.car, oobag.curve=T, overlay=T)

plot(boost.car,i="total_day_minutes")
plot(boost.car,i="number_customer_service_calls")
plot(boost.car,i="total_eve_minutes")

pred_boo = predict(boost.car, test, type="response", n.trees=500)

roc_boo = roc(High.test, pred_boo, n.trees=200)
plot(roc_boo)
auc_boo=auc(roc_boo)
auc_boo

###### printing all auc ######

auc_bag
auc_boo

########################### SVM ########################### 

rang = list(cost = c(0.01,0.05,0.1,1,2,5), gamma = c(0.1,0.5,1,2,4))

#Tunning en dos parametros de calibracion
tune_churn = tune(svm, churn~., data = train, ranges = rang)

tune_churn


#Modelo Calibrado
svm_churn = svm(churn~., data = train, cost=2, gamma=.5, probability=T, kernel="radial")

pred_svm = predict(svm_churn, test, type = "response")

roc_svm = roc(High.test, pred_svm)
plot(roc_svm)
auc_svm = auc(roc_svm)
auc_svm
