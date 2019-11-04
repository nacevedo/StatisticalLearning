# Punto 8

library(randomForest)
library(tree)
library(pROC)
library(gbm)
library(caret)
library(e1071)
library(mlr)
library(C50)
data(churn)

train = churnTrain
test = churnTest

#train$churn = as.numeric(train$churn)
#test$churn = as.numeric(test$churn)

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
train = churnTrain
test = churnTest

train$churn = as.numeric(train$churn)
test$churn = as.numeric(test$churn)

car = train
High.test = test$churn

rang = list(cost = c(3,4,5,6), gamma = c(0.01,0.02,0.05,0.1))

#Tunning en dos parametros de calibracion
tune_churn = tune(svm, churn~., data = train, ranges = rang)

tune_churn


#Modelo Calibrado
svm_churn = svm(churn~., data = train, cost = tune_churn$best.parameters$cost, gamma = tune_churn$best.parameters$gamma, probability=T, kernel="radial")

pred_svm = predict(svm_churn, test, type = "response")
pred_svm_prob = predict(svm_churn, test, type = "prob", probability = TRUE)



roc_svm = roc(High.test, pred_svm)
plot(roc_svm)
auc_svm = auc(roc_svm)
auc_svm

###### printing all auc ######

auc_bag
auc_boo
auc_svm

