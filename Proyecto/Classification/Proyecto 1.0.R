library(readr)
library(Hmisc)
library(dummies)
library(corrplot)
library(cluster)

library(randomForest)
library(tree)
library(pROC)
library(gbm)
library(caret)
library(e1071)
library(mlr)
library(C50)

########################### SVM ########################### 
library(caTools)
# index_train = sample.split(Train_final$Revenue,SplitRatio = 0.75)
# train = subset(Train_final,index_train==TRUE)
# test = subset(Train_final,index_train==FALSE)




rang = list(cost = c(175,200,225,250,250,275,300,325,330,350), gamma = c(0.0001,0.0005,0.0008,0.001,0.0015,0.002))
rang = list(cost = c(175,200,225,250,250,275,300,325,330,350,375,400), gamma = c(0.001,0.0015,0.002,0.003,0.004,0.005,0.006,0.007, 0.008, 0.009))


#Tunning en dos parametros de calibracion
tune = tune(svm, Revenue~., data = Train_final, ranges = rang)
tune
svm_c = svm(Revenue~., data = Train_final, cost = tune$best.parameters$cost, gamma = tune$best.parameters$gamma, probability=T, kernel="radial")

pred_svm = predict(svm_c, Test[,-18], type = "prob", probability = TRUE)
prob = predict(svm_c, Test[,-18])


prediccion = as.numeric(prob)
prediccion[prediccion == 1] = 0
prediccion[prediccion == 2] = 1


index = c(901:1065)
df = cbind(index,prediccion)
colnames(df) = c("Id","Predicted")

write.csv(df,file="submission1.csv", row.names = F)

########################### LDA ########################### 
library(klaR)

#forward
lda_for = stepclass(Revenue~.,data = train_xxs[,-1], method="lda", direction="forward", improvement=0.001)
lda_for
summary(lda_for)
lda_for$formula

lda_b = lda(Revenue ~ ProductRelated_Duration + PageValues + MonthFeb + TrafficType8, data = train_xxs[,-1])
lda_b #Ws

predl_b = predict(lda_b,newdata=test_xxs[,c(-1,-56)])$class

df = cbind(index,predl_b)
colnames(df) = c("Id","Predicted")

write.csv(df,file="submission2.csv", row.names = F)

########################### Random Forest ########################### 
library(randomForest)

rand_forest = randomForest(Revenue~., train_xxs[,-1], mtry = 8)

imp = importance(rand_forest)
imp
varImpPlot(rand_forest)

pred_rf = predict(rand_forest, test_xxs[,c(-1,-56)])

pred_rf[pred_rf > 0] = 1
pred_rf[pred_rf <= 0] = 0

df = cbind(index,pred_rf)
colnames(df) = c("Id","Predicted")

write.csv(df,file="submission3.csv", row.names = F)

########################### xgboost ########################### 
library(xgboost)

bstSparse <- xgboost(data = as.matrix(train_xxs[,-1]), label = train_xxs$Revenue, nthread = 2, nrounds = 10, objective = "binary:logistic", verbose =0)

pred_xg = predict(bstSparse, as.matrix(test_xxs[,c(-1)]))

pred_xg[pred_xg > 0] = 1
pred_xg[pred_xg <= 0] = 0

df = cbind(index,pred_xg)
colnames(df) = c("Id","Predicted")

#write.csv(df,file="submission4.csv", row.names = F)

########################### Boosting ########################### va ganandooooo 

boost = gbm(Revenue~.,data = train_xxs[,-1], distribution="bernoulli",
            n.trees=1000, interaction.depth = 3, shrinkage=0.1)
summary(boost)

gbm.perf(boost, oobag.curve=T, overlay=T)

plot(boost,i="PageValues")

pred_boo = predict(boost, test_xxs[,c(-1,-56)], type="link", n.trees=500)

pred_boo[pred_boo > 0] = 1
pred_boo[pred_boo <= 0] = 0

df = cbind(index,pred_boo)
colnames(df) = c("Id","Predicted")

#write.csv(df,file="submission5.csv", row.names = F)
