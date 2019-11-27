# Punto 2

library(readr)
library(randomForest)
library(caTools)
library(pROC)
library(gbm)
library(xgboost)

data <- read_csv("data_banknote_authentication.txt", col_names = FALSE)
colnames(data) = c("Varianza", "Skewness", "Curtosis", "Entropía", "Clase")
data$Clase = as.factor(data$Clase)

sample = sample.split(data$Clase, SplitRatio = .745)

train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)

########################### Random Forest ########################### 

rand_forest = randomForest(Clase~., train, mtry = 2)

imp = importance(rand_forest)
imp
varImpPlot(rand_forest, main = "Importancia de variables en Random Forest")

pred_rf = predict(rand_forest, test[,-5])

roc_rf = roc(test$Clase, as.numeric(pred_rf))
plot(roc_rf, main = 'ROC Random Forest')
auc_rf = auc(roc_rf)
auc_rf

########################### Boosting ########################### 
data$Clase = as.numeric(data$Clase)

boost = gbm(Clase~.,data = train, distribution="bernoulli",
                n.trees=1000, interaction.depth = 3, shrinkage=0.1)
summary(boost)

gbm.perf(boost, oobag.curve=T, overlay=T)


pred_boo = predict(boost, test[,-5], type="response", n.trees=500)

roc_boo = roc(test$Clase, pred_boo, n.trees=200)
plot(roc_boo, main = "ROC Boosting")
auc_boo=auc(roc_boo)
auc_boo

########################### xgboost ########################### 

bstSparse <- xgboost(data = as.matrix(train[,-5]), label = train$Clase, nthread = 2, nrounds = 100, objective = "binary:logistic", verbose =0)

pred_xg = predict(bstSparse, as.matrix(test[,-5]))

roc_xg = roc(test$Clase, pred_xg)
plot(roc_xg, main = "ROC Gradient Boosting")
auc_xg=auc(roc_xg)
auc_xg


########################### Stacking ########################### 
# bstSparse, boost, rand_forest
#Predicting using random forest model
testSet = data.frame(pred_rf = as.numeric(pred_rf), pred_boo = as.numeric(pred_boo), pred_xg = as.numeric(pred_xg))
testSet$pred_avg<-(testSet$pred_rf+testSet$pred_boo+testSet$pred_xg)/3
roc_stacking = roc(test$Clase, testSet$pred_avg)
plot(roc_stacking, main = "ROC Stacking")
auc_stacking=auc(roc_stacking)
auc_stacking
