# Punto 2

library(readr)
library(randomForest)
library(caTools)
library(pROC)
library(gbm)
library(xgboost)

data <- read_csv("data_banknote_authentication.txt", col_names = FALSE)

sample = sample.split(data$X5, SplitRatio = .745)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)

########################### Random Forest ########################### 

rand_forest = randomForest(X5~., train, mtry = 2)

imp = importance(rand_forest)
imp
varImpPlot(rand_forest)

pred_rf = predict(rand_forest, test[,-5])

roc_rf = roc(test$X5, pred_rf)
plot(roc_rf)
auc_rf = auc(roc_rf)
auc_rf

########################### Boosting ########################### 

boost = gbm(X5~.,data = train, distribution="bernoulli",
                n.trees=1000, interaction.depth = 3, shrinkage=0.1)
summary(boost)

gbm.perf(boost, oobag.curve=T, overlay=T)

plot(boost,i="X1")
plot(boost,i="X2")
plot(boost,i="X3")
plot(boost,i="X4")

pred_boo = predict(boost, test[,-5], type="response", n.trees=500)

roc_boo = roc(test$X5, pred_boo, n.trees=200)
plot(roc_boo)
auc_boo=auc(roc_boo)
auc_boo

########################### xgboost ########################### 

bstSparse <- xgboost(data = as.matrix(train[,-5]), label = train$X5, nthread = 2, nrounds = 100, objective = "binary:logistic", verbose =0)

pred_xg = predict(bstSparse, as.matrix(test[,-5]))

roc_xg = roc(test$X5, pred_xg)
plot(roc_xg)
auc_xg=auc(roc_xg)
auc_xg

