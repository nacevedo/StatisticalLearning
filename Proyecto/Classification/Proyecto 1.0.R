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


Train <- read_delim("Train.csv", ";", escape_double = FALSE,  trim_ws = TRUE)
Test <- read_delim("Test.csv", ";", escape_double = FALSE,  trim_ws = TRUE)

Train = as.data.frame(Train)
Test = as.data.frame(Test)

prep = rbind(Train,Test)
index = c(901:1065)

# Exploración de los datos
# http://www.rdatamining.com/docs/data-exploration-and-visualization-with-r

# number of rows
nrow(prep)

# number of columns
ncol(prep)

# dimensionality
dim(prep)

# column names
names(prep)

summary(prep)

# describe check all columns
describe(prep[, c(1, 19)])

# Organizar datos

names_dummies = c("Browser","TrafficType", "Region","VisitorType","Month", "OperatingSystems")

prep_fin = dummy.data.frame(prep, names = names_dummies, dummy.classes = getOption("dummy.classes"))
prep_fin["Weekend"][prep_fin["Weekend"]==TRUE] = 1

prep_fin = prep_fin[,-1]

test_fin = prep_fin[901:1065,]
train_fin = prep_fin[1:900,]

# Correlaciones para las variables no dummies
vc = cor(prep_fin[,1:10])
corrplot(vc,method="circle")

prep_xxs = scale(prep_fin, center=F) 

#DATA FINAL 

test_xxs = prep_fin[901:1065,]
train_xxs = prep_fin[1:900,]
test_xxs[test_xxs == "NaN"] = 0

#DATA PRUEBAS CALIBRAR #### ¡¡datos están escalados!! ### 

sample = sample.split(train_xxs$Revenue, SplitRatio = .75)
train_prueba = subset(train_xxs, sample == TRUE)
test_prueba  = subset(train_xxs, sample == FALSE)
dim(train_prueba)
dim(test_prueba)

########################### SVM ########################### 

rang = list(cost = c(250,275,300,325,330,350), gamma = c(0.0001,0.0005,0.0008,0.001))

#Tunning en dos parametros de calibracion
tune = tune(svm, Revenue~., data = train_prueba, ranges = rang)
tune

#Modelo Calibrado
svm_c = svm(Revenue~., data = train_prueba, cost = tune$best.parameters$cost, gamma = tune$best.parameters$gamma, probability=T, kernel="radial")

pred_svm = predict(svm_c, test_prueba[,-56], type = "response")
pred_svm_prob = predict(svm, test_prueba[,-56], type = "prob", probability = TRUE)

roc_svm = roc(test_prueba$Revenue, pred_svm)
plot(roc_svm)
auc_svm = auc(roc_svm)
auc_svm



pred_svm[pred_svm > 0] = 1
pred_svm[pred_svm <= 0] = 0

df = cbind(index,pred_svm)
colnames(df) = c("Id","Predicted")
  
write.csv(df,file="submission.csv", row.names = F)

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
