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

test_fin = prep_fin[901:1065,]
train_fin = prep_fin[1:900,]

# Correlaciones para las variables no dummies
vc = cor(prep_fin[,1:10])
corrplot(vc,method="circle")

prep_xxs = scale(prep_fin, center=F) 

#DATA 

test_xxs = prep_fin[901:1065,]
train_xxs = prep_fin[1:900,]
test_xxs[test_xxs == "NaN"] = 0

########################### SVM ########################### 

rang = list(cost = c(250,275,300,325,330,350), gamma = c(0.0001,0.0005,0.0008,0.001))

#Tunning en dos parametros de calibracion
tune = tune(svm, Revenue~., data = train_xxs[,c(-1)], ranges = rang)
tune

#Modelo Calibrado
svm = svm(Revenue~., data = train_xxs[,-1], cost = tune$best.parameters$cost, gamma = tune$best.parameters$gamma, probability=T, kernel="radial")

pred_svm = predict(svm, test_xxs[,c(-1,-56)], type = "response")
pred_svm_prob = predict(svm, test_xxs[,c(-1,-56)], type = "prob", probability = TRUE)

pred_svm[pred_svm > 0] = 1
pred_svm[pred_svm <= 0] = 0

df = cbind(test_fin[,1],pred_svm)
colnames(df) = c("Id","Predicted")
  
write.csv(df,file="submission.csv", row.names = F)

