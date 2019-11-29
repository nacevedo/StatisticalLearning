# Punto 1
library(FNN)
library(randomForest)
library(pROC)

neighbor_boosting = function(lambda, B, pk, datos){
  
  datos = train_real
  weights = rep(0.5,nrow(datos))
  pred = rep(0,nrow(datos))
  
  weights_before = weights
  
  for (b in 1:B){
    
    train_index = sample.split(datos[,1], SplitRatio = .75)
    train = subset(datos, train_index == TRUE)
    weights_temp = weights #Hay que arreglar este para que tenga la dimensión de los datos
    
    neighbors = get.knn(train[,-1], k = pk)
    cuantos = 0
    for (i in 1:length(weights)){
      if (train_index[i]){
        cuantos = cuantos + 1
        indices_vecinos = neighbors$nn.index[cuantos,]
        y = train$default[indices_vecinos]
        w = weights_before[indices_vecinos]
        prediction = crossprod(y,w)/sum(w)
        weights_temp[i] = (train$default[i] - prediction)^2 #Esta predicción debe ser la de la iteración anterior, puede ser guardar los pesos de la iteración pasada
        weights[i] = weights[i] + lambda*(weights_temp[i] - weights[i])
      }
      else{
        #Acá se me olvidó que iba
      }
      
    }
    
    weights_before = weights
  }
  return (weights) 
}

### Datos Default
library(ISLR)
library(caTools)

datos = Default
datos$student = as.numeric(datos$student)
datos$default = as.numeric(datos$default)
datos$default[datos$default == 2] = 0

sample = sample.split(datos[,1], SplitRatio = .75)
train_real = subset(datos, sample == TRUE)
test_real  = subset(datos, sample == FALSE)

wei = neighbor_boosting(0.1,300,3,train_real)

neigh = get.knn(test_real[,-1], k = 2)

indices_vecinos = neigh$nn.index
y_test = test_real$default
y_train = train_real$default
w = wei[indices_vecinos]

prediction_real = vector()

for(i in 1:nrow(test_real)){
  prediction_real[i] = crossprod(y_train[indices_vecinos[i]],w[i])/sum(w[i])
}

prediction_real

sum(prediction_real != y_test)/length(y_test)
auc(y_test, prediction_real)

sum(prediction_real == 0)
sum(prediction_real == 1)

r = roc(prediction_real, y_test)
plot(r, main = 'ROC Boosting')
a = auc(r)
a


########################### Random Forest ########################### 

rand_forest = randomForest(default~., train_real, mtry = 2)

imp = importance(rand_forest)
imp
varImpPlot(rand_forest, main = "Importancia de variables en Random Forest")

pred_rf = predict(rand_forest, test_real[,-1])

pred_rf[pred_rf > 0.5] = 1
pred_rf[pred_rf <= 0.5] = 0

auc(test_real$default, pred_rf)

roc_rf = roc(test_real$default, as.numeric(pred_rf))
plot(roc_rf, main = 'ROC Random Forest')
auc_rf = auc(roc_rf)
auc_rf

