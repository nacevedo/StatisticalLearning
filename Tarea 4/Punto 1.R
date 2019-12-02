# Punto 1
library(FNN)
library(randomForest)
library(pROC)

neighbor_boosting = function(lambda, B, pk, datos){
  
  datos = train_real
  weights = rep(0.5,nrow(datos))
  pred = rep(0,nrow(datos))
  weights_temp = rep(0.5,nrow(datos))
  
  weights_before = weights
  
  for (b in 1:B){
    
    train_index = sample.split(datos[,1], SplitRatio = .75)
    train = subset(datos, train_index == TRUE)
    weights_temp = weights
    
    neighbors = get.knn(train[,-1], k = pk)
    cuantos = 0
    for (i in 1:length(weights)){
      if (train_index[i]){
        cuantos = cuantos + 1
        indices_vecinos = neighbors$nn.index[cuantos,]
        y = datos$default[indices_vecinos]
        w = weights_before[indices_vecinos]
       
        prediction = crossprod(y,w)/sum(w)
        
        pred[i] = prediction
        weights_temp[i] = (datos$default[i] - prediction)^2 #Esta predicción debe ser la de la iteración anterior, puede ser guardar los pesos de la iteración pasada
        
        weights[i] = weights[i] + lambda*(weights_temp[i] - weights[i])
      }
      else{
        #Acá se me olvidó que iba
      }
      
    }
    
    weights_before = weights
  }
  return(cbind(weights, pred))
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

auc_comparar = vector()

for(j in 1:60){
  respuesta = neighbor_boosting(0.1,50,j,train_real)
  
  neigh = get.knnx(train_real[,-1], test_real[,-1], k = j)
  
  indices_vecinos = neigh$nn.index
  y_test = test_real$default
  y_train = train_real$default
  
  w = respuesta[,1]
  
  prediction_real = vector()
  
  for(i in 1:nrow(test_real)){
    prediction_real[i] = crossprod(y_train[indices_vecinos[i,]],w[indices_vecinos[i,]])/sum(w[indices_vecinos[i,]]) #!!!
  }
  
  prediction_real

  auc_comparar[j] = auc(y_test, prediction_real)
  
}

plot(auc_comparar, type = 'l', xlab = 'Vecinos', ylab = 'AUC', main = 'AUC al variar los vecinos')
points(auc_comparar)
k_calibrado = which.max(auc_comparar)
k_calibrado


auc_comparar_lambda = vector()

l = seq(0.001,1,0.005)

for(j in 1:length(l)){
  
  respuesta = neighbor_boosting(l[j],50,k_calibrado,train_real)
  
  neigh = get.knnx(train_real[,-1], test_real[,-1], k = k_calibrado)
  
  indices_vecinos = neigh$nn.index
  y_test = test_real$default
  y_train = train_real$default
  
  w = respuesta[,1]
  
  prediction_real = vector()
  
  for(i in 1:nrow(test_real)){
    prediction_real[i] = crossprod(y_train[indices_vecinos[i,]],w[indices_vecinos[i,]])/sum(w[indices_vecinos[i,]]) #!!!
  }
  
  auc_comparar_lambda[j] = auc(y_test, prediction_real)
  
}

plot(l,auc_comparar_lambda, type = 'l', xlab = 'Lambda', ylab = 'AUC', main = 'AUC al variar lambda')
points(l, auc_comparar_lambda)
lambda_calibrado = l[which.max(auc_comparar_lambda)]
lambda_calibrado

auc_comparar_B = vector()

for(j in 1:200){
  
  respuesta = neighbor_boosting(lambda_calibrado,j,k_calibrado,train_real)
  
  neigh = get.knnx(train_real[,-1], test_real[,-1], k = k_calibrado)
  
  indices_vecinos = neigh$nn.index
  y_test = test_real$default
  y_train = train_real$default
  
  w = respuesta[,1]
  
  prediction_real = vector()
  
  for(i in 1:nrow(test_real)){
    prediction_real[i] = crossprod(y_train[indices_vecinos[i,]],w[indices_vecinos[i,]])/sum(w[indices_vecinos[i,]]) #!!!
  }
  
  auc_comparar_B[j] = auc(y_test, prediction_real)
  
}

plot(auc_comparar_B, type = 'l', xlab = 'Iteraciones', ylab = 'AUC', main = 'AUC al variar las iteraciones del boosting')
points(auc_comparar_B)
B_calibrado = which.max(auc_comparar_B)
B_calibrado

###### real

respuesta = neighbor_boosting(lambda_calibrado,B_calibrado,k_calibrado,train_real)

neigh = get.knnx(train_real[,-1], test_real[,-1], k = k_calibrado)

indices_vecinos = neigh$nn.index
y_test = test_real$default
y_train = train_real$default

w = respuesta[,1]

prediction_real = vector()

for(i in 1:nrow(test_real)){
  prediction_real[i] = crossprod(y_train[indices_vecinos[i,]],w[indices_vecinos[i,]])/sum(w[indices_vecinos[i,]]) #!!!
}

prediction_real

sum(prediction_real != y_test)/length(y_test)
auc(y_test, prediction_real)

r = roc(y_test, prediction_real)
plot(r, main = 'ROC Boosting')
a = auc(r)
a


########################### Random Forest ########################### 

rand_forest = randomForest(default~., train_real, mtry = 2)

imp = importance(rand_forest)
imp
varImpPlot(rand_forest, main = "Importancia de variables en Random Forest")

pred_rf = predict(rand_forest, test_real[,-1])

auc(test_real$default, pred_rf)

roc_rf = roc(test_real$default, as.numeric(pred_rf))
plot(roc_rf, main = 'ROC Random Forest')
auc_rf = auc(roc_rf)
auc_rf

