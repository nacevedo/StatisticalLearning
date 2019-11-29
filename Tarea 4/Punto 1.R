# Punto 1
library(FNN)

neighbor_boosting = function(lambda, B, pk, datos, y){
  
  datos = train_real
  weights = rep(0,nrow(datos))
  pred = rep(0,nrow(datos))
  
  for (b in 1:B){
    
    train_index = sample.split(datos[,1], SplitRatio = .75)
    train = subset(datos, train_index == TRUE)
    weights_temp = subset(weights, train_index == TRUE)
    
    neighbors = get.knn(train, k = 5)
    
    for (i in 1:nrow(weights)){
      if (train_index[i]){
        weights_temp[i] = formula
      }
      else{
        
      }
    }
      
      
  }
}

### Datos Default
library(ISLR)
library(caTools)

datos = Default
datos$student = as.numeric(datos$student)
datos$default = as.numeric(datos$default)

sample = sample.split(datos[,1], SplitRatio = .75)
train_real = subset(datos, sample == TRUE)
test_real  = subset(datos, sample == FALSE)
