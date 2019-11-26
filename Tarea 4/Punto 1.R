# Punto 1
library(FNN)

neighbor_boosting = function(lambda, B, k, datos, y){
  
  weights = rep(0,nrow(datos))
  pred = rep(0,nrow(datos))
  for (b in 1:B){
    train_index = sample(nrow(datos),0.75*nrow(datos),replace = FALSE)
    train = datos[train_index,]
    neighbors = get.knn(train, k = 5)
    for (i in 1:nrow(datos)){
      
    }
      
      
  }
}

### Datos Default
library(ISLR)
  
datos = Default
datos$student = as.numeric(datos$student)
