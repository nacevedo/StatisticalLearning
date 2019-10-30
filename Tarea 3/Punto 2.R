# Punto 2

# Funcion matriz de confusion
confusion = function(actuales,p,threshold){
  actuales = replace(actuales,actuales == -1, 0)
  
  predecidos = ifelse(p>threshold,1,0)
  TP = sum(predecidos*actuales)
  FPv = vector(length = length(actuales))
  for (i in 1:length(predecidos)){
    if(predecidos[i] == 1 & actuales[i] == 0){
      FPv[i] = 1
    }
  }
  FP = sum(FPv)
  
  FNv = vector(length = length(actuales))
  for (i in 1:length(predecidos)){
    if(predecidos[i] == 0 & actuales[i] == 1){
      FNv[i] = 1
    }
  }
  FN = sum(FNv)
  
  TNs = predecidos+actuales
  TN = sum(TNs == 0)
  
  arriba = paste(TP, FP, sep='\t')
  abajo = paste(FN, TN, sep='\t')

  #cat("\t","\t",'  Real', "\n","\t","\t"," 1", "\t","0", "\n","\t","1","\t",arriba, "\n","Pred","\n","\t","0","\t",abajo, "\n","\n","\n")
  
  
  return(c(TP, FP, FN, TN))
  


}

# Curva ROC
ROC = function(actuales, p){
  threshold = seq(1,0,-0.0001)
  sensibilidad = vector(length = length(threshold))
  esp = vector(length = length(threshold))
  
  for (i in 1:length(threshold)){
    valores = confusion(actuales, p, threshold[i])
    TP = valores[1]
    FP = valores[2]
    FN = valores[3]
    TN = valores[4]
    
    sensibilidad[i] = TP/(TP+FN)
    esp[i] = TN/(TN+FP)
  }
  plot(1-esp,sensibilidad, type = 'l', ylab = 'Sensibilidad', xlab = '1 - Especificidad', main = 'Curva ROC')
  abline(c(0,0),c(1,1), col = 2)
  return(matrix(c(sensibilidad,esp),ncol =2))
  
}


# Area bajo curva
AUCnuestro = function(actuales, p){
  matriz = ROC(actuales,p)
  sensibilidad = matriz[,1]
  esp = matriz[,2]
  AUC = sum((sensibilidad)*diff(c(0,1-esp)))
  return(AUC)
  
  # 
  # area = 0
  # 
  # for (i in 1:length(esp)-1){
  #   base = abs(esp[i+1]-esp[i])
  #   altura1 = sensibilidad[i]
  #   altura2 = sensibilidad[i+1]
  #   
  #   area = area+altura1*base+(altura2-altura1)*base/2
  # 
  # }
  # return(area)
  
}


#####

# Prueba con CARS
library(ISLR)
library(tree)
library(pROC)

car=Carseats
attach(car)
High=ifelse(Sales<mean(Sales),0,1)
detach(car)

car = car[,-1]

lr=glm(High~.,data=car,family="binomial")
summary(lr)

predicted = predict(lr, car, type = "response")
pred = ifelse(p>0.5,1,0)


### Confusion
# Funcion R
table(predicted = pred, actual=High )

# Funcion nuestra
confusion(High, p, 0.5)


### ROC
# Funcion 1 R 
roc_tree = roc(High,predicted)
plot(roc_tree)

# Funcion 2 R
library(ROCR)
pred <- prediction(predicted, High)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)

# Funcion nuestra
ROC(High,p)


### AUC
# Funcion R
auc(roc_tree)

# Funcion nuestra
AUCnuestro(High,p)


#### 4
library(randomForest)
Highfactor = as.factor(High)

train=sample(1:nrow(car),nrow(car)*0.8)
car.test=car[-train,]
High.test=High[-train]
Highfactor.test = Highfactor[-train]

bag.car=randomForest(Highfactor~.,car,subset=train,mtry=(dim(car)[2])) # m = p. Le quite el -2 porque car ya no tiene High

imp=importance(bag.car)
imp # Importancia de valores
varImpPlot(bag.car)

bag.car$oob.times # Veces que cada dato no estuvo en el modelo


# ROC
pred_bag=predict(bag.car,car.test,type="prob")
roc_bag=roc(High.test,pred_bag[,2])
ROC(High.test,pred_bag[,2])

# AUC
AUCnuestro(High.test,pred_bag[,2])
