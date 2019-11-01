# Punto 7

library(AppliedPredictiveModeling)
library(MASS)
library(pROC)

data(hepatic)

# Creación de datos
x_b = bio
x_c = chem
y = injury

#Eliminación de columnas con el mismo dato 
x_b = x_b[vapply(x_b, function(x) length(unique(x)) > 1, logical(1L))]
x_c = x_c[vapply(x_c, function(x) length(unique(x)) > 1, logical(1L))]

data_b = data.frame(x_b,y)
data_c = data.frame(x_c,y)

# Separación de train y test
set.seed(2)
train = sample(1:length(y),225)
x_test_b = x_b[-train,]
x_test_c = x_c[-train,]
y_test = y[-train]
x_train_b = x_b[train,]
x_train_c = x_c[train,]
y_train = y[train]

train_b = cbind(x_train_b, y_train)
train_c = cbind(x_train_c, y_train)
test_b = cbind(x_test_b, y_test)
test_c = cbind(x_test_c, y_test)

# No hay NAs
sum(is.na(data_b))
sum(is.na(data_c))

########################### Biológicos ########################### 

########################### LDA ########################### 

lda_b = lda(y_train~., data = train_b)
lda_b #Ws
plot(lda_b)

predl_b = predict(lda_b,newdata=x_test_b)$class
errorl_b = 1 - mean(abs(as.numeric(predl_b)==as.numeric(y_test)))

########################### QDA ########################### 

qda_b = qda(y_train~., data = train_b)

predq_b = predict(qda_b, newdata = x_test_b)$class
errorq_b = 1 - mean(abs(as.numeric(predq_b)==as.numeric(y_test)))

########################### Regresión Logística ########################### 

logi_b = glm(y_train~., data = train_b, family=binomial)
summary(logi_b)

predlog_b = (sign(predict(logi_b, x_test_b))+1)/2
errorlog_b = 1- mean(abs(as.numeric(predlog_b)==as.numeric(y_test)))

########################### Print Errores ########################### 
errorl_b  #Error en LDA
errorq_b  #Error en Qda
errorlog_b  #Error en LR

########################### Huella Química ########################### 

########################### LDA ########################### 

lda_c = lda(y_train~., data = train_c)
lda_c #Ws
plot(lda_c)

predl_c = predict(lda_c,newdata = x_test_c)$class
errorl_c = 1 - mean(abs(as.numeric(predl_c)==as.numeric(y_test)))

########################### QDA ########################### 

qda_c = qda(y_train~., data = train_c)

predq_c = predict(qda_c, newdata = x_test_c)$class
errorq_c = 1 - mean(abs(as.numeric(predq_c)==as.numeric(y_test)))

########################### Regresión Logística ########################### 

logi_c = glm(y_train~., data = train_c, family=binomial)
summary(logi_c)

predlog_c = (sign(predict(logi_c, x_test_c))+1)/2
errorlog_c = 1- mean(abs(as.numeric(predlog_c)==as.numeric(y_test)))

########################### Print Errores ########################### 
errorl_c  #Error en LDA
errorq_c  #Error en Qda
errorlog_c  #Error en LR

