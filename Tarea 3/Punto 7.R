# Punto 7

library(AppliedPredictiveModeling)
library(MASS)
library(pROC)
library(klaR)
library(caret)

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

#forward
lda_for = stepclass(y_train~.,data=train_b,method="lda",direction="forward", improvement=0.001)
lda_for
summary(lda_for)
lda_for$formula

lda_b = lda(y_train ~ Z40 + Z73 + Z89 + Z136 + Z162 + Z163 + Z164 + Z166 + 
              Z172, data = train_b)
lda_b #Ws
plot(lda_b)

predl_b = predict(lda_b,newdata=x_test_b)$class
errorl_b = 1 - mean(abs(as.numeric(predl_b)==as.numeric(y_test)))

########################### QDA ########################### 

qda_b = qda(y_train~., data = train_b)

predq_b = predict(qda_b, newdata = x_test_b)$class
errorq_b = 1 - mean(abs(as.numeric(predq_b)==as.numeric(y_test)))

########################### Regresión Logística ########################### 

# define training control
train_control_b <- trainControl(method = "cv", number = 10)

# train the model on training set
model_b <- caret::train(y_train~.,
               data = train_b,
               trControl = train_control_b,
               method = "multinom",
               family = binomial())

# print cv scores
summary(model_b)

predlog_b = predict(model, x_test_b)
errorlog_b = 1- mean(abs((predlog_b)==(y_test)))

########################### Print Errores ########################### 
errorl_b  #Error en LDA
errorq_b  #Error en Qda
errorlog_b  #Error en LR

########################### Huella Química ########################### 

########################### LDA ########################### 

train_c = train_c[, -c(52,  58,  77,  90,  94, 107, 158)]

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

# define training control
train_control_c <- trainControl(method = "cv", number = 10)

# train the model on training set
model_c <- caret::train(y_train~.,
                        data = train_c,
                        trControl = train_control_c,
                        method = "multinom",
                        family = binomial())

# print cv scores
summary(model_c)

predlog_c = predict(model, x_test_c)
errorlog_c = 1- mean(abs((predlog_c)==(y_test)))

########################### Print Errores ########################### 
errorl_c  #Error en LDA
errorq_c  #Error en Qda
errorlog_c  #Error en LR

########################### Juntando modelos ########################### 

train = sample(1:length(y),225)
x_test_1 = x_b[-train,]
x_test_2 = x_c[-train,]
y_test = y[-train]
x_train_1 = x_b[train,]
x_train_2 = x_c[train,]
y_train = y[train]

data_train = cbind(x_train_1, x_train_2, y_train)
data_test = cbind(x_test_1, x_test_2, y_test)

########################### LDA ########################### 

data_train = data_train[, -c(78, 235, 273, 364)]

lda = lda(y_train~., data = data_train)
lda #Ws
plot(lda)

predl = predict(lda,newdata = cbind(x_test_1, x_test_2))$class
errorl = 1 - mean(abs(as.numeric(predl)==as.numeric(y_test)))

########################### QDA ########################### 

qda = qda(y_train~., data = data_train)

predq = predict(qda, newdata = cbind(x_test_1, x_test_2))$class
errorq = 1 - mean(abs(as.numeric(predq)==as.numeric(y_test)))

########################### Regresión Logística ########################### 

logi = glm(y_train~., data = data_train, family=binomial)
summary(logi)

predlog = (sign(predict(logi, cbind(x_test_1, x_test_2)))+1)/2
errorlog = 1- mean(abs(as.numeric(predlog)==as.numeric(y_test)))

########################### Print Errores ########################### 
errorl  #Error en LDA
errorq  #Error en Qda
errorlog  #Error en LR

