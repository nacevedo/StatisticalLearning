# Punto 6
library(e1071)
library(pROC)
#Generacion de inputs y tamano de muestra
N=200
x1=rnorm(N,0,3)
x2=rnorm(N,0,4)
plot(x1,x2) #grafica de inputs
#Definicion de funcion discriminante

# (1,1)
a=1 #complejidad en x1
b=1 #complejidad en x2
f_discrim=function(x1,x2){
  f=sin(a*x1/10)+sin(b*x2/10)
  return(f)
}
#Generacion de clases (respuesta)
y=sign(f_discrim(x1,x2))
#Graficando los datos con fun. discriminante
par(mfrow=c(1,3))
xx1 <- seq(-10, 10, len = 100)
f=outer(xx1,xx1,f_discrim)
contour(xx1,xx1,f,nlevels=1,xlim=c(-10,10),ylim=c(-10,10), main = '(1,1)')
points(x1,x2,col=y/2+1.5,lwd=2)


# (8,8)
a=8 #complejidad en x1
b=8 #complejidad en x2
f_discrim=function(x1,x2){
  f=sin(a*x1/10)+sin(b*x2/10)
  return(f)
}
#Generacion de clases (respuesta)
y=sign(f_discrim(x1,x2))
#Graficando los datos con fun. discriminante
xx1 <- seq(-10, 10, len = 100)
f=outer(xx1,xx1,f_discrim)
contour(xx1,xx1,f,nlevels=1,xlim=c(-10,10),ylim=c(-10,10),main = '(8,8)')
points(x1,x2,col=y/2+1.5,lwd=2)

# (1,7)
a=1 #complejidad en x1
b=7 #complejidad en x2
f_discrim=function(x1,x2){
  f=sin(a*x1/10)+sin(b*x2/10)
  return(f)
}
#Generacion de clases (respuesta)
y=sign(f_discrim(x1,x2))
#Graficando los datos con fun. discriminante
xx1 <- seq(-10, 10, len = 100)
f=outer(xx1,xx1,f_discrim)
contour(xx1,xx1,f,nlevels=1,xlim=c(-10,10),ylim=c(-10,10), main = '(1,7)')
points(x1,x2,col=y/2+1.5,lwd=2)



############## 2 ##############
############## (5,2) N = 300

##### Generacion de datos
#### Train
N=300
x1=rnorm(N,0,3)
x2=rnorm(N,0,4)

a=5 #complejidad en x1
b=2 #complejidad en x2
f_discrim=function(x1,x2){
  f=sin(a*x1/10)+sin(b*x2/10)
  return(f)
}

y=sign(f_discrim(x1,x2))
train = as.data.frame(cbind(y,x1,x2))

#### Test
N=200
x1=rnorm(N,0,3)
x2=rnorm(N,0,4)
plot(x1,x2) #grafica de inputs
#Definicion de funcion discriminante

a=5 #complejidad en x1
b=2 #complejidad en x2
f_discrim=function(x1,x2){
  f=sin(a*x1/10)+sin(b*x2/10)
  return(f)
}

y=sign(f_discrim(x1,x2))
test = as.data.frame(cbind(y,x1,x2))

### SVM
rang = list(cost = c(3,4,5,6), gamma = c(0.01,0.02,0.05,0.1))

#Tunning en dos parametros de calibracion
tune = tune(svm, y~., data = train, ranges = rang)
tune


# Modelo Calibrado
# Lineal
svm_lineal = svm(y~., data = train, cost = tune$best.parameters$cost, gamma = tune$best.parameters$gamma, probability=T, kernel="linear")
pred_svm_lineal = predict(svm_lineal, test, type = "response")
pred_svm_prob_lineal = predict(svm_lineal, test, type = "prob", probability = TRUE)

roc_svm_lineal = roc(test$y, pred_svm_lineal)
plot(roc_svm_lineal)
auc_svm_lineal = auc(roc_svm_lineal)
auc_svm_lineal


# Radial
svm_radial = svm(y~., data = train, cost = tune$best.parameters$cost, gamma = tune$best.parameters$gamma, probability=T, kernel="radial")
pred_svm_radial = predict(svm_radial, test, type = "response")
pred_svm_prob_radial = predict(svm_radial, test, type = "prob", probability = TRUE)

roc_svm_radial = roc(test$y, pred_svm_radial)
plot(roc_svm_radial)
auc_svm_radial = auc(roc_svm_radial)
auc_svm_radial


# Polinomial
svm_poli = svm(y~., data = train, cost = tune$best.parameters$cost, gamma = tune$best.parameters$gamma, probability=T, kernel="polynomial")
pred_svm_poli = predict(svm_poli, test, type = "response")
pred_svm_prob_poli = predict(svm_poli, test, type = "prob", probability = TRUE)

roc_svm_poli = roc(test$y, pred_svm_poli)
plot(roc_svm_poli)
auc_svm_poli = auc(roc_svm_poli)
auc_svm_poli



# (5,2) N = 800
N=800
x1=rnorm(N,0,3)
x2=rnorm(N,0,4)

a=5 #complejidad en x1
b=2 #complejidad en x2
f_discrim=function(x1,x2){
  f=sin(a*x1/10)+sin(b*x2/10)
  return(f)
}

y=sign(f_discrim(x1,x2))
train = as.data.frame(cbind(y,x1,x2))

#### Test
N=600
x1=rnorm(N,0,3)
x2=rnorm(N,0,4)
plot(x1,x2) #grafica de inputs
#Definicion de funcion discriminante

a=5 #complejidad en x1
b=2 #complejidad en x2
f_discrim=function(x1,x2){
  f=sin(a*x1/10)+sin(b*x2/10)
  return(f)
}

y=sign(f_discrim(x1,x2))
test = as.data.frame(cbind(y,x1,x2))

### SVM
rang = list(cost = c(3,4,5,6), gamma = c(0.01,0.02,0.05,0.1))

#Tunning en dos parametros de calibracion
tune = tune(svm, y~., data = train, ranges = rang)
tune


# Modelo Calibrado
# Lineal
svm_lineal = svm(y~., data = train, cost = tune$best.parameters$cost, gamma = tune$best.parameters$gamma, probability=T, kernel="linear")
pred_svm_lineal = predict(svm_lineal, test, type = "response")
pred_svm_prob_lineal = predict(svm_lineal, test, type = "prob", probability = TRUE)

roc_svm_lineal = roc(test$y, pred_svm_lineal)
plot(roc_svm_lineal)
auc_svm_lineal = auc(roc_svm_lineal)
auc_svm_lineal


# Radial
svm_radial = svm(y~., data = train, cost = tune$best.parameters$cost, gamma = tune$best.parameters$gamma, probability=T, kernel="radial")
pred_svm_radial = predict(svm_radial, test, type = "response")
pred_svm_prob_radial = predict(svm_radial, test, type = "prob", probability = TRUE)

roc_svm_radial = roc(test$y, pred_svm_radial)
plot(roc_svm_radial)
auc_svm_radial = auc(roc_svm_radial)
auc_svm_radial


# Polinomial
svm_poli = svm(y~., data = train, cost = tune$best.parameters$cost, gamma = tune$best.parameters$gamma, probability=T, kernel="polynomial")
pred_svm_poli = predict(svm_poli, test, type = "response")
pred_svm_prob_poli = predict(svm_poli, test, type = "prob", probability = TRUE)

roc_svm_poli = roc(test$y, pred_svm_poli)
plot(roc_svm_poli)
auc_svm_poli = auc(roc_svm_poli)
auc_svm_poli




# (8,8) N = 300
N=300
x1=rnorm(N,0,3)
x2=rnorm(N,0,4)

a=8 #complejidad en x1
b=8 #complejidad en x2
f_discrim=function(x1,x2){
  f=sin(a*x1/10)+sin(b*x2/10)
  return(f)
}

y=sign(f_discrim(x1,x2))
train = as.data.frame(cbind(y,x1,x2))

#### Test
N=200
x1=rnorm(N,0,3)
x2=rnorm(N,0,4)
plot(x1,x2) #grafica de inputs
#Definicion de funcion discriminante

a=8 #complejidad en x1
b=8 #complejidad en x2
f_discrim=function(x1,x2){
  f=sin(a*x1/10)+sin(b*x2/10)
  return(f)
}

y=sign(f_discrim(x1,x2))
test = as.data.frame(cbind(y,x1,x2))

### SVM
rang = list(cost = c(3,4,5,6), gamma = c(0.01,0.02,0.05,0.1))

#Tunning en dos parametros de calibracion
tune = tune(svm, y~., data = train, ranges = rang)
tune


# Modelo Calibrado
# Lineal
svm_lineal = svm(y~., data = train, cost = tune$best.parameters$cost, gamma = tune$best.parameters$gamma, probability=T, kernel="linear")
pred_svm_lineal = predict(svm_lineal, test, type = "response")
pred_svm_prob_lineal = predict(svm_lineal, test, type = "prob", probability = TRUE)

roc_svm_lineal = roc(test$y, pred_svm_lineal)
plot(roc_svm_lineal)
auc_svm_lineal = auc(roc_svm_lineal)
auc_svm_lineal


# Radial
svm_radial = svm(y~., data = train, cost = tune$best.parameters$cost, gamma = tune$best.parameters$gamma, probability=T, kernel="radial")
pred_svm_radial = predict(svm_radial, test, type = "response")
pred_svm_prob_radial = predict(svm_radial, test, type = "prob", probability = TRUE)

roc_svm_radial = roc(test$y, pred_svm_radial)
plot(roc_svm_radial)
auc_svm_radial = auc(roc_svm_radial)
auc_svm_radial


# Polinomial
svm_poli = svm(y~., data = train, cost = tune$best.parameters$cost, gamma = tune$best.parameters$gamma, probability=T, kernel="polynomial")
pred_svm_poli = predict(svm_poli, test, type = "response")
pred_svm_prob_poli = predict(svm_poli, test, type = "prob", probability = TRUE)

roc_svm_poli = roc(test$y, pred_svm_poli)
plot(roc_svm_poli)
auc_svm_poli = auc(roc_svm_poli)
auc_svm_poli



# (8,8) N = 800
N=800
x1=rnorm(N,0,3)
x2=rnorm(N,0,4)

a=8 #complejidad en x1
b=8 #complejidad en x2
f_discrim=function(x1,x2){
  f=sin(a*x1/10)+sin(b*x2/10)
  return(f)
}

y=sign(f_discrim(x1,x2))
train = as.data.frame(cbind(y,x1,x2))

#### Test
N=600
x1=rnorm(N,0,3)
x2=rnorm(N,0,4)
plot(x1,x2) #grafica de inputs
#Definicion de funcion discriminante

a=8 #complejidad en x1
b=8 #complejidad en x2
f_discrim=function(x1,x2){
  f=sin(a*x1/10)+sin(b*x2/10)
  return(f)
}

y=sign(f_discrim(x1,x2))
test = as.data.frame(cbind(y,x1,x2))

### SVM
rang = list(cost = c(3,4,5,6), gamma = c(0.01,0.02,0.05,0.1))

#Tunning en dos parametros de calibracion
tune = tune(svm, y~., data = train, ranges = rang)
tune


# Modelo Calibrado
# Lineal
svm_lineal = svm(y~., data = train, cost = tune$best.parameters$cost, gamma = tune$best.parameters$gamma, probability=T, kernel="linear")
pred_svm_lineal = predict(svm_lineal, test, type = "response")
pred_svm_prob_lineal = predict(svm_lineal, test, type = "prob", probability = TRUE)

roc_svm_lineal = roc(test$y, pred_svm_lineal)
plot(roc_svm_lineal)
auc_svm_lineal = auc(roc_svm_lineal)
auc_svm_lineal


# Radial
svm_radial = svm(y~., data = train, cost = tune$best.parameters$cost, gamma = tune$best.parameters$gamma, probability=T, kernel="radial")
pred_svm_radial = predict(svm_radial, test, type = "response")
pred_svm_prob_radial = predict(svm_radial, test, type = "prob", probability = TRUE)

roc_svm_radial = roc(test$y, pred_svm_radial)
plot(roc_svm_radial)
auc_svm_radial = auc(roc_svm_radial)
auc_svm_radial


# Polinomial
svm_poli = svm(y~., data = train, cost = tune$best.parameters$cost, gamma = tune$best.parameters$gamma, probability=T, kernel="polynomial")
pred_svm_poli = predict(svm_poli, test, type = "response")
pred_svm_prob_poli = predict(svm_poli, test, type = "prob", probability = TRUE)

roc_svm_poli = roc(test$y, pred_svm_poli)
plot(roc_svm_poli)
auc_svm_poli = auc(roc_svm_poli)
auc_svm_poli

