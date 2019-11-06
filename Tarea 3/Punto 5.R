# Punto 5
# 1 Generar
#Generacion de inputs y tamano de muestra
N=500
x1=rnorm(N,0,3)
x2=rnorm(N,0,4)
plot(x1,x2) #grafica de inputs
#Definicion de funcion discriminante
a=8 #complejidad en x1
b=5 #complejidad en x2
f_discrim=function(x1,x2){
  f=sin(a*x1/10)+sin(b*x2/10)
  return(f)
}

#Generacion de clases (respuesta)
y=sign(f_discrim(x1,x2)+rnorm(N,0,.4))
#Graficando los datos con fun. discriminante
xx1 <- seq(-10, 10, len = 100)
f=outer(xx1,xx1,f_discrim)
contour(xx1,xx1,f,nlevels=1,xlim=c(-10,10),ylim=c(-10,10))
points(x1,x2,col=y/2+1.5,lwd=2)
x=cbind(x1,x2)
x_train = x[1:350,]
x_test = x[351:N,]
y_train = y[1:350]
y_test = y[351:N]

train = as.data.frame(cbind(x_train, y_train))
colnames(train) = c('X1','X2', 'Y')

test = as.data.frame(cbind(x_test, y_test))
colnames(test) = c('X1','X2', 'Y')

# Rotacion
library(randomForest)
y_train = as.factor(y_train)
y_test = as.factor(y_test)

hh=function(theta){
  mm=matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),ncol=2)
  return(mm)}

N = 500
theta = runif(N, 0, 2*pi)
y = matrix(0,nrow(x_test), N)
auc = matrix(0,nrow(x_test), N)

for (i in 1:N){
 rotados = x_train%*%hh(theta[i])
 bag = randomForest(y_train~., data = rotados, mtry = (dim(rotados)[2]))
 
 imp = importance(bag)
 #imp
 #varImpPlot(bag)
 
 rotados_test = x_test%*%hh(theta[i])
 
 test = as.data.frame(cbind(y_test,rotados_test))
 colnames(test) = c('y_test', 'V1', 'V2')
 
 pred_bag = predict(bag, test, type="prob")
 y[,i] = predict(bag, test, type = "class")
 
 
 library(pROC)
 
 roc_bag=roc(test$y_test,pred_bag[,2])
 plot(roc_bag)
 auc[i]=auc(roc_bag)
} 

ygorro = (y==2)*1+(y==1)*-1
ybarra = rowMeans(ygorro)
yfinal = (ybarra<0)*-1+(ybarra>=0)*1

aucfinal = auc(y_test,yfinal)




  ### PREGUNTAR: QUE SE HACE CON LOS DATOS DE CADA ITERACION

test_norot = as.data.frame(cbind(y_test,x_test))

# Random Forest
rf.data=randomForest(y_train~., data = x_train) # m = 8

imp_rf=importance(rf.data)
imp_rf
varImpPlot(rf.data)

pred_rf=predict(rf.data,test_norot,type="prob")

roc_rf=roc(test_norot$y_test,pred_rf[,2])
plot(roc_rf)
auc_rf=auc(roc_rf)
auc_rf


# MDA
library(mda)
mda_out <- mda(Y~., data = train)
colnames(test_norot) = c('Y', 'X1', 'X2')

pred_mda=predict(mda_out,test_norot,type="posterior")

roc_mda=roc(test_norot$Y,pred_mda[,2])
plot(roc_mda)
auc_mda=auc(roc_mda)
auc_mda
