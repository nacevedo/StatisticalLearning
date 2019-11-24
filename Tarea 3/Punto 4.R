# Punto 4
library(emdbook)
#library(Rfast)
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




## LDA
library(MASS)
require(ggplot2)
require(scales)
require(gridExtra)
lda=lda(Y~.,data= train)
lda #Ws

predicted = predict(lda, test[,1:2], probaility = TRUE)

library(ggplot2)
p1 <- ggplot(test) + geom_point(aes(lda.LD1, colour = train$Y, shape = train$Y), size = 2.5) 


# Otra
roc=roc(test$Y,predicted$posterior[,2])
plot(roc)

# Nuestra
ROC(test$Y,predicted$posterior[,2])

auc(test$Y, predicted$posterior[,2])
AUCnuestro(test$Y,predicted$posterior[,2])
# 0.6495


# Punto 3
# Paso 0
grupo1 = train[train$Y == 1,]
grupo0 = train[train$Y == -1,]

tamano1 = floor(nrow(grupo1)/3)
tamano0 = floor(nrow(grupo0)/3)

grupo11 = grupo1[1:tamano1,]
grupo12 = grupo1[(tamano1+1):(2*tamano1),]
grupo13 = grupo1[(2*tamano1+1):(nrow(grupo1)),]

grupo01 = grupo0[1:tamano0,]
grupo02 = grupo0[(tamano0+1):(2*tamano0),]
grupo03 = grupo0[(2*tamano0+1):(nrow(grupo0)),]


p_g11 = rep(0,nrow(grupo1))
p_g12 = rep(0,nrow(grupo1))
p_g13 = rep(0,nrow(grupo1))

p_g01 = rep(0,nrow(grupo0))
p_g02 = rep(0,nrow(grupo0))
p_g03 = rep(0,nrow(grupo0))


prev_11 = matrix(1,nrow(grupo11),ncol(grupo11))
prev_12 = rep(1,length(grupo1))
prev_13 = rep(1,length(grupo1))

prev_01 = rep(1,length(grupo0))
prev_02 = rep(1,length(grupo0))
prev_03 = rep(1,length(grupo0))


suma = 1
j = 0

while (suma != 0) {
  j = j+1
  
  # Medias y varianzas de grupos
  media_x_g11 = colMeans(grupo11)[1:2]
  media_x_g12 = colMeans(grupo12)[1:2]
  media_x_g13 = colMeans(grupo13)[1:2]
  
  media_x_g01 = colMeans(grupo01)[1:2]
  media_x_g02 = colMeans(grupo02)[1:2]
  media_x_g03 = colMeans(grupo03)[1:2]
  
  x_grupo1 = grupo1[,1:2]
  x_grupo0 = grupo0[,1:2]
  
  var_x_g1 = var(x_grupo1)
  var_x_g0 = var(x_grupo0)
  
  
  # Probabilidades de SubGrupos
  for(i in 1:nrow(grupo1)){
    p_g11[i] = dmvnorm(c(grupo1[i,1],grupo1[i,2]), mu = media_x_g11, Sigma = var_x_g1)
    p_g12[i] = dmvnorm(c(grupo1[i,1],grupo1[i,2]), mu = media_x_g12, Sigma = var_x_g1)
    p_g13[i] = dmvnorm(c(grupo1[i,1],grupo1[i,2]), mu = media_x_g13, Sigma = var_x_g1)
  }
  
  for(i in 1:nrow(grupo0)){
    p_g01[i] = dmvnorm(c(grupo0[i,1],grupo0[i,2]), mu = media_x_g01, Sigma = var_x_g0)
    p_g02[i] = dmvnorm(c(grupo0[i,1],grupo0[i,2]), mu = media_x_g02, Sigma = var_x_g0)
    p_g03[i] = dmvnorm(c(grupo0[i,1],grupo0[i,2]), mu = media_x_g03, Sigma = var_x_g0)
    
  }
  
  #paso 3: Cambio de grupo
  grupo11 = matrix(ncol=3)
  colnames(grupo11) = c('X1','X2', 'Y')
  
  grupo12 = matrix(ncol=3)
  colnames(grupo12) = c('X1','X2', 'Y')
  
  grupo13 = matrix(ncol=3)
  colnames(grupo13) = c('X1','X2', 'Y')
  
  grupo01 = matrix(ncol=3)
  colnames(grupo01) = c('X1','X2', 'Y')
  
  grupo02 = matrix(ncol=3)
  colnames(grupo02) = c('X1','X2', 'Y')
  
  grupo03 = matrix(ncol=3)
  colnames(grupo03) = c('X1','X2', 'Y')
  
  for(i in 1:nrow(grupo1)){
    if (p_g11[i] > p_g12[i] & p_g11[i] > p_g13[i]){
      grupo11 = rbind(grupo11,grupo1[i,])
    } else if (p_g12[i] > p_g11[i] & p_g12[i] > p_g13[i]){
      grupo12 = rbind(grupo12,grupo1[i,])
    } else {
      grupo13 = rbind(grupo13,grupo1[i,])
    }
  }
  
  for(i in 1:nrow(grupo0)){
    if (p_g01[i] > p_g02[i] & p_g01[i] > p_g03[i]){
      grupo01 = rbind(grupo01,grupo0[i,])
    } else if (p_g02[i] > p_g01[i] & p_g02[i] > p_g03[i]){
      grupo02 = rbind(grupo02,grupo0[i,])
    } else 
      grupo03 = rbind(grupo03,grupo0[i,])
  }
  
  
  grupo11 = grupo11[2:nrow(grupo11),]
  grupo12 = grupo12[2:nrow(grupo12),]
  grupo13 = grupo13[2:nrow(grupo13),]
  
  grupo01 = grupo01[2:nrow(grupo01),]
  grupo02 = grupo02[2:nrow(grupo02),]
  grupo03 = grupo03[2:nrow(grupo03),]
  
  
  # Revisar si cambiaron
  if (nrow(grupo11) == nrow(prev_11) && nrow(grupo12) == nrow(prev_12) && nrow(grupo13) == nrow(prev_13) && nrow(grupo01) == nrow(prev_01) && nrow(grupo02) == nrow(prev_02) && nrow(grupo03) == nrow(prev_03)){
    suma = sum(prev_11 != grupo11) + sum(prev_12 != grupo12) + sum(prev_13 != grupo13) + sum(prev_01 != grupo01) + sum(prev_02 != grupo02) + sum(prev_03 != grupo03)
  } else {
    suma = 100
     }
  
  # Actualizacion prev
  prev_11 = grupo11
  prev_12 = grupo12
  prev_13 = grupo13
  
  prev_01 = grupo01
  prev_02 = grupo02
  prev_03 = grupo03
  
  print(suma)
  print(j)
  
}

# Test
p_g11 = rep(0,length(y_test))
p_g12 = rep(0,length(y_test))
p_g13 = rep(0,length(y_test))

p_g01 = rep(0,length(y_test))
p_g02 = rep(0,length(y_test))
p_g03 = rep(0,length(y_test))

# Probabilidades de Grupos
for(i in 1:length(y_test)){
  p_g11[i] = dmvnorm(x_test[i,], mu = media_x_g11, Sigma = var_x_g1)
  p_g12[i] = dmvnorm(x_test[i,], mu = media_x_g12, Sigma = var_x_g1)
  p_g13[i] = dmvnorm(x_test[i,], mu = media_x_g13, Sigma = var_x_g1)
  
  p_g01[i] = dmvnorm(x_test[i,], mu = media_x_g01, Sigma = var_x_g0)
  p_g02[i] = dmvnorm(x_test[i,], mu = media_x_g02, Sigma = var_x_g0)
  p_g03[i] = dmvnorm(x_test[i,], mu = media_x_g03, Sigma = var_x_g0)
  
}

#paso 3: Asignacion de grupo
grupo11_test = matrix(ncol=3)
colnames(grupo11_test) = c('X1','X2', 'Y')

grupo12_test = matrix(ncol=3)
colnames(grupo12_test) = c('X1','X2', 'Y')

grupo13_test = matrix(ncol=3)
colnames(grupo13_test) = c('X1','X2', 'Y')

grupo01_test = matrix(ncol=3)
colnames(grupo01_test) = c('X1','X2', 'Y')

grupo02_test = matrix(ncol=3)
colnames(grupo02_test) = c('X1','X2', 'Y')

grupo03_test = matrix(ncol=3)
colnames(grupo03_test) = c('X1','X2', 'Y')


for (i in 1:length(y_test)){
  if (p_g11[i] > p_g12[i] & p_g11[i] > p_g13[i] & p_g11[i] > p_g01[i] & p_g11[i] > p_g02[i] & p_g11[i] > p_g03[i]){
    grupo11_test = rbind(grupo11_test,grupo1[i,])
  } else if (p_g12[i] > p_g11[i] & p_g12[i] > p_g13[i] & p_g12[i] > p_g01[i] & p_g12[i] > p_g02[i] & p_g12[i] > p_g03[i]){
    grupo12_test = rbind(grupo12_test,grupo1[i,])
  } else if (p_g13[i] > p_g11[i] & p_g13[i] > p_g12[i] & p_g13[i] > p_g01[i] & p_g13[i] > p_g02[i] & p_g13[i] > p_g03[i]){
    grupo13_test = rbind(grupo13_test,grupo1[i,])
  } else if (p_g01[i] > p_g12[i] & p_g01[i] > p_g13[i] & p_g01[i] > p_g11[i] & p_g01[i] > p_g02[i] & p_g01[i] > p_g03[i]){
    grupo01_test = rbind(grupo01_test,grupo0[i,])
  } else if (p_g02[i] > p_g11[i] & p_g02[i] > p_g13[i] & p_g02[i] > p_g01[i] & p_g02[i] > p_g12[i] & p_g02[i] > p_g03[i]){
    grupo02_test = rbind(grupo02_test,grupo0[i,])
  } else {
    grupo03_test = rbind(grupo03_test,grupo0[i,])
  }
  
}


grupo11_test = grupo11_test[2:nrow(grupo11_test),]
grupo12_test = grupo12_test[2:nrow(grupo12_test),]
grupo13_test = grupo13_test[2:nrow(grupo13_test),]

grupo01_test = grupo01_test[2:nrow(grupo01_test),]
grupo02_test = grupo02_test[2:nrow(grupo02_test),]
grupo03_test = grupo03_test[2:nrow(grupo03_test),]

prob1 = rep(0,nrow(test))


# Normalizacion de P(1)
for (i in 1:length(y_test)){
  prob1[i] = (p_g11[i]+p_g12[i]+p_g13[i])/(p_g11[i]+p_g12[i]+p_g13[i]+p_g01[i]+p_g02[i]+p_g03[i])
}

# AUC
ROC(test$Y,prob1)
AUCnuestro(test$Y,prob1)

library(mda)
library(pROC)
modelo = mda(Y~., data = train)
pred_mda=predict(modelo,test,type="posterior")
roc_mda=roc(test$Y,pred_mda[,2])
plot((1-roc_mda$specificities),roc_mda$sensitivities, ylab = 'Sensibilidad', xlab = '1-Especificidad', type = 'l', xlim = c(0,1), main = 'Curva ROC')
abline(c(0,0),c(1,1), col = 2)

plot(roc_mda)
auc_mda=auc(roc_mda)
auc_mda
