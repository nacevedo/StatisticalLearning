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
lda=lda(Y~.,data= train)
lda #Ws
plot(lda)

predicted = predict(lda, test[,1:2], probaility = TRUE)
# Otra
roc=roc(test$Y,predicted$posterior[,2])

# Nuestra
ROC(test$Y,predicted$posterior[,2])

auc(test$Y, predicted$posterior[,2])
AUCnuestro(test$Y,predicted$posterior[,2])
# 0.6495


# Punto 3



#paso 0
grupo1 = train[train$Y == 1,]
grupo0 = train[train$Y == -1,]

tamano1 = floor(nrow(grupo1)/3)
tamano0 = floor(nrow(grupo0)/3)

grupo11 = grupo1[1:tamano1,]
grupo12 = grupo1[tamano1+1:2*tamano1,]
grupo13 = grupo1[2*tamano1+1:nrow(grupo1),]

grupo01 = grupo0[1:tamano0,]
grupo02 = grupo0[tamano0+1:2*tamano0,]
grupo03 = grupo0[2*tamano0+1:nrow(grupo0),]


p_g11 = rep(0,nrow(grupo1))
p_g12 = rep(0,nrow(grupo1))
p_g13 = rep(0,nrow(grupo1))

p_g01 = rep(0,nrow(grupo0))
p_g02 = rep(0,nrow(grupo0))
p_g03 = rep(0,nrow(grupo0))


prev_11 = rep(1,length(grupo1))
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
  grupo12 = matrix(ncol=3)
  grupo13 = matrix(ncol=3)
  
  grupo01 = matrix(ncol=3)
  grupo02 = matrix(ncol=3)
  grupo03 = matrix(ncol=3)
  
  for(i in 1:nrow(grupo1)){
    if (p_g11[i] > p_g12[i] & p_g11[i] > p_g13[i]){
      grupo11 = rbind(grupo11,grupo1[i,])
    } else if (p_g12[i] > p_g11[i] & p_g12[i] > p_g13[i]){
      grupo12 = rbind(grupo12,grupo1[i,])
    } else 
      grupo13 = rbind(grupo13,grupo1[i,])
  }
  
  for(i in 1:nrow(grupo0)){
    if (p_g01[i] > p_g02[i] & p_g11[i] > p_g13[i]){
      grupo11 = rbind(grupo11,grupo1[i,])
    } else if (p_g12[i] > p_g11[i] & p_g12[i] > p_g13[i]){
      grupo12 = rbind(grupo12,grupo1[i,])
    } else 
      grupo13 = rbind(grupo13,grupo1[i,])
  }
  
  
  for (i in 1:length(train)){
    if (p_g11[i] > p_g12[i] & p_g11[i] > p_g13[i] & p_g11[i] > p_g21[i] & p_g11[i] > p_g22[i] & p_g11[i] > p_g23[i]){
      x_grupo11 = rbind(x_grupo11,x_train[i,])
    } else if (p_g12[i] > p_g11[i] & p_g12[i] > p_g13[i] & p_g12[i] > p_g21[i] & p_g12[i] > p_g22[i] & p_g12[i] > p_g23[i]){
      x_grupo12 = rbind(x_grupo12,x_train[i,])
    } else if (p_g13[i] > p_g11[i] & p_g13[i] > p_g12[i] & p_g13[i] > p_g21[i] & p_g13[i] > p_g22[i] & p_g13[i] > p_g23[i]){
      x_grupo13 = rbind(x_grupo13,x_train[i,])
    } else if (p_g21[i] > p_g12[i] & p_g21[i] > p_g13[i] & p_g21[i] > p_g11[i] & p_g21[i] > p_g22[i] & p_g21[i] > p_g23[i]){
      x_grupo21 = rbind(x_grupo21,x_train[i,])
    } else if (p_g22[i] > p_g11[i] & p_g22[i] > p_g13[i] & p_g22[i] > p_g21[i] & p_g22[i] > p_g12[i] & p_g22[i] > p_g23[i]){
      x_grupo22 = rbind(x_grupo22,x_train[i,])
    } else {
      x_grupo23 = rbind(x_grupo23,x_train[i,])
    }
    
  }
  
  x_grupo11 = x_grupo11[2:nrow(x_grupo11),]
  x_grupo12 = x_grupo12[2:nrow(x_grupo12),]
  x_grupo13 = x_grupo13[2:nrow(x_grupo13),]
  
  x_grupo21 = x_grupo21[2:nrow(x_grupo21),]
  x_grupo22 = x_grupo22[2:nrow(x_grupo22),]
  x_grupo23 = x_grupo23[2:nrow(x_grupo23),]
  
  
  # Revisar si cambiaron
  if (length(x_grupo11) == length(prev_11) && length(x_grupo12) == length(prev_12) && length(x_grupo13) == length(prev_13) && length(x_grupo21) == length(prev_21) && length(x_grupo22) == length(prev_22) && length(x_grupo23) == length(prev_23)){
    suma = sum(prev_11 != x_grupo11) + sum(prev_12 != x_grupo12) + sum(prev_13 != x_grupo13) + sum(prev_21 != x_grupo21) + sum(prev_22 != x_grupo22) + sum(prev_23 != x_grupo23)
  } else {
    suma = 100
     }
  
  # Actualizacion prev
  prev_11 = x_grupo11
  prev_12 = x_grupo12
  prev_13 = x_grupo13
  
  prev_21 = x_grupo21
  prev_22 = x_grupo22
  prev_23 = x_grupo23
  
  print(suma)
  print(j)
  
}

# Test
p_g11 = rep(0,length(y_test))
p_g12 = rep(0,length(y_test))
p_g13 = rep(0,length(y_test))

p_g21 = rep(0,length(y_test))
p_g22 = rep(0,length(y_test))
p_g23 = rep(0,length(y_test))

# Probabilidades de Grupos
for(i in 1:length(y_test)){
  p_g11[i] = dmvnorm(x_test[i,], mu = media_x_g11, Sigma = var_x_g1)
  p_g12[i] = dmvnorm(x_test[i,], mu = media_x_g12, Sigma = var_x_g1)
  p_g13[i] = dmvnorm(x_test[i,], mu = media_x_g13, Sigma = var_x_g1)
  
  p_g21[i] = dmvnorm(x_test[i,], mu = media_x_g21, Sigma = var_x_g2)
  p_g22[i] = dmvnorm(x_test[i,], mu = media_x_g22, Sigma = var_x_g2)
  p_g23[i] = dmvnorm(x_test[i,], mu = media_x_g23, Sigma = var_x_g2)
  
}

#paso 3: Cambio de grupo
x_grupo11_test = matrix(ncol=2)
x_grupo12_test = matrix(ncol=2)
x_grupo13_test = matrix(ncol=2)

x_grupo21_test = matrix(ncol=2)
x_grupo22_test = matrix(ncol=2)
x_grupo23_test = matrix(ncol=2)


for (i in 1:length(y_test)){
  if (p_g11[i] > p_g12[i] & p_g11[i] > p_g13[i] & p_g11[i] > p_g21[i] & p_g11[i] > p_g22[i] & p_g11[i] > p_g23[i]){
    x_grupo11_test = rbind(x_grupo11_test,x_test[i,])
  } else if (p_g12[i] > p_g11[i] & p_g12[i] > p_g13[i] & p_g12[i] > p_g21[i] & p_g12[i] > p_g22[i] & p_g12[i] > p_g23[i]){
    x_grupo12_test = rbind(x_grupo12_test,x_test[i,])
  } else if (p_g13[i] > p_g11[i] & p_g13[i] > p_g12[i] & p_g13[i] > p_g21[i] & p_g13[i] > p_g22[i] & p_g13[i] > p_g23[i]){
    x_grupo13_test = rbind(x_grupo13_test,x_test[i,])
  } else if (p_g21[i] > p_g12[i] & p_g21[i] > p_g13[i] & p_g21[i] > p_g11[i] & p_g21[i] > p_g22[i] & p_g21[i] > p_g23[i]){
    x_grupo21_test = rbind(x_grupo21_test,x_test[i,])
  } else if (p_g22[i] > p_g11[i] & p_g22[i] > p_g13[i] & p_g22[i] > p_g21[i] & p_g22[i] > p_g12[i] & p_g22[i] > p_g23[i]){
    x_grupo22_test = rbind(x_grupo22_test,x_test[i,])
  } else {
    x_grupo23_test = rbind(x_grupo23_test,x_test[i,])
  }
  
}

x_grupo11_test = x_grupo11_test[2:nrow(x_grupo11_test),]
x_grupo12_test = x_grupo12_test[2:nrow(x_grupo12_test),]
x_grupo13_test = x_grupo13_test[2:nrow(x_grupo13_test),]

x_grupo21_test = x_grupo21_test[2:nrow(x_grupo21_test),]
x_grupo22_test = x_grupo22_test[2:nrow(x_grupo22_test),]
x_grupo23_test = x_grupo23_test[2:nrow(x_grupo23_test),]

