# Punto 4

library(Rfast)
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
train = sample(1:length(y_train),175)
x_grupo1 = x_train[-train,]
y_grupo1 = y_train[-train]
x_grupo2 = x_train[train,]
y_grupo2 = y_train[train]

#paso 1

media_x_g1 = colMeans(x_grupo1)
media_x_g2 = colMeans(x_grupo2)

var_x_g1 = var(x_grupo1)
var_x_g2 = var(x_grupo2)

#paso 2 

#denominador

p_g1 = rep(0,length(x_grupo1))
p_g2 = rep(0,length(x_grupo2))

for(i in 1:length(x_train[,1])){
  
  p_g1[i] = dmvnorm(x_train[i,], mu = media_x_g1, sigma = var_x_g1)
  p_g2[i] = dmvnorm(x_train[i,], mu = media_x_g2, sigma = var_x_g2)
  
}

#paso 3

x_grupo1 = x_grupo1[p_g1 > p_g2]
x_grupo2 = x_grupo2[p_g1 <= p_g2]
