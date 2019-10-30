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
hh=function(theta){
  mm=matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),ncol=2)
  return(mm)}
