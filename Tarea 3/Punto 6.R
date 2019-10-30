# Punto 6
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
contour(xx1,xx1,f,nlevels=1,xlim=c(-10,10),ylim=c(-10,10))
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
contour(xx1,xx1,f,nlevels=1,xlim=c(-10,10),ylim=c(-10,10))
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
contour(xx1,xx1,f,nlevels=1,xlim=c(-10,10),ylim=c(-10,10))
points(x1,x2,col=y/2+1.5,lwd=2)

