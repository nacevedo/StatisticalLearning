############################ Problema 2 ############################ 

#You can try to modify the denominator inside the cosine to change the complexity of f*
f=function(x){
  y=2+x^(.2)*cos(x/.15)/x^-.45
  return(y) }
plot(f,0,5)

#Points simulation: you change n and sigma
set.seed(1)
N=400
sigma=1.2
x=runif(N,0,5);x=sort(x)  #For convenience, the input x is sorted
y=rep(0,times=N)
for(i in 1:N){
  y[i]=f(x[i])+rnorm(1,0,sigma)
}
plot(x,y)
points(x,f(x),type="l",col=2,lwd=2)

library(splines)

num=10
part=seq(0,5,by=5/num)

### CAMBIAR DEGREE POR 2, PARA HACER LA OTRA PRUEBA Y VICEVERSA ### !!!!! IMPORTANTE !!!!!!
spline.lm <- lm(y ~ bs(x, knots=part,degree=2))
plot(f,0,5)
points(x,y)
lines(x[1:N], predict(spline.lm), lwd=2, col='yellow')

# AIC
aic = vector()
for(i in 1:200){
  
  part=seq(0,5,by=5/i)
  
  ### CAMBIAR DEGREE POR 2, PARA HACER LA OTRA PRUEBA Y VICEVERSA ### !!!!! IMPORTANTE !!!!!!
  spline.lm <- lm(y ~ bs(x, knots=part,degree=3))
  plot(f,0,5)
  points(x,y)
  lines(x[1:N], predict(spline.lm), lwd=2, col='yellow')
  
  aic[i] = AIC(spline.lm)
  
}

plot(seq(1:200),aic,type="l")

aic
min(aic) 
which.min(aic)


# LOOCV

cv = rep(0,50)
df = seq(2,51,length=50)
for(i in 1:50){cv[i] = smooth.spline(x,y,df=df[i],cv=TRUE)$cv}
plot(df,cv,type="l")
df[cv == min(cv)]

cv
min(cv)
df[which.min(cv)] 


