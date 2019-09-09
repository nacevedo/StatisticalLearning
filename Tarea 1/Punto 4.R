#You can try to modify the denominator inside the cosine to change the complexity of f*
f=function(x){
  y=2+x^(.2)*cos(x/.15)/x^-.45
  return(y) }
plot(f,0,5)

#Points simulation: you change n and sigma
N=400
sigma=1.2
x=runif(N,0,5);x=sort(x)  #For convenience, the input x is sorted
y=rep(0,times=N)
for(i in 1:N){
  y[i]=f(x[i])+rnorm(1,0,sigma)
}
plot(x,y)
points(x,f(x),type="l",col=2,lwd=2)

data = cbind(x,y)

# Creaci?n de train y test
ss=seq(1:N)
ss=sample(ss,N,replace=F)
ss1=ss[1:300]
ss2=ss[301:N]


y_train=y[ss1]
x_train=x[ss1]
y_test=y[ss2]
x_test=x[ss2]


MSE <- vector()

#Estimator by k-neighbors
#k es el numero de vecinos y test=TRUE/FALSE determina si se estima sobre
#un grid cualquiera (FALSE) o sobre los x en la muestra de test (TRUE)
kn=function(k,test){
  if(test=="FALSE"){z=seq(0,5,by=0.01);ll=length(z)}  #predict on train
  if(test=="TRUE"){z=x_test;ll=length(z)}    #x_test is the name of set for prediction
  nk=rep(0,times=ll)
  for(j in 1:ll){
    veci=which(abs(z[j]-x_train) %in% sort(abs(z[j]-x_train))[1:k])
    nk[j]=sum(y_train[veci])/k
  }
  
  return(nk) 
  
}

#Graficando el estimador
for(k in 1:200){
  pred = kn(k,T)
  MSE[k] = mean((pred-y_test)^2)
  
}

plot(MSE, type="l", xlab = 'k')

min(MSE)
which.min(MSE) # 15
# Error irreducible es 1.44



k=14 #Puede cambiarlo


plot(x,y)
points(x,f(x),type="l",col=2,lwd=2)
points(z,kn(k,T),type="l",col=4,lwd=2)
points(z,kn(200,T),type="l",col=4,lwd=2)



##### b #######
#You can try to modify the denominator inside the cosine to change the complexity of f*
f=function(x){
  y=2+x^(.2)*cos(x/.15)/x^-.45
  return(y) }
plot(f,0,5)

#Points simulation: you change n and sigma
N=600
sigma=1.2
x=runif(N,0,5);x=sort(x)  #For convenience, the input x is sorted
y=rep(0,times=N)
for(i in 1:N){
  y[i]=f(x[i])+rnorm(1,0,sigma)
}
plot(x,y)
points(x,f(x),type="l",col=2,lwd=2)

ss=seq(1:N)
ss=sample(ss,N,replace=F)
ss1=ss[1:300]
ss2=ss[301:N]


y_train=y[ss1]
x_train=x[ss1]
y_test=y[ss2]
x_test=x[ss2]



MSE_600 <- vector()

#Estimator by k-neighbors
#k es el numero de vecinos y test=TRUE/FALSE determina si se estima sobre
#un grid cualquiera (FALSE) o sobre los x en la muestra de test (TRUE)
kn=function(k,test){
  if(test=="FALSE"){z=seq(0,5,by=0.01);ll=length(z)}  #predict on train
  if(test=="TRUE"){z=x_test;ll=length(z)}    #x_test is the name of set for prediction
  nk=rep(0,times=ll)
  for(j in 1:ll){
    veci=which(abs(z[j]-x_train) %in% sort(abs(z[j]-x_train))[1:k])
    nk[j]=sum(y_train[veci])/k
  }
  
  return(nk) 
  
}

#Graficando el estimador


for(k in 1:200){
  pred = kn(k,T)
  MSE_600[k] = mean((pred-y_test)^2)
  
}

plot(MSE_600, type="l", xlab = 'k')
min(MSE_600)
which.min(MSE_600) # 16
# Error irreducible es 1.44


##### c ##########  CV y LOO
N=400
sigma=1.2
x=runif(N,0,5);x=sort(x)  #For convenience, the input x is sorted
y=rep(0,times=N)
for(i in 1:N){
  y[i]=f(x[i])+rnorm(1,0,sigma)
}
plot(x,y)
points(x,f(x),type="l",col=2,lwd=2)

ss=seq(1:N)
ss=sample(ss,N,replace=F)

y=y[ss]
x=x[ss]

data = cbind(x,y)

## CV
# Aleatorizar
data <-data[sample(nrow(data)),]

# Partir
folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)

# 10 CV
MSE_final= vector()

for(k in 1:200){
  MSE = vector()
  
  
  for(i in 1:10){
    test_i <- which(folds==i,arr.ind=TRUE)
    test <- data[test_i, ]
    x_test = test[,1]
    y_test = test[,2]
    train <- data[-test_i, ]
    x_train = train[,1]
    y_train = train[,2]
    
    pred = kn(k,T)
    MSE[i] = mean((pred-y_test)^2)
  }
  MSE_final[k] = mean(MSE)
}
plot(MSE_final, type = 'l', xlab = 'k', ylab = 'MSE')
which.min(MSE_final)
min(MSE_final)


# LOO
folds <- cut(seq(1,nrow(data)),breaks=N,labels=FALSE)

MSE_final_LOO= vector()

for(k in 1:200){
  MSE = vector()
  
  
  for(i in 1:N){
    test_i <- which(folds==i,arr.ind=TRUE)
    test <- data[test_i,]
    x_test = test[1]
    y_test = test[2]
    train <- data[-test_i, ]
    x_train = train[,1]
    y_train = train[,2]
    
    pred = kn(k,T)
    MSE[i] = mean((pred-y_test)^2)
  }
  MSE_final_LOO[k] = mean(MSE)
}
plot(MSE_final_LOO, type = 'l', xlab = 'k', ylab = 'MSE')
which.min(MSE_final_LOO)
min(MSE_final_LOO)



#### d ######

#You can try to modify the denominator inside the cosine to change the complexity of f*
f=function(x){
  y=2+x^(.2)*cos(x/.15)/x^-.45
  return(y) }
plot(f,0,5)

#Points simulation: you change n and sigma
N=400
#sigma=1.2
#sigma=2
sigma = 2
x=runif(N,0,5);x=sort(x)  #For convenience, the input x is sorted
y=rep(0,times=N)
for(i in 1:N){
  y[i]=f(x[i])+rnorm(1,0,sigma)
}
#plot(x,y)
#points(x,f(x),type="l",col=2,lwd=2)

ss=seq(1:N)
ss=sample(ss,N,replace=F)
ss1=ss[1:300]
ss2=ss[301:N]


y_train=y[ss1]
x_train=x[ss1]
y_test=y[ss2]
x_test=x[ss2]


MSE_2 <- vector()

#Estimator by k-neighbors
#k es el numero de vecinos y test=TRUE/FALSE determina si se estima sobre
#un grid cualquiera (FALSE) o sobre los x en la muestra de test (TRUE)
kn=function(k,test){
  if(test=="FALSE"){z=seq(0,5,by=0.01);ll=length(z)}  #predict on train
  if(test=="TRUE"){z=x_test;ll=length(z)}    #x_test is the name of set for prediction
  nk=rep(0,times=ll)
  for(j in 1:ll){
    veci=which(abs(z[j]-x_train) %in% sort(abs(z[j]-x_train))[1:k])
    nk[j]=sum(y_train[veci])/k
  }
  
  return(nk) 
  
}

#Graficando el estimador


for(k in 1:200){
  pred = kn(k,T)
  MSE_2[k] = mean((pred-y_test)^2)
  
}

plot(MSE_1.2, type = 'l', xlab = 'k', ylab = 'MSE',ylim = c(0, 9))
lines(MSE_2, col = 2)
lines(MSE_0.5, col = 4)
min(MSE)
which.min(MSE_1.2)
which.min(MSE_2)
which.min(MSE_0.5)


##################### e Bootstrap ##################### 

N=400
sigma=1.2
x=runif(N,0,5);x=sort(x)  #For convenience, the input x is sorted
y=rep(0,times=N)
for(i in 1:N){
  y[i]=f(x[i])+rnorm(1,0,sigma)
}
plot(x,y)
points(x,f(x),type="l",col=2,lwd=2)

ss=seq(1:N)
ss=sample(ss,N,replace=F)
ss=ss[1:400]

y_train=y[ss]
x_train=x[ss]

### Bootstrap final con 500 resamples ###

B = 500
ybar_boot = rep(0,times=B) # Creaci?n de vector vac?o
xbar_boot = rep(2,times=B)

for(i in 1:B)
{
  sampl = sample(seq(1:N), N, replace = T) # T: Muestreo con remplazo
  x_train = x[sampl]
  y_train = y[sampl]
  x_test = 2
  
  ybar_boot[i] = kn(9, T)
}

plot(density(ybar_boot))  # Plot de medidas obtenidas con el bootstrap
c(quantile(ybar_boot,0.025),quantile(ybar_boot,0.975))

