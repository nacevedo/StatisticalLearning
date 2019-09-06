#You can try to modify the denominator inside the cosine to change the complexity of f*
f=function(x){
  y=2+x^(.2)*cos(x/.15)/x^-.45
  return(y) }
plot(f,0,5)

#Points simulation: you change n and sigma
N=1000
sigma=1.2
x=runif(N,0,5);x=sort(x)  #For convenience, the input x is sorted
y=rep(0,times=N)
for(i in 1:N){
  y[i]=f(x[i])+rnorm(1,0,sigma)
}
plot(x,y)
points(x,f(x),type="l",col=2,lwd=2)

data = cbind(x,y)

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

plot(MSE, type="l")
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
x=runif(N,0,5);#x=sort(x)  #For convenience, the input x is sorted
y=rep(0,times=N)
for(i in 1:N){
  y[i]=f(x[i])+rnorm(1,0,sigma)
}
plot(x,y)
points(x,f(x),type="l",col=2,lwd=2)

x_test = x[301:600]
y_test = y[301:600]

x_train =x[1:300]
y_train = y[1:300]



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

plot(MSE)
min(MSE)
which.min(MSE) # 16
# Error irreducible es 1.44


##### c ##########  CV y LOO
N=400
sigma=1.2
x=runif(N,0,5);#x=sort(x)  #For convenience, the input x is sorted
y=rep(0,times=N)
for(i in 1:N){
  y[i]=f(x[i])+rnorm(1,0,sigma)
}
plot(x,y)
points(x,f(x),type="l",col=2,lwd=2)

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
plot(MSE_final)
which.min(MSE_final)
min(MSE_final)


# LOO
data <-data[sample(nrow(data)),]
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
plot(MSE_final_LOO)
which.min(MSE_final_LOO)
min(MSE_final_LOO)





#You can try to modify the denominator inside the cosine to change the complexity of f*
f=function(x){
  y=2+x^(.2)*cos(x/.15)/x^-.45
  return(y) }
plot(f,0,5)

#Points simulation: you change n and sigma
N=1000
sigma=1.2
x=runif(N,0,5);#x=sort(x)  #For convenience, the input x is sorted
y=rep(0,times=N)
for(i in 1:N){
  y[i]=f(x[i])+rnorm(1,0,sigma)
}
plot(x,y)
points(x,f(x),type="l",col=2,lwd=2)

x_test = x[501:N]
y_test = y[501:N]

x_train =x[1:500]
y_train = y[1:500]



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

plot(MSE)
min(MSE)
which.min(MSE) # 15
# Error irreducible es 1.44



k=14 #Puede cambiarlo


plot(x,y)
points(x,f(x),type="l",col=2,lwd=2)
points(z,kn(k,T),type="l",col=4,lwd=2)



##################### e Bootstrap