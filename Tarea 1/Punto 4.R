#You can try to modify the denominator inside the cosine to change the complexity of f*
f=function(x){
  y=2+x^(.2)*cos(x/.15)/x^-.45
  return(y) }
plot(f,0,5)

#Points simulation: you change n and sigma
N=400
sigma=1.2
x=runif(N,0,5);#x=sort(x)  #For convenience, the input x is sorted
y=rep(0,times=N)
for(i in 1:N){
  y[i]=f(x[i])+rnorm(1,0,sigma)
}
plot(x,y)
points(x,f(x),type="l",col=2,lwd=2)

x_test = x[301:400]
y_test = y[301:400]

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
# Error irreducible es 1.44



k=14 #Puede cambiarlo


plot(x,y)
points(x,f(x),type="l",col=2,lwd=2)
points(z,kn(k,T),type="l",col=4,lwd=2)
points(z,kn(200,T),type="l",col=4,lwd=2)


