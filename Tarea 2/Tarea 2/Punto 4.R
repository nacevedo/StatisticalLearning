#You can try to modify the denominator inside the cosine to change the complexity of f*
f=function(x){
  y=2+x^(.2)*cos(x/.15)/x^-.45
  return(y)
}
plot(f,0,5)
#Points simulation: you change n and sigma
N=400
sigma=1.2
x=runif(N,0,5);x=sort(x) #For convenience, the input x is sorted
y=rep(0,times=N)
for(i in 1:N){
  y[i]=f(x[i])+rnorm(1,0,sigma)
}
plot(x,y)
points(x,f(x),type="l",col=2,lwd=2)


L=function(hh,x_test,x_train){
  N = length(x_test)
  N2 = length(x_train)
  L=matrix(rep(0,times=N2*N2), ncol=N2)
  
  for(m in 1:N){
    cond = 0
    zz=rep(x_test[m],times=N2)
    for(j in 1:N2){
      cond[j] = ifelse(abs(x_test[m]-x_train[j]) > hh, 0, 1)
    }
    bottom=sum((1-(zz-x_train)^2/h)^2*cond)
    L[m,]=(1-(zz-x_train)^2/h)^2*cond/bottom
  }
  return(L)
}





# 5 Fold CV
xCV <-x[sample(length(x))]
yCV <-y[sample(length(y))]
data = cbind(xCV, yCV)

# Partir
folds <- cut(seq(1,nrow(data)),breaks=5,labels=FALSE)

MSE_final= vector()
h=seq(.1,2.1,by=.5)

for(k in 1:length(h)){
  MSE = vector()
    for(i in 1:5){
    test_i <- which(folds==i,arr.ind=TRUE)
    test <- data[test_i, ]
    x_test = test[,1]
    y_test = test[,2]
    train <- data[-test_i, ]
    x_train = train[,1]
    y_train = train[,2]
    
    ff = L(h,x_test,x_train)%*%y_train
    MSE[i] = mean((ff-y_test)^2)
  }
  MSE_final[k] = mean(MSE)
}
plot(MSE_final, type = 'l', xlab = 'k', ylab = 'MSE')
which.min(MSE_final)
min(MSE_final)