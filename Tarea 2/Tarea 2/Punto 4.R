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
  L=matrix(rep(0,times=N*N2), ncol=N2)
  
  for(m in 1:N){
    cond = 0
    zz=rep(x_test[m],times=N2)
    for(j in 1:N2){
      cond[j] = ifelse(abs(x_test[m]-x_train[j]) > hh, 0, 1)
    }
    bottom=sum((1-(zz-x_train)^2/hh)^2*cond)
    L[m,]=(1-(zz-x_train)^2/hh)^2*cond/bottom
  }
  return(L)
}


xCV <-x[sample(length(x))]
yCV <-y[sample(length(y))]
data = cbind(xCV, yCV)


# 5 Fold CV
# Partir
folds <- cut(seq(1,nrow(data)),breaks=5,labels=FALSE)

MSE_final= vector()
h=seq(.1,5,by=.1)

for(k in 1:length(h)){
  MSE = vector()
  hh = h[k]
    for(i in 1:5){
    test_i <- which(folds==i,arr.ind=TRUE)
    test <- data[test_i, ]
    x_test = test[,1]
    y_test = test[,2]
    train <- data[-test_i, ]
    x_train = train[,1]
    y_train = train[,2]
    
    N = length(x_test)
    N2 = length(x_train)
    mat=matrix(rep(0,times=N*N2), ncol=N2)
    
    for(m in 1:N){
      cond = 0
      zz=rep(x_test[m],times=N2)
      for(j in 1:N2){
        cond[j] = ifelse(abs(x_test[m]-x_train[j]) > hh, 0, 1)
      }
      bottom=sum((1-(zz-x_train)^2/hh)^2*cond)
      mat[m,]=(1-(zz-x_train)^2/hh)^2*cond/bottom
    }
    
    ff = mat%*%y_train
    MSE[i] = mean((ff-y_test)^2)
  }
  MSE_final[k] = mean(MSE)
}
plot(h, MSE_final, type = 'l', xlab = 'h', ylab = 'MSE', main = '5 - Fold CV')
h[which.min(MSE_final)]
min(MSE_final)


### 10 Fold CV
# Partir
folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)

MSE_final10 = vector()

for(k in 1:length(h)){
  MSE = vector()
  hh = h[k]
  for(i in 1:10){
    test_i <- which(folds==i,arr.ind=TRUE)
    test <- data[test_i, ]
    x_test = test[,1]
    y_test = test[,2]
    train <- data[-test_i, ]
    x_train = train[,1]
    y_train = train[,2]
    
    N = length(x_test)
    N2 = length(x_train)
    mat=matrix(rep(0,times=N*N2), ncol=N2)
    
    for(m in 1:N){
      cond = 0
      zz=rep(x_test[m],times=N2)
      for(j in 1:N2){
        cond[j] = ifelse(abs(x_test[m]-x_train[j]) > hh, 0, 1)
      }
      bottom=sum((1-(zz-x_train)^2/hh)^2*cond)
      mat[m,]=(1-(zz-x_train)^2/hh)^2*cond/bottom
    }
    
    ff = mat%*%y_train
    MSE[i] = mean((ff-y_test)^2)
  }
  MSE_final10[k] = mean(MSE)
}
plot(h, MSE_final10, type = 'l', xlab = 'h', ylab = 'MSE', main = '10 - Fold CV')
h[which.min(MSE_final10)]
min(MSE_final10)

# Comparacion 
plot(h, MSE_final, type = 'l', xlab = 'h', ylab = 'MSE', main = 'Cross Validation', col = 4)
lines(h, MSE_final10, type = 'l', xlab = 'h', ylab = 'MSE', col = 2)
legend('topright',legend = c('5 - Fold CV', '10 - Fold CV'),col=c("red", "blue"),lty=1, cex=0.8)


#### LOOCV ####
folds <- cut(seq(1,nrow(data)),breaks=400,labels=FALSE)

MSE_final400 = vector()

for(k in 1:length(h)){
  MSE = vector()
  hh = h[k]
  for(i in 1:400){
    test_i <- which(folds==i,arr.ind=TRUE)
    test <- data[test_i, ]
    x_test = test[1]
    y_test = test[2]
    train <- data[-test_i, ]
    x_train = train[,1]
    y_train = train[,2]
    
    N = length(x_test)
    N2 = length(x_train)
    mat=matrix(rep(0,times=N*N2), ncol=N2)
    
    for(m in 1:N){
      cond = 0
      zz=rep(x_test[m],times=N2)
      for(j in 1:N2){
        cond[j] = ifelse(abs(x_test[m]-x_train[j]) > hh, 0, 1)
      }
      bottom=sum((1-(zz-x_train)^2/hh)^2*cond)
      mat[m,]=(1-(zz-x_train)^2/hh)^2*cond/bottom
    }
    
    ff = mat%*%y_train
    MSE[i] = mean((ff-y_test)^2)
  }
  MSE_final400[k] = mean(MSE)
}
plot(h, MSE_final400, type = 'l', xlab = 'h', ylab = 'MSE', main = '10 - Fold CV')
h[which.min(MSE_final400)]
min(MSE_final400)


# Comparacion 
plot(h, MSE_final, type = 'l', xlab = 'h', ylab = 'MSE', main = 'Cross Validation', col = 4)
lines(h, MSE_final10, type = 'l', xlab = 'h', ylab = 'MSE', col = 2)
lines(h, MSE_final400, type = 'l', xlab = 'h', ylab = 'MSE', col = 9)

legend('topright',legend = c('5 - Fold CV', '10 - Fold CV', 'LOOCV'),col=c("red", "blue","black"),lty=1, cex=0.8)

plot(h, MSE_final5, type = 'l', xlab = 'h', ylab = 'MSE', main = '10 - Fold CV')
h[which.min(MSE_final5)]
min(MSE_final5)


### IC ###



#### Bootstrap ###
B = 500
ybar_boot = c(rep(0,times=B)) # Creacion de vector vacio


for(i in 1:B){
  sample = sample(seq(1:N), N, replace = T)
  x_train = x[sample]
  y_train = y[sample]
  x_test = c(3, 4)
  
  NN = length(x_test)
  N2 = length(x_train)
  mat=matrix(rep(0,times=NN*N2), ncol=N2)
  
  for(m in 1:NN){
    cond = 0
    zz=rep(x_test[m],times=N2)
    for(j in 1:N2){
      cond[j] = ifelse(abs(x_test[m]-x_train[j]) > hh, 0, 1)
    }
    bottom=sum((1-(zz-x_train)^2/hh)^2*cond)
    mat[m,]=(1-(zz-x_train)^2/hh)^2*cond/bottom
  }
  
  ybar_boot[i] = (mat%*%y)[1]-(mat%*%y)[2]
}


plot(density(ybar_boot))  # Plot de medidas obtenidas con el bootstrap
c(quantile(ybar_boot,0.025),quantile(ybar_boot,0.975))


