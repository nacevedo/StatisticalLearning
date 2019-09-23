library(ISLR)
library(gam)
library(splines)

# Creación de datos

?Hitters
data = Hitters
data = na.omit(data)
dim(data)
head(data)
index = sample(1:dim(data)[1],64)
test = data[index,]
train = data[-index,]

# Generalized Additive Models GAMS (automático)

gams.fit <- gam(Salary~s(CRuns)+s(CWalks), data=data)

# Resumen de gams
summary(gams.fit)

# Plot de CWalks vs s(CWalks)
plot(gams.fit)

# Plot de cada una de las variables
par(mfrow=c(1,2))
plot(gams.fit,se=T,col=4,lwd=2)

# Plot de los datos a predecir (Salary)
par(mfrow=c(1,1))
plot(Hitters$Salary)

### Funciones útiles 

# Función L

Lfun = function(h,x){
  N = length(x)
  L = matrix(rep(0,times=N*N), ncol=N)
  for(i in 1:N){
    zz = rep(x[i],times=N)
    bottom = sum(dnorm((zz-x)/h))
    L[i,] = dnorm((zz-x)/h)/bottom
  }
  return(L)
}

f_hat = function(h,x,y){
  c(Lfun(h,x)%*%y) 
} 

LOOCV_Normal = function(L,y){
  N = length(y)
  ll = diag(L)
  ff = L%*%y
  tra = mean(ll)
  MSE = (1/N)*sum(((y-ff)/(1-ll))^2)
  return(MSE)
}

h_L = function(x,y){
  
  h = seq(.1,max(x),by=5)
  MSE = vector()
  
  for(k in 1:length(h)){
    
    hh = h[k]
    MSE[k] = LOOCV_Normal(Lfun(hh,x), y)
    
  }
  return(h[which.min(MSE)])
}



### Generalized Additive Models GAMS (a mano)

s0 = mean(data$Salary)

#CRuns
f1 = 0

#CWalks
f2 = 0

m = 0 
Y = data$Salary

h1 = 0
h1_prev = -1
h2 = 0
h2_prev = -1

while(h1 != h1_prev && h2 != h2_prev){
  
  h1_prev = h1
  h2_prev = h2
  
  h1 = h_L(data$CRuns,Y - s0 - f2)
  f1 = f_hat(h1, data$CRuns, Y - s0 - f2)
  
  h2 = h_L(data$CWalks,Y - s0 - f1)
  f2 = f_hat(h2, data$CWalks, Y -s0 - f1)
  
}






#bw = .35
#kk = ksmooth(x, y, kernel = "normal", bandwidth = bw)

