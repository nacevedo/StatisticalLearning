library(ISLR)
library(mgcv)
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
par(mfrow=c(1,3))
plot(gams.fit,se=T,col=4,lwd=2)

# Plot de los datos a predecir (Salary)
par(mfrow=c(1,1))
plot(Hitters$Salary)

### Funciones útiles 

# Funcion L

# Lfun = function(h,x){
#   N = length(x)
#   L = matrix(rep(0,times=N*N), ncol=N)
#   for(i in 1:N){
#     zz = rep(x[i],times=N)
#     bottom = sum(dnorm((zz-x)/h))
#     L[i,] = dnorm((zz-x)/h)/bottom
#   }
#   return(L)
# }

Lfun=function(h,x){
  N = length(x)
  L=matrix(rep(0,times=N*N), ncol=N)
  for(i in 1:N){
    cond = vector()
    zz=rep(x[i],times=N)
    for(j in 1:N){
      cond[j] = ifelse(abs(x[i]-x[j]) > (h), 0, 1)
    }
    bottom=sum((1-abs(zz-x)/h)*cond)
    L[i,]=(1-abs(zz-x)/h)*cond/bottom
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

h1 = .01
h1_prev = -1
h2 = .02
h2_prev = -1

N = length(Y)
cont = 0

while(sum(abs(f1 - f1_prev)) >1*10^-8 && sum(abs(f2 - f2_prev)) >1*10^-8 && cont <= 12){
  cont = cont + 1
  h1_prev = h1
  h2_prev = h2
  f1_prev = f1
  f2_prev = f2
  
  nf1 = Y - s0 - f2_prev
  nf2 = Y - s0 - f1_prev 
  
  h1 = h_L(data$CRuns,nf1)
  f1 = f_hat(h2, data$CWalks, nf1)
  f1 = f1-mean(f1)
  
  
  
  h2 = h_L(data$CWalks,nf2)
  f2 = f_hat(h1, data$CRuns,nf2)
  f2 = f2-mean(f2)
  
  print(cont)
}








