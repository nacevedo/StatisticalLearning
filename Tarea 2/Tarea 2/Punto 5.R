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

gams.fit <- gam(Salary~s(CRuns)+s(CWalks), data=train)

# Resumen de gams
summary(gams.fit)

# Plot de CWalks vs s(CWalks)
plot(gams.fit)

# Plot de cada una de las variables
par(mfrow=c(2,2))
plot(gams.fit,se=T,col=4,lwd=2)

# Plot de los datos a predecir (Salary)
par(mfrow=c(1,1))
plot(Hitters$Salary)


#Funcion L

Lfung = function(h,x){
   N = length(x)
   L = matrix(rep(0,times=N*N), ncol=N)
   for(i in 1:N){
     zz = rep(x[i],times=N)
     bottom = sum(dnorm((zz-x)/h))
     L[i,] = dnorm((zz-x)/h)/bottom
   }
   return(L)
 }

Lfunt = function(h,x){
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

f_hatt = function(h,x,y){
  c(Lfunt(h,x)%*%y) 
} 

f_hatg = function(h,x,y){
  c(Lfung(h,x)%*%y) 
} 

LOOCV_Normal = function(L,y){
  N = length(y)
  ll = diag(L)
  ff = L%*%y
  tra = mean(ll)
  MSE = (1/N)*sum(((y-ff)/(1-ll))^2)
  return(MSE)
}

h_Lg = function(x,y){
  
  h = seq(.1,max(x),by=5)
  MSE = vector()
  
  for(k in 1:length(h)){
    
    hh = h[k]
    MSE[k] = LOOCV_Normal(Lfung(hh,x), y)
    
  }
  return(h[which.min(MSE)])
}

h_Lt = function(x,y){
  
  h = seq(.1,max(x),by=5)
  MSE = vector()
  
  for(k in 1:length(h)){
    
    hh = h[k]
    MSE[k] = LOOCV_Normal(Lfunt(hh,x), y)
    
  }
  return(h[which.min(MSE)])
}



### Generalized Additive Models GAMS (a mano)

s0 = mean(train$Salary)

#CRuns
f1 = 0
f1_prev = -1

#CWalks
f2 = 0
f2_prev = -1

m = 0 
Y = train$Salary
Runs = train$CRuns
Walks = train$CWalks

h1 = .01
h1_prev = -1
h2 = .02
h2_prev = -1

N = length(Y)
cont = 0

while((abs(h1 - h1_prev)>1*10^-8 || abs(h2 - h2_prev) >1*10^-8) && cont <= 10){
  cont = cont + 1
  h1_prev = h1
  h2_prev = h2
  f1_prev = f1
  f2_prev = f2
  
  nf1 = Y - s0 - f2_prev
  nf2 = Y - s0 - f1_prev 
  
  h1 = h_Lg(Runs,nf1)
  f1 = f_hatg(h2, Walks, nf1)
  f1 = f1-mean(f1)
  
  
  
  h2 = h_Lt(Walks,nf2)
  f2 = f_hatt(h1, Runs,nf2)
  f2 = f2-mean(f2)
  
  print(cont)
}


# Plot de cada una de las variables
par(mfrow=c(2,2))
plot(gams.fit,se=T,col=4,lwd=2)
plot(Runs, f1, xlab = 'CRuns', ylab = 's(CRuns)')
plot(Walks, f2,  xlab = 'CWalks', ylab = 's(CWalks)')



## Test ##
h1 = 85.1
#1501.1
h2 = 265.1
s0 = mean(train$Salary)
testRuns = test$CRuns
testWalks = test$CWalks

## Runs
hh = h1

x_test = testRuns
y_test = test$Salary
    
x_train = Runs
y_train = train$Salary
    
N = length(x_test)
N2 = length(x_train)
mat=matrix(rep(0,times=N*N2), ncol=N2)
  
for(m in 1:N){
    cond = 0
    zz=rep(x_test[m],times=N2)
    bottom = sum(dnorm((zz-x_train)/hh))
    mat[m,] = dnorm((zz-x_train)/hh)/bottom
}
    
ff = mat%*%y_train



### Walks ###
hh = h2

x_test = testWalks
y_test = test$Salary

x_train = Walks
y_train = train$Salary

N = length(x_test)
N2 = length(x_train)
mat=matrix(rep(0,times=N*N2), ncol=N2)

for(m in 1:N){
  cond = 0
  zz=rep(x_test[m],times=N2)
  for(j in 1:N2){
    cond[j] = ifelse(abs(x_test[m]-x_train[j]) > hh, 0, 1)
  }
  bottom=sum((1-abs(zz-x_train)/hh)*cond)
  mat[m,]=(1-abs(zz-x_train)/hh)*cond/bottom
}

ff2 = mat%*%y_train
MSE = mean((ff+ff2-y_test)^2)



