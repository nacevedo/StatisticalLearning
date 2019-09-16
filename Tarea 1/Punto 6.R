library(corrplot)
library(leaps)
library(matrixStats)
library(pls)
library(glmnet)

#Generacion de variables
x1=rnorm(150,0,4)
x2=-1.5*x1+rnorm(150,0,3)
y=10+2*x1+1.5*x2+rnorm(150,0,3)
par(mfrow=c(1,3))
plot(x1,y) ; plot(x2,y) ; plot(x1,x2)


# Modelo lineal
x=cbind(x1,x2)
fit=lm(y~x); summary(fit)
cor(x1,x2)
plot(fit)


## a. Sí tienen sentido. Ambos están correlacionados

###### b ######
fit1=lm(y~x1); summary(fit1)
fit2=lm(y~x2); summary(fit2)

### c #####
x3=1*y+rnorm(150,0,4)
x4=-6*x2+rnorm(150,0,3)
x5=5*x1+rnorm(150,0,5)
x6=0.3*x3+rnorm(150,0,4)
x=cbind(x1,x2,x3,x4,x5,x6)
fit=lm(y~x); summary(fit)

data = cbind(y,x)
data = as.data.frame(data)

pairs(data) ## Graficar correlacion
corrplot(cor(data), method = "circle")


# Creación de test y train
index=sample(c(1:150),75,replace=F)
test = data[index,]
train = data[-index,]


# Secuencial
reg_subset=regsubsets(y~.,data=train,method="forward",nvmax=6) ## nvmax debe ser el total
reg_sub_summary=summary(reg_subset)
reg_sub_summary

reg_sub_summary$cp 

par(mfrow=c(1,3))
plot(reg_sub_summary$cp,type="b") #El cp de mallows
plot(reg_sub_summary$bic,type="b",col="red")  #El BIC
plot(reg_sub_summary$adjr2,type="b",col="blue")  #El R2ajustado

which.min(reg_sub_summary$cp)
# 4
fit_sec = lm(y~x1+x2+x3+x4, data = train)
pred=predict(fit_sec,test)
msesec=mean((test$y-pred)^2)
msesec


# Exaustivo
reg_subset=regsubsets(y~.,data=train,method="exhaustive",nvmax=6) ## nvmax debe ser el total
reg_sub_summary=summary(reg_subset)
reg_sub_summary

reg_sub_summary$cp 

par(mfrow=c(1,3))
plot(reg_sub_summary$cp,type="b") #El cp de mallows
plot(reg_sub_summary$bic,type="b",col="red")  #El BIC
plot(reg_sub_summary$adjr2,type="b",col="blue")  #El R2ajustado

which.min(reg_sub_summary$cp)
# 3
fit_ex = lm(y~x1+x2+x3, data = train)
pred=predict(fit_ex,test)
mseex=mean((test$y-pred)^2)
mseex


# PCA
# Escala train
scale_train=scale(train[,-7])
            
# Componentes principales
pp=princomp(scale_train,scores=TRUE)

zz=pp$scores
train_comps = data.frame(cbind(zz,train$y))

# Variables
reg_subset=regsubsets(V7~.,data=train_comps,method="forward",nvmax=6) ## nvmax debe ser el total
reg_sub_summary=summary(reg_subset)
reg_sub_summary

reg_sub_summary$cp  
which.min(reg_sub_summary$cp)
plot(reg_sub_summary$cp,type="b") #El cp de mallows  
# El mejor es con 1, 2, 3, 4, 5, 6

### PCA
pca = pcr(y~.,data=train, scale=T, validation = "CV")
summary(pca)
predpca = predict(pca,test, ncomp = 1:6)    
msepca=mean((test$y-predpca)^2)
msepca


### PLS
scale_train=scale(train[,-7])

pls=plsr(y~.,data=train,scale=T,validation="CV")
summary(pls)

zz = pls$scores
train_comps = data.frame(cbind(zz,train$y))

# Variables
reg_subset=regsubsets(V7~.,data=train_comps,method="forward",nvmax=6) ## nvmax debe ser el total
reg_sub_summary=summary(reg_subset)
reg_sub_summary

reg_sub_summary$cp  
which.min(reg_sub_summary$cp)
plot(reg_sub_summary$cp,type="b") #El cp de mallows  
# El mejor es con 1, 2, 3, 4, 5


predpl=predict(pls,test)
msepls=mean((test$y-predpl)^2)
msepls

xtrain = train[,2:7]
xtrain = as.matrix(xtrain)

###### Ridge #####
cvmod=cv.glmnet(xtrain,train$y,alpha=0)
cvmod$lambda.min
plot(cvmod)
cvmod$lambda.min #Lambda mínimo

mod_pen2=glmnet(xtrain,train$y,alpha=0,lambda=cvmod$lambda.min)
coef(mod_pen2)

xtest = as.matrix(test[,2:7])


predp2=predict(mod_pen2,xtest)
mseridge=mean((test$y-predp2)^2)
mseridge


###### Lasso #######
cvmod=cv.glmnet(xtrain,train$y,alpha=1)
cvmod$lambda.min
plot(cvmod)
cvmod$lambda.min #Lambda mínimo

mod_pen2=glmnet(xtrain,train$y,alpha=1,lambda=cvmod$lambda.min)
coef(mod_pen2)

xtest = as.matrix(test[,2:7])


predp2=predict(mod_pen2,xtest)
mselasso=mean((test$y-predp2)^2)
mselasso
