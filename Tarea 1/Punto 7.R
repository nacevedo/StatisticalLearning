library(ISLR)
library(corrplot)
library(leaps)

data=Carseats
n=sample(c(1:400),80,replace=F)

# Creación de train y test aleatorio
data = data[,-c(7,10,11)]

# Omitir valores nulos
data = na.omit(data)

train=data[-n,]
test=data[n,]


scale_train=scale(train)

# Componentes principales
pca = pcr(Sales~.,data=train, scale=T, validation = "CV")
summary(pca)
predpca = predict(pca,test)    
msepca=mean((test$Sales-predpca)^2)
msepca
pca$scores



# PCA
scale_train=scale(train[,-8])
pp=princomp(scale_train,scores=TRUE)

zz=pp$scores
train_comps = data.frame(cbind(zz,train$Sales))

# Variables
reg_subset=regsubsets(V8~.,data=train_comps,method="forward",nvmax=7) ## nvmax debe ser el total
reg_sub_summary=summary(reg_subset)
reg_sub_summary

reg_sub_summary$cp  
which.min(reg_sub_summary$cp)
plot(reg_sub_summary$cp,type="b") #El cp de mallows  
# El mejor es con 1, 2, 3, 4, 5, 6, 7


### Hacer código que haga princomp!!!!!

corrplot(cor(data), method = "circle")

S=var(scale_train[,-1])
eig = eigen(S)
W = eig$vectors
lambdas = eig$values

XX=as.matrix(scale_train[,-1])  
Z=XX%*%W

corrplot(cor(Z), method = "circle")

pp=princomp(scale_train[,-1],scores=TRUE)
zz=pp$scores
train_comps = data.frame(cbind(Z,scale_train[,1]))

# Variables
reg_subset=regsubsets(scale_train[,1]~.,data=train_comps,method="forward",nvmax=7) ## nvmax debe ser el total
reg_sub_summary=summary(reg_subset)
reg_sub_summary

reg_sub_summary$cp  
which.min(reg_sub_summary$cp)
plot(reg_sub_summary$cp,type="b") #El cp de mallows  
# El mejor es con 1, 4, 6, 7

reg=lm(scale_train[,1]~.,data=scale_train[,-1],scale=F,validation="CV")

predpl=predict(pls,test)
msepls=mean((test$Sales-predpl)^2)
msepls

### PLSR
pls=plsr(Sales~.,data=train,scale=T,validation="CV")
summary(pls)

zz = pls$scores
train_comps = data.frame(cbind(zz,train$Sales))

predpl=predict(pls,test)
msepls=mean((test$Sales-predpl)^2)
msepls


### Ridge
xtrain = as.matrix(train[,-1])
cvmod=cv.glmnet(xtrain,train$Sales,alpha=0)
cvmod$lambda.min
plot(cvmod)
cvmod$lambda.min #Lambda mínimo

mod_pen2=glmnet(xtrain,train$Sales,alpha=0,lambda=cvmod$lambda.min)
coef(mod_pen2)

xtest = as.matrix(test[,2:8])


predp2=predict(mod_pen2,xtest)
mseridge=mean((test$Sales-predp2)^2)
mseridge





