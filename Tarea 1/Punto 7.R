library(ISLR)
library(corrplot)
library(leaps)
library(pls)

data=Carseats
n=sample(c(1:400),80,replace=F)

# Creación de train y test aleatorio
data = data[,-c(7,10,11)]

# Omitir valores nulos
data = na.omit(data)

train=data[-n,]
test=data[n,]


# PCA
pca2=prcomp(train[,-1],scale = TRUE)
pca = pcr(Sales~.,data=train, scale=T, validation = "CV")
summary(pca)
pca$rotation

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
plot(reg_sub_summary$cp,type="b", xlab = 'k', ylab = 'Cp') #El cp de mallows  
# El mejor es con 1, 2, 3, 4, 5, 6, 7

predpca = predict(pca,test[,-1])    
msepca=mean((test$Sales-predpca)^2)
msepca



### Hacer código que haga princomp!!!!!
corrplot(cor(data), method = "circle")

scale_train = scale(train[,-1])

S=t(var(scale_train))
eig = eigen(S)
W = eig$vectors
W = as.matrix(cbind(-W[,1],W[,2:3],-W[,4],W[,5:7]))
lambdas = eig$values

z=scale_train%*%W

corrplot(cor(z), method = "circle")
pairs(cbind(train$Sales, z))

comps = data.frame(cbind(z,Y = train[,1]))

# Variables
reg_subset=regsubsets(Y~.,data=train_comps,method="forward",nvmax=7) ## nvmax debe ser el total
reg_sub_summary=summary(reg_subset)
reg_sub_summary

reg_sub_summary$cp  
which.min(reg_sub_summary$cp)
plot(reg_sub_summary$cp,type="b") #El cp de mallows  


####
train_matrix = data.matrix(train[,-1]) # Pasar data frame a matriz
miu = colMeans(train_matrix)
s = colSds(train_matrix)

# Escalar Test
mius = rep(miu,nrow(test))
mius = matrix(mius, ncol = nrow(test))
mius = t(mius)

s = rep(s,nrow(test))
s = matrix(s, ncol = nrow(test))
s = t(s)
scaled = (train[,-1]-(mius))/s

scale_test = (test[,-1]-(mius))/s

scale_test = as.matrix(scale_test)


pp_test <- (scale_test %*% W)

test_comps = data.frame(cbind(Comp.1= pp_test[,1],
                              Comp.2= pp_test[,2],
                              Comp.3= pp_test[,3],
                              Comp.4= pp_test[,4],
                              Comp.5= pp_test[,5],
                              Comp.6= pp_test[,6],
                              Comp.7= pp_test[,7]))




                       


# Fit
fit = lm(V8~., data = train_comps)
summary(fit)
predpp=predict(fit,test_comps)
msecpa=mean((test$Sales-predpp)^2)
msecpa
















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
cvmod$lambda.min #Lambda mÃ­nimo

mod_pen2=glmnet(xtrain,train$Sales,alpha=0,lambda=cvmod$lambda.min)
coef(mod_pen2)

xtest = as.matrix(test[,2:8])


predp2=predict(mod_pen2,xtest)
mseridge=mean((test$Sales-predp2)^2)
mseridge





