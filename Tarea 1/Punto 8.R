library(psych)
library(corrplot)
library(leaps)
library(pls)
library(glmnet)

## Leer el archivo

data = read.table("communities.data", sep = ",")
head(data)

#Quitar columnas y datos nulos (correr sólo una vez)

s = seq(97,116)
data = data[,c(-1,-2,-3,-4,-5,-s,-117,-118,-119,-120,-122,-123,-124,-125,-127)]
data = na.omit(data)

r = describeBy(data)

#varible a predecir: V128 (posición 94 del arreglo)

#creación de train y test

x = as.data.frame(data[,-94])
y = data$V128

N = length(data$V6)
ss = seq(1:N)
ss = sample(ss,N,replace=F)
ss1 = ss[1:1593]
ss2 = ss[1594:N]

y_train = y[ss1]
x_train = x[ss1,]
y_val = y[ss2]
x_val = x[ss2,]

########################  Métodos ######################## 

###### PCA ###### 

pca = pcr(y_train~., data = x_train, scale = T, validation = "CV")
summary(pca)

predpca = predict(pca, x_val, ncomp = 1:93)    
msePCA = mean((y_val-predpca)^2)
msePCA

###### PLS ###### 

pls = plsr(data$V128~., data = data, scale = T, validation = "CV")
summary(pls)

predPLS = predict(pls, x_val, ncomp = 1:93)    

msePLS = mean((y_val-predPLS)^2)
msePLS 

###### Ridge ###### 

cvmodRIDGE = cv.glmnet(as.matrix(x), y, alpha = 0)
cvmodRIDGE$lambda.min
plot(cvmodRIDGE)
cvmodRIDGE$lambda.min #Lambda mínimo

mod_penRIDGE = glmnet(as.matrix(x), y, alpha = 0, lambda = cvmodRIDGE$lambda.min)
coef(mod_penRIDGE)

predRIDGE = predict(mod_penRIDGE, as.matrix(x_val))    
mseRIDGE = mean((y_val-predRIDGE)^2)
mseRIDGE 


###### Lasso #######

cvmodLASSO = cv.glmnet(as.matrix(x), y, alpha = 1)
cvmodLASSO$lambda.min
plot(cvmodLASSO)
cvmodLASSO$lambda.min #Lambda mínimo

mod_penLASSO = glmnet(as.matrix(x), y, alpha = 1, lambda = cvmodLASSO$lambda.min)
coef(mod_penLASSO)

predLASSO = predict(mod_penLASSO, as.matrix(x_val))    
mseLASSO = mean((y_val-predLASSO)^2)
mseLASSO

##### Comparación MSE #####

msePCA
msePLS
mseRIDGE
mseLASSO #El mejor

