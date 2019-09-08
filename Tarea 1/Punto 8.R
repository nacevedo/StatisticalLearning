library(psych)
library(corrplot)

## Leer el archivo

data = read.table("communities.data", sep = ",")
head(data)

#Quitar columnas y datos nulos (correr sólo una vez)

s = seq(97,116)
data = data[,c(-1,-2,-3,-4,-5,-s,-117,-118,-119,-120,-122,-123,-124,-125,-127)]
data = na.omit(data)

r = describeBy(data)

#nombramiento de las variables

#varible a predecir: V128 (posición 94 del arreglo)

#creación de train y test

x = as.data.frame(data[,-94])
y = data$V128

N = length(data$V6)
ss=seq(1:N)
ss=sample(ss,N,replace=F)
ss1=ss[1:1593]
ss2=ss[1594:N]

y_train=y[ss1]
x_train=x[ss1,]
y_test=y[ss2]
x_test=x[ss2,]

#Métodos

