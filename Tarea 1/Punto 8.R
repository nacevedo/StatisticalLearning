library(psych)
library(corrplot)

## Leer el archivo

data = read.table("communities.data", sep = ",")
head(data)

#Quitar columnas y datos nulos (correr sólo una vez)

s = seq(97,116)
data = data[,c(-1,-2,-3,-4,-5,-s,-117,-118,-119,-120,-122,-123,-124,-125,-127)]
data = na.omit(data)

r =describeBy(data)

#nombramiento de las variables

#varible a predecir: V128 (posición 94 del arreglo)


