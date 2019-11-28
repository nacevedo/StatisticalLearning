# Punto 5
# 1. Datos ------
data=read.table('reprocessed.hungarian.data',sep=' ')
data = data[-295,] 
names(data) = c("age","sex", "cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")

# 2. Valores perdidos
data[data == -9] = NA
data[data == 9] = NA


# 3. Analisis NAs -----
sum(is.na(data$age)) # Datos faltantes de age: 0
sum(is.na(data$sex)) # Datos faltantes de sex: 0
sum(is.na(data$cp)) # Datos faltantes de cp: 0
sum(is.na(data$trestbps)) # Datos faltantes de trestbps: 1
sum(is.na(data$chol)) # Datos faltantes de chol: 23
sum(is.na(data$fbs)) # Datos faltantes de fbs: 8
sum(is.na(data$restecg)) # Datos faltantes de restecg: 1
sum(is.na(data$thalach)) # Datos faltantes de thalach: 1
sum(is.na(data$exang)) # Datos faltantes de exang: 1
sum(is.na(data$oldpeak)) # Datos faltantes de oldpeak: 0
sum(is.na(data$slope)) # Datos faltantes de slope: 190
sum(is.na(data$ca)) # Datos faltantes de ca: 291
sum(is.na(data$thal)) # Datos faltantes de thal: 266
sum(is.na(data$num)) # Datos faltantes de num: 0

sum(complete.cases(data))
sum(!complete.cases(data))
mean(!complete.cases(data)) # Porcentaje faltantes
mean(!complete.cases(data$ca)) # Porcentaje faltan ca

# Patrones
library(data.table)
instal("VIM", dependencies = TRUE)
matrixplot(data, interactive = F)
aggr(data,prop=F,numbers=T)
# slope, ca ,thal
# ca, thal son NMAR
# El resto son missing at random
# chol, slope, ca ,thal



marginplot(data[c("thal","ca")],alpha = 0.8, 
           col = c(1:3))

marginplot(data[c("thal","slope")],alpha = 0.8, 
           col = c(1:3))

marginplot(data[c("ca","slope")],col = c(1:3))

marginplot(data[c("ca","chol")],col = c(1:3))


# Observacion
which.max(rowSums(is.na(data)))
(data)[290,] # trestbps, thalach, exang, ca, thal


# 4. Imputacion Multiple ----------
# Estime e interprete un modelo de regersión lineal, donde la variable dependiente es la máxima
# frecuencia cardiaca alcanazada (thalach) en la octava columna y las variables independientes
# son todas las demás excepto las correspondientes a las últimas dos columnas (thal y num). Use
# imputación múltiple para asigna valores perdidos.

# Imputacion Multiple
library(mice)
thalach = data$thalach
x = data[,1:12]
x = x[,-8]
x = x[,-11]
data2 = cbind(x,thalach)
x = as.matrix(x)
# PODEMOS QUITAR CA????

imputed <- mice(data2,seed=1220)
summary(imputed)

filled <- complete(imputed)
head(filled)

fit=with(imputed,lm(thalach~x))

pooled=pool(fit)
summary(pooled)



# 5. Quitar filas
data3 = data
data3$ca  = NULL
sinfilas = na.omit(data3)

thalach = sinfilas$thalach
x = sinfilas[,1:12]
x = x[,-8]
x = x[,-11]
x$fbs = NULL
# NO CORRE PORQUE ES UNA SOLA FILA

fit2 = lm(thalach ~ ., data = x)
summary(fit2)
