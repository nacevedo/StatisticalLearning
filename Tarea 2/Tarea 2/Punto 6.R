library(MASS)

#Creación de datos

?Boston
data=Boston
dim(data)
head(data)
index=sample(1:dim(data)[1],150)
test=data[index,]
train=data[-index,]

# 1. Encuentre el modelo lineal que mejor predice la variable medv (Lasso, PCA, PLS, etc)


# 2. Estime un modelo MARS para predecir Sales

# 3. Estime un modelo GAM para predecir Sales