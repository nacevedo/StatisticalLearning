library(ISLR)

# Creación de datos

?Hitters
data = Hitters
dim(data)
head(data)
index = sample(1:dim(data)[1],64)
test = data[index,]
train = data[-index,]

# Generalized Additive Models GAMS (automático)
library(gam)

gams.fit <- gam(Salary~s(CRuns)+s(CWalks), data=data)

# Resumen de gams
summary(gams.fit)

# Plot de CWalks vs s(CWalks)
plot(gams.fit)

# Plot de cada una de las variables
par(mfrow=c(1,2))
plot(gams.fit,se=T,col=4,lwd=2)

# Plot de los datos a predecir (Salary)
par(mfrow=c(1,1))
plot(Hitters$Salary)




