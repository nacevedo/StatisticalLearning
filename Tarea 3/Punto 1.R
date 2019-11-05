# Punto 1
library(e1071)
library(sfsmisc)

make.grid = function(x, n = 20) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}

data <- read.csv("arboles.txt", sep="")
data$forest = as.factor(data$forest)
x = data.matrix(data[,-3], rownames.force = NA)
y = ifelse(data$forest == 2, -1, 1)

dat = data.frame(x, y = as.factor(y))
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)

print(svmfit)

xgrid = make.grid(x)
xgrid[1:10,]
colnames(xgrid) = colnames(x)

beta = drop(t(svmfit$coefs)%*%x[svmfit$index,])
beta0 = svmfit$rho

ygrid = predict(svmfit, xgrid)

plot(xgrid, col = c("blue", "red")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = -y + 3, pch = 1)

abline(beta0 / beta[2], -beta[1] / beta[2])
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)

beta0 / beta[2]

dist1 = (beta0 - 1) / beta[2] - beta0 / beta[2]
dist2 = (beta0 + 1) / beta[2] - beta0 / beta[2]
ancho1 = 2*dist1

### cortar árboles
#circular árboles a cortar
points(x[svmfit$index,], pch = 1, cex = 2)

dat1 = dat[-svmfit$index,]

x = data.matrix(dat1[,-3], rownames.force = NA)
y = ifelse(dat1$y == -1, -1, 1)

svmfit = svm(y ~ ., data = dat1, kernel = "linear", cost = 10, scale = FALSE)

print(svmfit)

xgrid = make.grid(x)
xgrid[1:10,]
colnames(xgrid) = colnames(x)

beta = drop(t(svmfit$coefs)%*%x[svmfit$index,])
beta0 = svmfit$rho

ygrid = predict(svmfit, xgrid)

plot(xgrid, col = c("blue", "red")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = -y + 3, pch = 1)

abline(beta0 / beta[2], -beta[1] / beta[2])
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)

dist1 = (beta0 - 1) / beta[2] - beta0 / beta[2]
dist2 = (beta0 + 1) / beta[2] - beta0 / beta[2]
ancho2 = 2*dist1

diferencia = ancho2 - ancho1

