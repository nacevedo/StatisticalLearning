library(MASS)
library(psych)
library(corrplot)
library(leaps)
library(pls)
library(glmnet)

library(MASS)
library(caret)
library(nnet)

library(mgcv)
library(MASS)
library(mda)

#Creación de datos

?Boston
data=Boston
dim(data)
head(data)
index=sample(1:dim(data)[1],150)
test=data[index,]
train=data[-index,]



y_train = train$medv
x_train = train[,-14]

y_val = test$medv
x_val = test[,-14]




# 1. Encuentre el modelo lineal que mejor predice la variable medv (Lasso, PCA, PLS, etc)

###### PCA ###### 

pca = pcr(y_train~., data = x_train, scale = T, validation = "CV")
summary(pca)

predpca = predict(pca, x_val, ncomp = 1:12)    
msePCA = mean((y_val-predpca)^2)
msePCA
validationplot(pca)
validationplot(pca, val.type="MSEP")
predplot(pca)

###### PLS ###### 

pls = plsr(train$medv ~., data = train, scale = T, validation = "CV")

summary(pls)

predPLS = predict(pls, x_val)    

msePLS = mean((y_val-predPLS)^2)
msePLS 
plot(RMSEP(pls))
predplot(pls)

###### Ridge ###### 

cvmodRIDGE = cv.glmnet(as.matrix(x_train), y_train, alpha = 0)
cvmodRIDGE$lambda.min
plot(cvmodRIDGE)
cvmodRIDGE$lambda.min #Lambda mínimo

mod_penRIDGE = glmnet(as.matrix(x_train), y_train, alpha = 0, lambda = cvmodRIDGE$lambda.min)
coef(mod_penRIDGE)

predRIDGE = predict(mod_penRIDGE, as.matrix(x_val))    
mseRIDGE = mean((y_val-predRIDGE)^2)
mseRIDGE 
plot(y_val,predRIDGE,  xlab = 'measured', ylab = 'predicted', main = 'Ridge Penalization')


###### Lasso #######

cvmodLASSO = cv.glmnet(as.matrix(x_train), y_train, alpha = 1)
cvmodLASSO$lambda.min
plot(cvmodLASSO)
cvmodLASSO$lambda.min #Lambda mínimo

mod_penLASSO = glmnet(as.matrix(x_train), y_train, alpha = 1, lambda = cvmodLASSO$lambda.min)
coef(mod_penLASSO)

predLASSO = predict(mod_penLASSO, as.matrix(x_val))  
plot(y_val,predLASSO,  xlab = 'measured', ylab = 'predicted', main = 'Lasso Penalization')

mseLASSO = mean((y_val-predLASSO)^2)
mseLASSO


##### Comparación MSE #####

msePCA
msePLS
mseRIDGE
mseLASSO #El mejor

x = c("PCA", "PLS", "Ridge", "Lasso")
y = c(msePCA, msePLS, mseRIDGE, mseLASSO)
library(lattice)
dotplot(y~x, ylab = 'MSE', cex = 2, main = 'Comparaci�n MSEs')

# 2. Estime un modelo MARS para predecir Sales

# FIT AN ADDITIVE MARS MODEL
mars.fit <- mars(train[, -14], train[14], degree = 1, prune = TRUE, forward.step = TRUE)

# SHOW CUT POINTS OF MARS
cuts <- mars.fit$cuts[mars.fit$selected.terms, ];
dimnames(cuts) <- list(NULL, names(Boston)[-14]);
print(cuts);

factor <- mars.fit$factor[mars.fit$selected.terms, ];
dimnames(factor) <- list(NULL, names(Boston)[-14]);
print(factor);

# EXAMINE THE FITTED FUNCTION BETWEEN EACH IV AND DV
par(mfrow = c(3, 5), mar=c(2, 2, 2, 2), pty="s")
for (i in 1:13)
{
  xp <- matrix(sapply(Boston[1:13], mean), nrow(Boston), ncol(Boston) - 1, byrow = TRUE);
  xr <- sapply(Boston, range);
  xp[, i] <- seq(xr[1, i], xr[2, i], len=nrow(Boston));
  xf <- predict(mars.fit, xp);
  plot(xp[, i], xf, xlab = names(Boston)[i], ylab = "mdev pred", type = "l");
}


mars.fit$gcv

mars.fit$coefficients

predMARS= predict(mars.fit, as.matrix(x_val)) 
mseMARS= mean((y_val-predMARS)^2)


# 3. Estime un modelo GAM para predecir Sales

gams.fit <- gam(medv~s(crim)+s(rm)+s(age)+s(dis)+s(tax)+s(ptratio)+s(lstat)+zn+s(nox)+s(indus)+s(black),data=train)


summary(gams.fit)
plot.gam(gams.fit)


par(mfrow=c(3,4))
plot(gams.fit,se=T,col=4,lwd=2)

par(mfrow=c(1,1))
plot(Boston$crim)

predGAMS= predict(gams.fit, x_val) 
mseGAMS= mean((y_val-predGAMS)^2)

gams.fit2=gam(medv~crim+s(rm)+s(dis)+s(tax)
              +s(ptratio)+s(lstat),data=Boston)
par(mfrow=c(2,4))
plot(gams.fit2,se=T,col=4,lwd=2)

anova(gams.fit,gams.fit2,test="Chi")
anova(gams.fit2,gams.fit,test="F")




################################################
#Generalized Additive Models GAMS
#Case: Poisson regression
#Taken form: IDRE, UCLA

p <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", 
                                            "Vocational"))
  id <- factor(id)
})
summary(p)


poi=gam(num_awards ~ prog + bs(math,c(5,10,25)), family="poisson", data=p)
summary(poi)
anova(poi)
plot(poi,all.terms=T)
gam.check(poi)

