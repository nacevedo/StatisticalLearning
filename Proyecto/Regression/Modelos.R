### Modelos

library(MASS)
library(psych)
library(corrplot)
library(leaps)
library(pls)
library(glmnet)

library(caret)
library(nnet)

library(mgcv)
library(mda)

index = seq(8001,10000,1)
###### PCA ###### 

pca = pcr(Wage~., data = train, scale = T, validation = "CV")
summary(pca)

predpca = predict(pca, test[,-22], ncomp = 1:21)    #cambiar el 50

#msePCA = mean((y_val-predpca)^2)
#msePCA
#validationplot(pca)
#validationplot(pca, val.type="MSEP")
#predplot(pca)
#plot(y_val,predpca,  xlab = 'measured', ylab = 'predicted', main = 'PCA')


###### PLS ###### 

pls = plsr(Wage ~., data = train, scale = T, validation = "CV")

summary(pls)

predPLS = predict(pls, test[,-17]) #cambiar 50    

#msePLS = mean((y_val-predPLS)^2)
#msePLS 
#plot(RMSEP(pls))
#predplot(pls)

###### Ridge ###### 

cvmodRIDGE = cv.glmnet(as.matrix(x_train), train$Wage, alpha = 0) #cambiar
cvmodRIDGE$lambda.min
plot(cvmodRIDGE)
cvmodRIDGE$lambda.min #Lambda m?nimo

mod_penRIDGE = glmnet(as.matrix(x_train), train$Wage, alpha = 0, lambda = cvmodRIDGE$lambda.min) #cambiar 50
coef(mod_penRIDGE)

predRIDGE = predict(mod_penRIDGE, as.matrix(x_test)) #cambiar 50  


df = cbind(index,predRIDGE)
colnames(df) = c("Id","Predicted")

write.csv(df,file="submission_r.csv", row.names = F)

#mseRIDGE = mean((y_val-predRIDGE)^2)
#mseRIDGE 
#plot(y_val,predRIDGE,  xlab = 'measured', ylab = 'predicted', main = 'Ridge Penalization')


###### Lasso #######

cvmodLASSO = cv.glmnet(as.matrix(x_train), train$Wage, alpha = 1)
cvmodLASSO$lambda.min
plot(cvmodLASSO)
cvmodLASSO$lambda.min #Lambda mínimo

mod_penLASSO = glmnet(as.matrix(x_train), train$Wage, alpha = 1, lambda = cvmodLASSO$lambda.min)
coef(mod_penLASSO)

predLASSO = predict(mod_penLASSO, as.matrix(x_test))  

df = cbind(index,predLASSO)
colnames(df) = c("Id","Predicted")

write.csv(df,file="submission_l.csv", row.names = F)

#plot(y_val,predLASSO,  xlab = 'measured', ylab = 'predicted', main = 'Lasso Penalization')

#mseLASSO = mean((y_val-predLASSO)^2)
#mseLASSO


##### Comparación MSE #####

#msePCA
#msePLS
#mseRIDGE
#mseLASSO #El mejor

#x = c("PCA", "PLS", "Ridge", "Lasso")
#y = c(msePCA, msePLS, mseRIDGE, mseLASSO)
#library(lattice)
#dotplot(y~x, ylab = 'MSE', cex = 2, main = 'Comparaci?n MSEs')

###### MARS #######

# FIT AN ADDITIVE MARS MODEL
mars.fit <- mars(train[, -16], train$Wage, degree = 1, prune = TRUE, forward.step = TRUE) #cambiar 14

# SHOW CUT POINTS OF MARS
cuts <- mars.fit$cuts[mars.fit$selected.terms, ];
dimnames(cuts) <- list(NULL, names(train)[-16]); #cambiar 14
print(cuts);

factor <- mars.fit$factor[mars.fit$selected.terms, ];
dimnames(factor) <- list(NULL, names(train)[-14]); #cambiar 14
print(factor);

# EXAMINE THE FITTED FUNCTION BETWEEN EACH IV AND DV
par(mfrow = c(3, 5), mar=c(2, 2, 2, 2), pty="s")
for (i in 1:50) #cambiar 50 a num variables creo
{
  xp <- matrix(sapply(train[1:13], mean), nrow(train), ncol(train) - 1, byrow = TRUE); #cambiar 13 a num var.
  xr <- sapply(train, range);
  xp[, i] <- seq(xr[1, i], xr[2, i], len=nrow(train));
  xf <- predict(mars.fit, xp);
  plot(xp[, i], xf, xlab = names(train)[i], ylab = "mdev pred", type = "l");
}


mars.fit$gcv

mars.fit$coefficients

predMARS= predict(mars.fit, as.matrix(test[,-50])) # cambiar 50 
#mseMARS= mean((y_val-predMARS)^2)

#plot(y_val,predMARS,  xlab = 'measured', ylab = 'predicted', main = 'MARS')



#x = c("PCA", "PLS", "Ridge", "Lasso", "MARS")
#y = c(msePCA, msePLS, mseRIDGE, mseLASSO, mseMARS)
#library(lattice)
#dotplot(y~x, ylab = 'MSE', cex = 2, main = 'Comparaci?n MSEs')


###### GAMS #######

gams.fit <- gam(medv~s(crim)+s(rm)+s(age)+s(dis)+s(tax)+s(ptratio)+s(lstat)+s(zn)+s(nox)+s(indus)+s(black)+(chas)+(rad),data=train)


par(mfrow=c(3,4))
summary(gams.fit)
plot.gam(gams.fit)



plot(gams.fit,se=T,lwd=2)

par(mfrow=c(1,1))
plot(Boston$crim)

predGAMS= predict(gams.fit, x_val) 
mseGAMS= mean((y_val-predGAMS)^2)

plot(y_val,predGAMS,  xlab = 'measured', ylab = 'predicted', main = 'GAMS')


x = c("PCA", "PLS", "Ridge", "Lasso", "MARS", "GAMS")
y = c(msePCA, msePLS, mseRIDGE, mseLASSO, mseMARS, mseGAMS)
library(lattice)
dotplot(y~x, ylab = 'MSE', cex = 2, main = 'Comparaci?n MSEs')



### Prueba de hip?tesis ###
full =  gam(medv~s(crim)+s(rm)+s(age)+s(dis)+s(tax)
            +s(ptratio)+s(lstat)
            +s(zn)+s(nox)+s(indus)+s(black)+(chas)+(rad),data=train)


red = gam(medv~crim+s(rm)+s(age)+s(dis)+s(tax)+s(ptratio)+s(lstat)+s(zn)+s(nox)+s(indus)+s(black),data=train)

anova(red,full,test="F")


