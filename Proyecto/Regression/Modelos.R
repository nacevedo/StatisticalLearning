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

predpca = predict(pca, test[,-15], ncomp = 1:19)    #cambiar el 50

df = cbind(index,predpca)
colnames(df) = c("Id","Predicted")

write.csv(df,file="submission_pca.csv", row.names = F)

#msePCA = mean((y_val-predpca)^2)
#msePCA
#validationplot(pca)
#validationplot(pca, val.type="MSEP")
#predplot(pca)
#plot(y_val,predpca,  xlab = 'measured', ylab = 'predicted', main = 'PCA')


###### PLS ###### ----

train_psl = train
train_psl$GK = NULL
train_psl$WeakFoot = NULL
train_psl$PreferredFoot = NULL
train_psl$Jumping = NULL

test_psl = test
test_psl$GK = NULL
test_psl$WeakFoot = NULL
test_psl$PreferredFoot = NULL
test_psl$Jumping = NULL
test_psl$Wage = NULL

#quitamos los foot porque no importan 
#quitamos jumping ya está en heads
#quitamos skills 3 son caracteristicas del GK 

aggr(test_psl,prop=F,numbers=T)

names = colnames(test_psl)
names = names[-1]
names = names[-18]

library(mice)
library(VIM)
library(dplyr)
library(tibble)
library(magrittr)

init = mice(test_psl, maxit=0) 
meth = init$method
predM = init$predictorMatrix
imputed = mice(test_psl, method=meth, predictorMatrix=predM, m=1)
imp = complete(imputed)

imp$Imputado_imp = is.na(test_psl$Position)
head(imp)

dt1 = test_psl %>% 
   dplyr::select(Age,InternationalReputation,SkillMoves,
                 WorkRate,Position,Weight,HeadingAccuracy,Reactions,
                 Stamina,Strength,Composure,Skills3,Skills15,Skills24,OverallPotential) %>% 
   rename(Position_imp = Position) %>% 
   mutate(
     Position_imp = as.logical(ifelse(is.na(Position_imp), "TRUE", "FALSE"))
   ) %>% 
   rownames_to_column()
 
 dt2 = imp %>% 
   dplyr::select(Age,InternationalReputation,SkillMoves,
                 WorkRate,Position,Weight,HeadingAccuracy,Reactions,
                 Stamina,Strength,Composure,Skills3,Skills15,Skills24,OverallPotential)  %>% 
   rownames_to_column()
 
 test_psl = left_join(dt1, dt2)
 head(test_psl)
 
 
 vars <- c(names[1],"Position", "Position_imp")
 marginplot(test_psl[,vars], delimiter="_imp", alpha=0.6, pch=c(19))
 
 ## Quitar Position: NMAR
 ### ----
 
 train_psl = train
 train_psl$GK = NULL
 train_psl$WeakFoot = NULL
 train_psl$PreferredFoot = NULL
 train_psl$Jumping = NULL
 
 test_psl = test
 test_psl$GK = NULL
 test_psl$WeakFoot = NULL
 test_psl$PreferredFoot = NULL
 test_psl$Jumping = NULL
 test_psl$Wage = NULL
 
 train_psl$Position = NULL
 test_psl$Position = NULL
 
 init = mice(test_psl, maxit=0) 
 meth = init$method
 predM = init$predictorMatrix
 imputed = mice(test_psl, method=meth, predictorMatrix=predM, m=1)
 imp = complete(imputed)
 
 dt1 = test_psl %>% 
   dplyr::select(Age,InternationalReputation,SkillMoves,
                 WorkRate,Weight,HeadingAccuracy,Reactions,
                 Stamina,Strength,Composure,Skills3,Skills15,Skills24,OverallPotential) %>% 
   rename(InternationalReputation_imp = InternationalReputation) %>% 
   mutate(
     InternationalReputation_imp = as.logical(ifelse(is.na(InternationalReputation_imp), "TRUE", "FALSE"))
   ) %>% 
   rownames_to_column()
 
 dt2 = imp %>% 
   dplyr::select(Age,InternationalReputation,SkillMoves,
                 WorkRate,Weight,HeadingAccuracy,Reactions,
                 Stamina,Strength,Composure,Skills3,Skills15,Skills24,OverallPotential)  %>% 
   rownames_to_column()
 
 test_psl = left_join(dt1, dt2)
 head(test_psl)
 names = colnames(test_psl)
 
 vars <- c(names[2],"InternationalReputation", "InternationalReputation_imp")
 marginplot(test_psl[,vars], delimiter="_imp", alpha=0.6, pch=c(19))
 
 # Se dejan
 
 matrixplot(imp, interactive = F)
 aggr(imp,prop=F,numbers=T)
 

 # ----
 library(pls)
train_psl$Wage = NULL
pls = plsr(train$Wage ~., data = train_psl, scale = T, validation = "CV")
 summary(pls)
 plot(RMSEP(pls))
 plot(RMSEP(pls), xlim = c(0,5))
 
sum(is.na(test_psl$InternationalReputation))
test_psl$InternationalReputation[is.na(test_psl$InternationalReputation)] = imp$InternationalReputation[is.na(test_psl$InternationalReputation)]
test_psl$SkillMoves[is.na(test_psl$SkillMoves)] = imp$SkillMoves[is.na(test_psl$SkillMoves)]
test_psl$Weight[is.na(test_psl$Weight)] = imp$Weight[is.na(test_psl$Weight)]
test_psl$HeadingAccuracy[is.na(test_psl$HeadingAccuracy)] = imp$HeadingAccuracy[is.na(test_psl$HeadingAccuracy)]
test_psl$Reactions[is.na(test_psl$Reactions)] = imp$Reactions[is.na(test_psl$Reactions)]
test_psl$Stamina[is.na(test_psl$Stamina)] = imp$Stamina[is.na(test_psl$Stamina)]
test_psl$Strength[is.na(test_psl$Strength)] = imp$Strength[is.na(test_psl$Strength)]
test_psl$Composure[is.na(test_psl$Composure)] = imp$Composure[is.na(test_psl$Composure)]
test_psl$Skills3[is.na(test_psl$Skills3)] = imp$Skills3[is.na(test_psl$Skills3)]
test_psl$Skills15[is.na(test_psl$Skills15)] = imp$Skills15[is.na(test_psl$Skills15)]
test_psl$Skills24[is.na(test_psl$Skills24)] = imp$Skills24[is.na(test_psl$Skills24)]
test_psl$OverallPotential[is.na(test_psl$OverallPotential)] = imp$OverallPotential[is.na(test_psl$OverallPotential)]
test_psl$WorkRate[is.na(test_psl$WorkRate)] = imp$WorkRate[is.na(test_psl$WorkRate)]
test_psl$rowname = NULL
test_psl$InternationalReputation_imp = NULL

predPLS = predict(pls, test_psl)
predPLS = predPLS[,1,]

a = pls$scores

predPLS$

sum(is.na(predPLS))

df = cbind(index,predPLS)
df = as.data.frame(df)
colnames(df) = c("Id","Predicted")
df$Predicted[df$Id == 8813] = 2000
df$Predicted[df$Id == 8917] = 2000
df$Predicted[df$Id == 9517] = 1000
df$Predicted[df$Id == 9841] = 1000
df$Predicted[df$Id == 8216] = 0
df$Predicted[df$Id == 9336] = 0

sum(is.na(df))
sum(df$Predicted<0)

write.csv(df,file="submission_pls.csv", row.names = F)

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

todos = sapply(todos, as.numeric)
todos = as.data.frame(todos)

todos$PreferredFoot = as.factor(todos$PreferredFoot)
todos$WorkRate = as.factor(todos$WorkRate)
todos$Position = as.factor(todos$Position)

train = todos[1:18127,]
test = todos[18128:20127,]

y = as.data.frame(train[,15])
y = as.numeric(y[[1]])

x1 = train[,1:14] 
x2 = as.numeric(train[,16][[1]])
x3 = as.numeric(train[,17][[1]])
x4 = as.numeric(train[,18][[1]])
x5 = as.numeric(train[,19][[1]])
x6 = as.numeric(train[,20][[1]])

names = colnames(train)
names = names[-15]

x = as.data.frame(cbind(x1,x2,x3,x4,x5,x6))
colnames(x) = names

library(dplyr)
x = x %>% mutate_if(is.character,as.factor)

mars.fit <- mars(x, y)

# SHOW CUT POINTS OF MARS
cuts <- mars.fit$cuts[mars.fit$selected.terms, ];
dimnames(cuts) <- list(NULL, names(x)); #cambiar 14
print(cuts);

factor <- mars.fit$factor[mars.fit$selected.terms, ];
dimnames(factor) <- list(NULL, names(x)); #cambiar 14
print(factor);

# EXAMINE THE FITTED FUNCTION BETWEEN EACH IV AND DV
par(mfrow = c(3, 5), mar=c(2, 2, 2, 2), pty="s")
for (i in 1:19) #cambiar 50 a num variables creo
{
  xp <- matrix(sapply(train[1:13], mean), nrow(train), ncol(train) - 1, byrow = TRUE); #cambiar 13 a num var.
  xr <- sapply(train, range);
  xp[, i] <- seq(xr[1, i], xr[2, i], len=nrow(train));
  xf <- predict(mars.fit, xp);
  plot(xp[, i], xf, xlab = names(train)[i], ylab = "mdev pred", type = "l");
}


mars.fit$gcv

mars.fit$coefficients


x1 = as.numeric(test[,1])
x2 = as.numeric(test[,2])
x3 = as.numeric(test[,3])
x4 = as.numeric(test[,4])
x5 = as.numeric(test[,5])
x6 = as.numeric(test[,6])
x7 = as.numeric(test[,7])
x8 = as.numeric(test[,8])
x9 = as.numeric(test[,9])
x10 = as.numeric(test[,10])
x11 = as.numeric(test[,11])
x12 = as.numeric(test[,12])
x13 = as.numeric(test[,13])
x14 = as.numeric(test[,14])
x15 = as.numeric(test[,16])
x16 = as.numeric(test[,17])
x17 = as.numeric(test[,18])
x18 = as.numeric(test[,19])
x19 = as.numeric(test[,20])

y = as.data.frame(cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19))
colnames(y) = names


predMARS= predict(mars.fit, y) # cambiar 50 

sum(is.na(predMARS))

df = cbind(index,predMARS)
df = as.data.frame(df)
colnames(df) = c("Id","Predicted")
df$Predicted[df$Id == 8813] = 2000
df$Predicted[df$Id == 8917] = 2000
df$Predicted[df$Id == 9517] = 1000
df$Predicted[df$Id == 9841] = 1000
df$Predicted[df$Id == 8216] = 0
df$Predicted[df$Id == 9336] = 0

sum(is.na(df))
sum(df$Predicted<0)

write.csv(df,file="submission_mars.csv", row.names = F)


###### GAMS #######

train$WeakFoot = as.factor(train$WeakFoot)
train$InternationalReputation = as.factor(train$InternationalReputation)
train$SkillMoves = as.factor(train$SkillMoves)

gams.fit <- gam(Wage~s(Age)+(Position)+(PreferredFoot)
                +(InternationalReputation)+(WeakFoot)+(WorkRate)
                +(SkillMoves)+s(Weight)+s(HeadingAccuracy)+s(Reactions)
                +s(Jumping)+s(Stamina)+s(Strength)+s(Composure)+s(GK)+s(Skills3)
                +s(Skills15)+s(Skills24)+s(OverallPotential),data=train)

par(mfrow=c(3,4))
summary(gams.fit)
plot.gam(gams.fit)

#quitamos los foot porque no importan 
#quitamos jumping ya está en heads
#quitamos skills 3 son caracteristicas del GK 

gams.fit <- gam(Wage~s(Age)+(Position)
                +(InternationalReputation)+(WorkRate)
                +(SkillMoves)+s(Weight)+s(HeadingAccuracy)+s(Reactions)
                +s(Stamina)+s(Strength)+s(Composure)+s(GK)
                +s(Skills15)+s(Skills24)+s(OverallPotential),data=train)


par(mfrow=c(3,4))
summary(gams.fit)
plot.gam(gams.fit)

plot(gams.fit,se=T,lwd=2)

par(mfrow=c(1,1))

predGAMS= predict(gams.fit, test[,-15]) 

df = cbind(index,predGAMS)
df = as.data.frame(df)
colnames(df) = c("Id","Predicted")
df$Predicted[df$Id == 8813] = 2000
df$Predicted[df$Id == 8917] = 2000
df$Predicted[df$Id == 9517] = 1000
df$Predicted[df$Id == 9841] = 1000
df$Predicted[df$Id == 8216] = 0
df$Predicted[df$Id == 9336] = 0

sum(is.na(df))
sum(df$Predicted<0)

write.csv(df,file="submission_gams2.csv", row.names = F)

#mseGAMS= mean((y_val-predGAMS)^2)

#plot(y_val,predGAMS,  xlab = 'measured', ylab = 'predicted', main = 'GAMS')


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


df = cbind(index,predLASSO)
colnames(df) = c("Id","Predicted")

write.csv(df,file="submission_l.csv", row.names = F)