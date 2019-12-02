# Punto 7
library(readr)
train <- read_csv("digit-recognizer/train.csv")
test <- read_csv("digit-recognizer/test.csv")

y_train = train$label
pix_train = train
pix_train$label = NULL

pix_test = test

#indices = c(18911,1529,14543,12525,30034,11761,14272,35858,27608,14367)

# 7.1 ----
indexes=which(y_train == 8)
index = indexes[runif(1,min=1, length(indexes))]
index = indices[2]
# recuperar imagen
pixi=matrix(rep(0,784),ncol=28)
for(i in 1:28){
  for(j in 1:28){
    pixi[j,i]=as.numeric(pix_train[index,(i-1)*28+j])
  }
}
pixi <- apply(pixi, 1, rev)
pixi = t(pixi)

x=seq(0,1,by=1/28)
yy=seq(0,1,by=1/28)
image(x, yy, pixi,col = terrain.colors(100)) # Mapa de color
y_train[index]




# 7.2 Principal components ----
S=var(pix_train)
library(corrplot)
M=cor(pix_train)
corrplot(M, method="circle")

pp=princomp(pix_train,scores=T)
summary(pp) # Matriz de varianza covarianza de componentes principales

#Grafico de codo
plot(pp$sdev^2,type="b",main="Varianzas de Componentes", xlim = c(0,20))


##############################################
#Images reconstruction
nn=dim(pix_train)[1]
p=dim(pix_train)[2]

#Solucion a primer omponente principal
w=eigen(S)$vectors

#New variable: First Component
medias = colMeans(pix_train)
pixx=as.matrix(pix_train - rep(medias, rep.int(nrow(pix_train), ncol(pix_train))))
Z=pixx%*%w

#Projections
#number of components
k=1
ZZ=Z%*%rbind(t(w)[1:k,],matrix(rep(0,(p-k)*p),ncol=p))
ZZ=as.matrix(ZZ+rep(medias, rep.int(nrow(pix_train), ncol(pix_train))))


#pixi=matrix(rep(0,784),ncol=28)
pixi2=matrix(rep(0,784),ncol=28)  # Imagen con componentes principales


index = indices[5]

for(i in 1:28){
  for(j in 1:28){
    #pixi[j,i]=as.numeric(pix_train[index,(i-1)*28+j])
    pixi2[j,i]=ZZ[index,(i-1)*28+j]
  }
}

#pixi <- apply(pixi, 1, rev)
#pixi = t(pixi)

pixi2 <- apply(pixi2, 1, rev)
pixi2 = t(pixi2)

x=seq(0,1,by=1/28)
yy=seq(0,1,by=1/28)

#image(x, yy, pixi,col = terrain.colors(100))
image(x, yy, pixi2,col = terrain.colors(100))


# 7.3 Clasificación ----
accuracy_final= vector()

for(k in 1:20){
  accuracy = rep(0,10)
  ZZ=Z%*%rbind(t(w)[1:k,],matrix(rep(0,(p-k)*p),ncol=p))
  ZZ=as.matrix(ZZ+rep(medias, rep.int(nrow(pix_train), ncol(pix_train))))
  
  folds <- cut(seq(1,nrow(ZZ)),breaks=10,labels=FALSE)
  
  for(i in 1:10){
    test_i <- which(folds==i,arr.ind=TRUE)
    x_test <- as.data.frame(ZZ[test_i, ])
    y_test = train$label[test_i]
    x_train = as.data.frame(ZZ[-test_i, ]) 
    y_train = train$label[-test_i]
    
    
    
    CARTnum = rpart(y_train ~ ., data=x_train, method="class")
    pred = predict(CARTnum, newdata=x_test, type="class")
    t = table(y_test, pred)
    diag = diag(t)
    accuracy[i] = sum(diag/nrow(x_test))
  }
  accuracy_final[k] = mean(accuracy)
}
plot(accuracy_final, type = 'l', xlab = 'Componentes', ylab = 'ACA', main = 'Average Classification Accuracy' ) 
points(accuracy_final)
k_opt = which.max(accuracy_final)
max(accuracy_final)


accuracy = rep(0,10)
ZZ=Z%*%rbind(t(w)[1:k_opt,],matrix(rep(0,(p-k_opt)*p),ncol=p))
ZZ=as.matrix(ZZ+rep(medias, rep.int(nrow(pix_train), ncol(pix_train))))

folds <- cut(seq(1,nrow(ZZ)),breaks=10,labels=FALSE)

for(i in 1:10){
  test_i <- which(folds==i,arr.ind=TRUE)
  x_test <- as.data.frame(ZZ[test_i, ])
  y_test = train$label[test_i]
  x_train = as.data.frame(ZZ[-test_i, ]) 
  y_train = train$label[-test_i]
  
  
  
  CARTnum = rpart(y_train ~ ., data=x_train, method="class")
  pred = predict(CARTnum, newdata=x_test, type="class")
  t = table(y_test, pred)
  diag = diag(t)
  accuracy[i] = sum(diag/nrow(x_test))
}
accuracy_final[k] = mean(accuracy)




# 7.5 PCA y KPCA ----
# PCA ----
kk=princomp(train,scores=T)

plot(kk$scores[,1:2],col=train$label, pch = 16, cex = 0.5)
legend(-2530, 1600, legend=c(0:9),
       col=c(1:10), lty=1, cex=0.8)
# Con PCA no se puede predecir bien

plot(kk$scores[,1:2],col=rainbow(10)[train$label], pch = 16, cex = 0.5)
legend(-2530, 1600, legend=c(0:9),
       col=rainbow(10)[1:10], lty=1, cex=0.8)

# 7.4 ----


# 7.5 PCA y KPCA ----
# PCA ----
kk=princomp(train,scores=T)

plot(kk$scores[,1:2],col=train$label, pch = 16, cex = 0.5)
legend(-2530, 1600, legend=c(0:9),
       col=c(1:10), lty=1, cex=0.8)
# Con PCA no se puede predecir bien
kk=princomp(train,scores=T)

kk$scores

library(RColorBrewer)
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))


plot(kk$scores[,1:2],col=col_vector[as.numeric(train$label)], pch = 16, xlim = c(-2500, 1800),cex = 0.5)
legend(1300, 1600, legend=c(0:9),
       col=col_vector[(1:10)], lty=1, cex=0.8)
# Con PCA no se puede predecir bien



plot(kk$scores[,1:2],col=as.numeric(train$label)+1, pch = 16, xlim = c(-2500, 1800),cex = 0.5)
legend(1300, 1600, legend=levels(train$label),
       col=c(1:10), lty=1, cex=0.8)
# Con PCA no se puede predecir bien

library(colorRamps)

train$label = as.factor(train$label)
plot(kk$scores[,1:2],col=rainbow(10)[as.numeric(train$label)+1], pch = 16, xlim = c(-2500, 1800),cex = 0.5)
legend(1300, 1600, legend=levels(train$label),
       col=rainbow(10)[1:10], lty=1, cex=0.8)


train$label = as.factor(train$label)
plot(kk$scores[,1:2],col=Set3(10)[as.numeric(train$label)+1], pch = 16, xlim = c(-2500, 1800),cex = 0.5)
legend(1300, 1600, legend=levels(train$label),
       col=Set3(10)[1:10], lty=1, cex=0.8)

# KPCA ----
library(kernlab)
library(caTools)

train_index = sample.split(train, SplitRatio = .25)
train2 = subset(train, train_index == TRUE)

kpc <- kpca(~.,data=as.data.frame(train2[,-train$label]),kernel = "rbfdot",
            kpar=list(sigma=1.1),features=2, eta=0.01, maxiter=10)
#print the principal component vectors
plot(pcv(kpc),col=train$label)

#plot the data projection on the components
plot(predict(kpc,as.data.frame(train)),col=train$label,
     xlab="1st Principal Component",ylab="2nd Principal Component")


