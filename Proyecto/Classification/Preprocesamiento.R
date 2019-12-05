library(readr)
library(Hmisc)
library(dummies)
library(corrplot)
library(cluster)

library(randomForest)
library(tree)
library(pROC)
library(gbm)
library(caret)
library(e1071)
library(mlr)
library(C50)

library(leaps)
library(pls)
library(matrixStats)

# Obtención ----
Train <- read_delim("Train.csv", ";", escape_double = FALSE,  trim_ws = TRUE)
Test <- read_delim("Test.csv", ";", escape_double = FALSE,  trim_ws = TRUE)

Train = as.data.frame(Train)
Train$Id = NULL

Test = as.data.frame(Test)
Test$Id = NULL

prep = rbind(Train,Test)
index = c(901:1065)


# Organizar datos ---- 

names_factor = c("Browser","TrafficType", "Region","VisitorType","Month", "OperatingSystems", "Revenue", "Weekend")
prep = Train
prep_fin = Train
prep_fin[names_factor] <- lapply(prep_fin[,names_factor], as.factor) 
prep_fin = as.data.frame(prep_fin)

Train[names_factor] <- lapply(prep_fin[,names_factor], as.factor) 
Train = as.data.frame(Train)


Test[names_factor] <- lapply(prep_fin[,names_factor], as.factor) 
Test = as.data.frame(Test)


# Exploración de los datos pre filtrado ----
# http://www.rdatamining.com/docs/data-exploration-and-visualization-with-r

# number of rows
nrow(prep)

# number of columns
ncol(prep)

# dimensionality
dim(prep)

# column names
names(prep)

summary(prep)

# describe check all columns
describe(prep[, 1])


summary(prep_fin)
str(prep_fin)


# Histogramas
hist(prep_fin$Administrative)
prep_fin$Administrative %>% density() %>% plot(main = 'Density')
table(prep_fin$Administrative)

hist(prep_fin$Administrative_Duration)

library(magrittr)


# Categóricas
# Browser
table(prep_fin$Browser)

library(dplyr)
prep_fin$Browser %>% table() %>% pie()
# add percentages
tab <- prep_fin$Browser %>% table()
precentages <- tab %>% prop.table() %>% round(3) * 100
txt <- paste0(names(tab), '\n', precentages, '%')
pie(tab, labels=txt)

# Multiple 
pairs(prep_fin)


# Filtrado
# Varianza Continuas ----
x = prep_fin
x[names_factor] = NULL
x$Revenue = NULL

x = scale(x, center = FALSE, scale = TRUE)

varianza = var(x)
varianza = diag(varianza)

plot(varianza,xaxt="n", ylim = c(0,1))
axis(1,at=1:10,labels=names(varianza))
min(varianza) # No se quita ninguna

# Varianza Categoricas ----
x = prep_fin[names_factor]
x$Revenue = NULL

barplot(prop.table(table(x[,1])), main = names_factor[1])
df = as.data.frame(cbind(x[,1],ifelse(Train$Revenue ==1,1,0)))
colnames(df) = c(names_factor[1],'Revenue')
ggplot(df, aes(x = Browser, y = Revenue, color = Revenue)) + 
  geom_point(position = position_dodge(width = 0.4))
t = table(df$Revenue,df$Browser)
prop = round(prop.table(t,1)*100,digits=0)
ones = prop[2,]
plot(ones)


barplot(prop.table(table(x[,2])), main = names_factor[2])
df = as.data.frame(cbind(x[,2],ifelse(Train$Revenue ==1,1,0)))
colnames(df) = c(names_factor[2],'Revenue')
ggplot(df, aes(x = TrafficType, y = Revenue, color = Revenue)) + 
  geom_point(position = position_dodge(width = 0.4))
t = table(df$Revenue,df$TrafficType)

prop = round(prop.table(t,1)*100,digits=0)
prop
ones = prop[2,]
plot(ones)


barplot(prop.table(table(x[,3])), main = names_factor[3])
df = as.data.frame(cbind(x[,3],ifelse(Train$Revenue ==1,1,0)))
colnames(df) = c(names_factor[3],'Revenue')
ggplot(df, aes(x = Region, y = Revenue, color = Revenue)) + 
  geom_point(position = position_dodge(width = 0.4))
t = table(df$Revenue,df$Region)

prop = round(prop.table(t,1)*100,digits=0)
prop
ones = prop[2,]
plot(ones)


barplot(prop.table(table(x[,4])), main = names_factor[4])
df = as.data.frame(cbind(x[,4],ifelse(Train$Revenue ==1,1,0)))
colnames(df) = c(names_factor[4],'Revenue')
ggplot(df, aes(x = VisitorType, y = Revenue, color = Revenue)) + 
  geom_point(position = position_dodge(width = 0.4))
t = table(df[,2],df[,1])
prop = round(prop.table(t,1)*100,digits=0)
prop
ones = prop[2,]
plot(ones)


barplot(prop.table(table(x[,5])), main = names_factor[5])
df = as.data.frame(cbind(x[,5],ifelse(Train$Revenue ==1,1,0)))
colnames(df) = c(names_factor[5],'Revenue')
ggplot(df, aes(x = Month, y = Revenue, color = Revenue)) + 
  geom_point(position = position_dodge(width = 0.4))
t = table(df[,2],df[,1])
prop = round(prop.table(t,1)*100,digits=0)
prop
ceros = prop[1,]
plot(ceros)

barplot(prop.table(table(x[,6])), main = names_factor[6])
df = as.data.frame(cbind(x[,6],ifelse(Train$Revenue ==1,1,0)))
colnames(df) = c(names_factor[6],'Revenue')
ggplot(df, aes(x = OperatingSystems, y = Revenue, color = Revenue)) + 
  geom_point(position = position_dodge(width = 0.4))
t = table(df[,2],df[,1])
prop = round(prop.table(t,1)*100,digits=0)
prop
ceros = prop[1,]
plot(ceros)


barplot(prop.table(table(x[,7])), main = names_factor[8])
df = as.data.frame(cbind(x[,7],ifelse(Train$Revenue ==1,1,0)))
colnames(df) = c(names_factor[8],'Revenue')
ggplot(df, aes(x = Weekend, y = Revenue, color = Revenue)) + 
  geom_point(position = position_dodge(width = 0.4))
t = table(df[,2],df[,1])
prop = round(prop.table(t,1)*100,digits=0)
prop
ceros = prop[1,]
plot(ceros)



# test_fin = prep_fin[901:1065,]
# train_fin = prep_fin[1:900,]

# Correlaciones para las variables no dummies
vc = cor(prep_fin[,1:10])
corrplot(vc,method="circle")

prep_xxs = scale(prep_fin, center=F) 

#DATA FINAL 

test_xxs = prep_fin[901:1065,]
train_xxs = prep_fin[1:900,]
test_xxs[test_xxs == "NaN"] = 0


# Outliers ----
library(DMwR)
library(tidyverse)
x = prep_fin
x[names_factor] = NULL
x = scale(x, center = FALSE, scale = TRUE)

outlier.scores <- lofactor(x, k=5) ## CUÁNTOS VECINOS USAR??

# pick top 5 as outliers
outliers <- order(outlier.scores, decreasing=T)[1:round(0.01*nrow(x))]
labels <- 1:length(outlier.scores)
labels[-outliers] <- "."
biplot(prcomp(x), cex=.8, xlabs=labels, na.rm = TRUE)
biplot(prcomp(x), cex=.8, xlabs=labels, na.rm = TRUE, xlim = c(-0.09,0.15), ylim = c(-0.08,0.09))

plot(prep_fin$BounceRates)


# who are outliers
print(outliers)
print(Train[outliers,])

Train = Train[-outliers,]


# Desbalance
table(Train$Revenue)

Train$Revenue = as.factor(Train$Revenue)
## now using SMOTE to create a more "balanced problem"
newData <- SMOTE(Revenue ~ ., Train, perc.over = 1200, perc.under = 100)
# perc.over: Qu? porcentaje se quiere hacer m?s de la chiquita
# perc.under: Qu? porcentaje se quiere hacer menos de la grande

################################### ver si sin round predice mejor !!!!!! ################


## Checking visually the created data
par(mfrow = c(1, 2))
plot(Train[, 1], Train[, 2], pch = 19 + as.integer(Train[, 3]),
     main = "Original Data")
plot(newData[, 1], newData[, 2], pch = 19 + as.integer(newData[,3]),
     main = "SMOTE'd Data")


table(newData$Revenue)

Train_final = newData

# names_dummies = c("Browser","TrafficType", "Region","VisitorType", "OperatingSystems", "Weekend")
# newData = dummy.data.frame(newData, names = names_dummies, dummy.classes = getOption("dummy.classes"))
# Test = dummy.data.frame(Test, names = names_dummies, dummy.classes = getOption("dummy.classes"))


##### Seleccion de variables #######

# Componentes principales
library(leaps)
pp = prcomp(newData[,-ncol(newData)],scale = TRUE)

plot(pp, type = "l")
plot(pp$sdev^2)

sum = summary(pp)[6]
importancia = sum$importance[3,]

plot(importancia,xaxt="n", ylim = c(0,1), type = 'l')
points(importancia)

plot(importancia[30:51],xaxt="n", type = 'l')
axis(1,at=1:22,labels=names(importancia[30:51]))
points(importancia[30:51])
importancia[41]
# Escogemos 41 componentes
# Explica el 98.75% de la varianza


zz = pp$x
train_comps = data.frame(cbind(zz,newData$Revenue))
columnas_solo_train = setdiff(colnames(newData), colnames(Test)) # newData, pero no test

columnas_solo_test = setdiff(colnames(Test),colnames(newData))
Test[columnas_solo_train] = 0
Test[columnas_solo_test] = NULL


x_Test = Test
x_Test$Revenue = NULL
scale_test = sweep(sweep(x_Test, MARGIN = 2, FUN = "-", STATS = pp$center), MARGIN = 2, FUN = "/", STATS = pp$scale)
scale_test = as.matrix(scale_test)
weights = matrix(cbind(pp$rotation[,1:41]), ncol = 41)

pp_test <- (scale_test %*% weights)

Train_final = data.frame(cbind(train_comps[,1:41],'Revenue' = newData$Revenue))


Test_final = pp_test



