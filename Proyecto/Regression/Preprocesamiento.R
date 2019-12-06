library(readr)
Train_FIFA <- read_delim("Train_FIFA.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)
Test_FIFA <- read_delim("Test_FIFA.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)

# Preprocesamiento

# Valores----
# Dejar Joined solo como año
library(stringr)
Train_FIFA$Joined = as.numeric(str_sub(Train_FIFA$Joined,-4,-1))
Test_FIFA$Joined = as.numeric(str_sub(Test_FIFA$Joined,-4,-1))  

# Dejar Contract Valid solo como año
Train_FIFA$`Contract Valid Until` = as.numeric(str_sub(Train_FIFA$`Contract Valid Until`,-4,-1))
Test_FIFA$`Contract Valid Until` = as.numeric(str_sub(Test_FIFA$`Contract Valid Until`,-4,-1))  

# Convertir height a metros para facilitar analisis
height <- function(x) {
  as.numeric(str_sub(x,1,1))*0.3048+as.numeric(strsplit(x,"'")[[1]][2])*0.0254
}

altura = sapply(Train_FIFA$Height, function(x) sapply(x, height))
Train_FIFA$Height = as.vector(altura)

altura = sapply(Test_FIFA$Height, function(x) sapply(x, height))
Test_FIFA$Height = as.vector(altura)


# Quitar lbs en peso
weight <- function(x) {
  sub(str_sub(x,-3,-1),'',x)
}

Train_FIFA$Weight = as.numeric(sapply(Train_FIFA$Weight, function(x) sapply(x, weight)))
Test_FIFA$Weight = as.numeric(sapply(Test_FIFA$Weight, function(x) sapply(x, weight)))

# Quitar +2
mas <- function(x) {
  as.numeric(str_split(x,'\\+')[[1]][1])
}

c = c("LS","ST","RS","LW","LF", "CF","RF","RW",
      "LAM","CAM","RAM","LM","LCM","CM","RCM","RM",
      "LWB","LDM","CDM","RDM","RWB","LB","LCB","CB","RCB","RB") 

Train_FIFA[c] = sapply(Train_FIFA[c], function(x) sapply(x, mas))
Test_FIFA[c] = sapply(Test_FIFA[c], function(x) sapply(x, mas))


# Release Clause
a = Train_FIFA$`Release Clause`
b = a

millionk <- function(x) {
  ifelse((str_sub(x,-1,-1) == 'M' ), 10^6*as.numeric(gsub("M", "", x)),
         ifelse((str_sub(x,-1,-1) == 'K'),    
                 10^3*as.numeric(gsub("K", "", x))))
}


Train_FIFA$`Release Clause` = as.vector(sapply(Train_FIFA$`Release Clause`, function(x) sapply(x, millionk)))
Test_FIFA$`Release Clause` = as.vector(sapply(Test_FIFA$`Release Clause`, function(x) sapply(x, millionk)))




# Tipos ----
# ID
Train_FIFA$ID = NULL
Test_FIFA$ID = NULL

# Age
sum(is.na(Train_FIFA[,1]))
plot(Train_FIFA$Age)
hist(Train_FIFA$Age)
plot(Train_FIFA$Age, Train_FIFA$Wage)


# Overall
sum(is.na(Train_FIFA[,2]))
plot(Train_FIFA$Overall)
hist(Train_FIFA$Overall)
plot(Train_FIFA$Overall, Train_FIFA$Wage)

# Potential
sum(is.na(Train_FIFA[,3]))
plot(Train_FIFA$Potential)
hist(Train_FIFA$Potential)
plot(Train_FIFA$Potential, Train_FIFA$Wage)

plot(Train_FIFA$Overall, Train_FIFA$Potential) # Quitar overall o potencial, alta correlacion


# Club
sum(is.na(Train_FIFA[,4]))
sum(is.na(Test_FIFA[,4]))

Train_FIFA[is.na(Train_FIFA[,4]),4] = 'No Club'
Test_FIFA[is.na(Test_FIFA[,4]),4] = 'No Club'

Train_FIFA$Club = as.factor(Train_FIFA$Club)
Test_FIFA$Club = as.factor(Test_FIFA$Club)

require(ggplot2)
df = as.data.frame(cbind(Train_FIFA$Club,Train_FIFA$Wage))
colnames(df) = c('Club','Wage')

plot(table(Train_FIFA$Club))

which.min(table(Train_FIFA$Club))
table(Train_FIFA$Club)[26]

ggplot(df, aes(x = Club, y = Wage, color = Wage)) + 
  geom_point(position = position_dodge(width = 0.4))

t = table(df[,2],df[,1])
prop = round(prop.table(t,1)*100,digits=0)
prop



# Preferred Foot
sum(is.na(Train_FIFA[,5]))
sum(is.na(Test_FIFA[,5]))

Train_FIFA$`Preferred Foot` = as.factor(Train_FIFA$`Preferred Foot`)
Test_FIFA$`Preferred Foot` = as.factor(Test_FIFA$`Preferred Foot`)


require(ggplot2)
df = as.data.frame(cbind(Train_FIFA$`Preferred Foot`,Train_FIFA$Wage))
colnames(df) = c('PreferredFoot','Wage')

plot(table(Train_FIFA$'Preferred Foot'))

barplot(prop.table(table(Train_FIFA$`Preferred Foot`)), main = names_factor[8])

ggplot(df, aes(x = PreferredFoot, y = Wage, color = Wage)) + 
  geom_point(position = position_dodge(width = 0.4))


# International Reputation
sum(is.na(Train_FIFA$`International Reputation`))
plot(Train_FIFA$`International Reputation`)
hist(Train_FIFA$`International Reputation`)
plot(Train_FIFA$`International Reputation`, Train_FIFA$Wage)


# Weak Foot
sum(is.na(Train_FIFA$`Weak Foot`))
plot(Train_FIFA$`Weak Foot`)
hist(Train_FIFA$`Weak Foot`)
plot(Train_FIFA$`Weak Foot`, Train_FIFA$Wage)


# Skill Moves
sum(is.na(Train_FIFA$`Skill Moves`))
plot(Train_FIFA$`Skill Moves`)
hist(Train_FIFA$`Skill Moves`)
plot(Train_FIFA$`Skill Moves`, Train_FIFA$Wage)


# Work Rate
sum(is.na(Train_FIFA$`Work Rate`))
sum(is.na(Test_FIFA$`Work Rate`))

Train_FIFA$`Work Rate` = as.factor(Train_FIFA$`Work Rate`)
Test_FIFA$`Work Rate` = as.factor(Test_FIFA$`Work Rate`)

require(ggplot2)
df = as.data.frame(cbind(Train_FIFA$`Work Rate`,Train_FIFA$Wage))
colnames(df) = c('WorkRate','Wage')

plot(table(Train_FIFA$`Work Rate`))

barplot(prop.table(table(Train_FIFA$`Work Rate`)), main = 'Work Rate')

ggplot(df, aes(x = WorkRate, y = Wage, color = Wage)) + 
  geom_point(position = position_dodge(width = 0.4))



# Body Type
sum(is.na(Train_FIFA$`Body Type`))
sum(is.na(Test_FIFA$`Body Type`))

Train_FIFA$`Body Type` = as.factor(Train_FIFA$`Body Type`)
Test_FIFA$`Body Type` = as.factor(Test_FIFA$`Body Type`)

require(ggplot2)
df = as.data.frame(cbind(Train_FIFA$`Body Type`,Train_FIFA$Wage))
colnames(df) = c('BodyType','Wage')

ggplot(df, aes(x = BodyType, y = Wage, color = Wage)) + 
  geom_point(position = position_dodge(width = 0.4)) +
  scale_x_continuous(breaks = c(1:5),
                     labels = levels(Train_FIFA$`Body Type`))

plot(table(Train_FIFA$`Body Type`))
barplot(prop.table(table(Train_FIFA$`Body Type`)), main = 'Body Type')

table(Test_FIFA$`Body Type`)

# De pronto quitar estos
Train_FIFA[which(Train_FIFA$`Body Type`== 'Shaqiri'),10] = 'Stocky'
Train_FIFA[which(Train_FIFA$`Body Type`== 'PLAYER_BODY_TYPE_25'),10] = 'Normal'

  


# Position
# Por alta correlacion!!!
posicion = Train_FIFA$Position
posicion = as.factor(posicion)
require(ggplot2)
df = as.data.frame(cbind(posicion,Train_FIFA$Wage))
colnames(df) = c('Position','Wage')

ggplot(df, aes(x = Position, y = Wage, color = Wage)) + 
  geom_point(position = position_dodge(width = 0.4)) +
  scale_x_continuous(breaks = c(1:27),
                     labels = levels(posicion))

plot(table(Train_FIFA$Position))
barplot(prop.table(table(Train_FIFA$Position)), main = 'Position')

table(Test_FIFA$Position)



Train_FIFA$Position <- ifelse(Train_FIFA$Position == "GK" , "GK",
                        ifelse(Train_FIFA$Position %in% c("RF", "ST", "LF", "RS", "LS", "CF"), "F",
                               ifelse(Train_FIFA$Position %in% c("LW", "RCM", "LCM", "LDM", "CAM", "CDM", "RM", "LAM", "LM", "RDM", "RW", "CM", "RAM"), "M","D")))
                                      #ifelse(Train_FIFA$Position %in% c("RCB", "CB", "LCB", "LB", "RB", "RWB","LWB"), "D",))))                  

Test_FIFA$Position <- ifelse(Test_FIFA$Position == "GK" , "GK",
                              ifelse(Test_FIFA$Position %in% c("RF", "ST", "LF", "RS", "LS", "CF"), "F",
                                     ifelse(Test_FIFA$Position %in% c("LW", "RCM", "LCM", "LDM", "CAM", "CDM", "RM", "LAM", "LM", "RDM", "RW", "CM", "RAM"), "M","D")))


sum(is.na(Train_FIFA$Position))
sum(is.na(Test_FIFA$Position))

Train_FIFA$Position = as.factor(Train_FIFA$Position)
Test_FIFA$Position = as.factor(Test_FIFA$Position)

require(ggplot2)
df = as.data.frame(cbind(Train_FIFA$Position,Train_FIFA$Wage))
colnames(df) = c('Position','Wage')

ggplot(df, aes(x = Position, y = Wage, color = Wage)) + 
  geom_point(position = position_dodge(width = 0.4)) +
  scale_x_continuous(breaks = c(1:4),
                     labels = levels(Train_FIFA$Position))

plot(table(Train_FIFA$Position))
barplot(prop.table(table(Train_FIFA$Position)), main = 'Position')

table(Test_FIFA$Position)


# Joined
sum(is.na(Train_FIFA$Joined))
sum(is.na(Test_FIFA$Joined))

Train_FIFA$Joined = as.factor(Train_FIFA$Joined)
Test_FIFA$Joined = as.factor(Test_FIFA$Joined)

require(ggplot2)
df = as.data.frame(cbind(Train_FIFA$Joined,Train_FIFA$Wage))
colnames(df) = c('Joined','Wage')

ggplot(df, aes(x = Joined, y = Wage, color = Wage)) + 
  geom_point(position = position_dodge(width = 0.4)) +
  scale_x_continuous(breaks = c(1:20),
                     labels = levels(Train_FIFA$Joined))

plot(table(Train_FIFA$Joined))
barplot(prop.table(table(Train_FIFA$Joined)), main = 'Joined')


# Joined From: QUITAR
sum(is.na(Train_FIFA$`Loaned From`))
sum(is.na(Test_FIFA$`Loaned From`))

Train_FIFA$`Loaned From` = as.factor(Train_FIFA$`Loaned From`)
Test_FIFA$`Loaned From` = as.factor(Test_FIFA$`Loaned From`)

require(ggplot2)
df = as.data.frame(cbind(Train_FIFA$`Loaned From`,Train_FIFA$Wage))
colnames(df) = c('LoanedFrom','Wage')

ggplot(df, aes(x = LoanedFrom, y = Wage, color = Wage)) + 
  geom_point(position = position_dodge(width = 0.4))
  #scale_x_continuous(breaks = c(1:20),
               #      labels = levels(Train_FIFA$`Loaned From`))

plot(table(Train_FIFA$`Loaned From`))


# Contract Valid 
# De pronto quitar
sum(is.na(Train_FIFA$`Contract Valid Until`))
sum(is.na(Test_FIFA$`Contract Valid Until`))

Train_FIFA$`Contract Valid Until` = as.factor(Train_FIFA$`Contract Valid Until`)
Test_FIFA$`Contract Valid Until` = as.factor(Test_FIFA$`Contract Valid Until`)

require(ggplot2)
df = as.data.frame(cbind(Train_FIFA$`Contract Valid Until`,Train_FIFA$Wage))
colnames(df) = c('ContractValidUntil','Wage')

ggplot(df, aes(x = ContractValidUntil, y = Wage, color = Wage)) + 
  geom_point(position = position_dodge(width = 0.4))+
scale_x_continuous(breaks = c(1:length(levels(Train_FIFA$`Contract Valid Until`))),
      labels = levels(Train_FIFA$`Contract Valid Until`))

plot(table(Train_FIFA$`Loaned From`))


# Height
sum(is.na(Train_FIFA$Height))
plot(Train_FIFA$Height)
hist(Train_FIFA$Height)
plot(Train_FIFA$Height, Train_FIFA$Wage)


# Weight
sum(is.na(Train_FIFA$Weight))
plot(Train_FIFA$Weight)
hist(Train_FIFA$Weight)
plot(Train_FIFA$Weight, Train_FIFA$Wage)


# Skills
skills = Train_FIFA[c]
#pairs(skills)

varianza = cor(na.omit(skills))
corrplot(varianza, type="upper", order="hclust", tl.col="black", tl.srt=45)

s1 = c("RCB", "CB", "LCB", "LB", "RB", "RWB","LWB", "RDM","LDM","CDM")
s2 = c("RCM","LCM","CM","RS","LS","ST","LW","RW","LM","RM","RF","LF","CF","RAM","LAM", "CAM")

Train_FIFA$Skills1 = rowMeans(Train_FIFA[s1], na.rm = TRUE)
Test_FIFA$Skills1 = rowMeans(Test_FIFA[s1], na.rm = TRUE)

Train_FIFA$Skills2 = rowMeans(Train_FIFA[s2], na.rm = TRUE)
Test_FIFA$Skills2 = rowMeans(Test_FIFA[s2], na.rm = TRUE)

Train_FIFA[s1] = NULL
Test_FIFA[s1] = NULL

Train_FIFA[s2] = NULL
Test_FIFA[s2] = NULL



g = c('GKDiving',	'GKHandling',	'GKKicking',	'GKPositioning',	'GKReflexes')
skillgk = Train_FIFA[g]
varianza = cor(na.omit(skillgk))
corrplot(varianza) # Se promedian

Train_FIFA$GK = rowMeans(Train_FIFA[g], na.rm = TRUE)
Test_FIFA$GK = rowMeans(Test_FIFA[g], na.rm = TRUE)

Train_FIFA[g] = NULL
Test_FIFA[g] = NULL 

#
s = c('Crossing',	'Finishing',	'HeadingAccuracy',	'ShortPassing',	'Volleys',	'Dribbling',	'Curve',	'FKAccuracy',	'LongPassing',
      'BallControl',	'Acceleration',	'SprintSpeed',	'Agility',	'Reactions',	'Balance',	'ShotPower',	'Jumping',	'Stamina',	
      'Strength',	'LongShots',	'Aggression',	'Interceptions',	'Positioning',	'Vision',	'Penalties',	'Composure',	'Marking',
      'StandingTackle',	'SlidingTackle')
otros_skills = Train_FIFA[s]
varianza = cor(na.omit(otros_skills))
corrplot(varianza, type="upper", order="hclust", tl.col="black", tl.srt=45)

s3 = c("Acceleration","SprintSpeed","Agility","Balance")
s4 = c("Penalties","ShotPower","LongShots","Volleys","Finishing",
       "Positioning","Vision","ShortPassing","LongPassing",
       "Curve","FKAccuracy","Crossing","Dribbling","BallControl")
s5 = c("Aggression","Marking","Interceptions","StandingTackle","SlidingTackle")

Train_FIFA$Skills3 = rowMeans(Train_FIFA[s3], na.rm = TRUE)
Test_FIFA$Skills3 = rowMeans(Test_FIFA[s3], na.rm = TRUE)

Train_FIFA[s3] = NULL
Test_FIFA[s3] = NULL 


Train_FIFA$Skills4 = rowMeans(Train_FIFA[s4], na.rm = TRUE)
Test_FIFA$Skills4 = rowMeans(Test_FIFA[s4], na.rm = TRUE)

Train_FIFA[s4] = NULL
Test_FIFA[s4] = NULL 


Train_FIFA$Skills5 = rowMeans(Train_FIFA[s5], na.rm = TRUE)
Test_FIFA$Skills5 = rowMeans(Test_FIFA[s5], na.rm = TRUE)

Train_FIFA[s5] = NULL
Test_FIFA[s5] = NULL 




# Correlacion ----
factor_names = names(Filter(is.factor, Train_FIFA))
numeric_names = names(Filter(is.numeric, Train_FIFA))

numericas = Train_FIFA[numeric_names]
varianza = cor(na.omit(numericas))

corrplot(varianza, type="upper", order="hclust", tl.col="black", tl.srt=45)


s15 = c("Skills1","Skills5")

Train_FIFA$Skills15 = rowMeans(Train_FIFA[s15], na.rm = TRUE)
Test_FIFA$Skills15 = rowMeans(Test_FIFA[s15], na.rm = TRUE)

Train_FIFA[s15] = NULL
Test_FIFA[s15] = NULL 


s24 = c("Skills2","Skills4")

Train_FIFA$Skills24 = rowMeans(Train_FIFA[s24], na.rm = TRUE)
Test_FIFA$Skills24 = rowMeans(Test_FIFA[s24], na.rm = TRUE)

Train_FIFA[s24] = NULL
Test_FIFA[s24] = NULL 


# Quitar ----
numeric_names = names(Filter(is.numeric, Train_FIFA))

numericas = Train_FIFA[numeric_names]
numericas$Wage = NULL
varianza = cor(na.omit(numericas))

corrplot(varianza, type="upper", order="hclust", tl.col="black", tl.srt=45)

#pairs(numericas)

# Quitar:
# Weight o Height: Dejar Weight
# Overall y Potential: Crear OverallPotential con promedios
# Joined From
# Contract Valid

Train = Train_FIFA
Test = Test_FIFA
op = c("Overall","Potential")
Train$OverallPotential = rowMeans(Train_FIFA[op])
Train[op] = NULL
Test$OverallPotential = rowMeans(Test[op])
Test[op] = NULL


Train$Height = NULL
Train$`Contract Valid Until`= NULL

Test$Height = NULL
Test$`Contract Valid Until`= NULL

# Missing ----

# 3. Analisis NAs -----
summary(Train)
sum(complete.cases(Train))
sum(!complete.cases(Train))

mean(!complete.cases(Train)) # Porcentaje faltantes


# Patrones
library(data.table)
library(VIM)

matrixplot(Train, interactive = F)
Train$`Loaned From` = NULL
Test$`Loaned From` = NULL

sum(complete.cases(Train))
matrixplot(Train, interactive = T)

aggr(Train,prop=F,numbers=T)

# Quitar los 26 con faltantes
v = Train[is.nan(Train$GK),] 
Train = Train[!is.nan(Train$GK),] 
aggr(Train,prop=F,numbers=T)

# Joined, Release Clause, Position
marginplot(Train[c("Joined","Release Clause")],alpha = 0.8, 
           col = c(1:3))

marginplot(Train[c("Position","Joined")],alpha = 0.8, 
           col = c(1:3))

marginplot(Train[c("Position","Release Clause")],alpha = 0.8, 
           col = c(1:3))


Train$`Release Clause` = NULL
Train$Joined = NULL

Test$`Release Clause` = NULL
Test$Joined = NULL


aggr(Train,prop=F,numbers=T)
Train = na.omit(Train)  

# Outliers ----
library(DMwR)
library(tidyverse)
x = Train
x$Wage = NULL
x[factor_names] = NULL
x = scale(x, center = FALSE, scale = TRUE)

outlier.scores <- lofactor(x, k=5) ## CUÁNTOS VECINOS USAR??

# pick top 5 as outliers
outliers <- order(outlier.scores, decreasing=T)[1:round(0.003*nrow(x))]
labels <- 1:length(outlier.scores)
labels[-outliers] <- "."
biplot(prcomp(x), cex=.8, xlabs=labels, na.rm = TRUE)
biplot(prcomp(x), cex=.8, xlabs=labels, na.rm = TRUE, xlim = c(-0.08,0.04), ylim = c(-0.02,0.1))

# who are outliers
print(outliers)
print(Train[outliers,])

Train = Train[-outliers,]


################ MUCHAS PRUEBAS PARA QUE SIRVIERAN LOS DATOS #############



train = Train
test = Test

fn = names(Filter(is.factor, Train))

train = sapply(train, as.numeric)
train = as.data.frame(train)

train$Club = as.factor(train$Club)
train$`Preferred Foot` = as.factor(train$`Preferred Foot`)
train$`Body Type` = as.factor(train$`Body Type`)
train$`Work Rate` = as.factor(train$`Work Rate`)
train$Position = as.factor(train$Position)

test = sapply(test, as.numeric)
test = as.data.frame(test)

test$Club = as.factor(test$Club)
test$`Preferred Foot` = as.factor(test$`Preferred Foot`)
test$`Body Type` = as.factor(test$`Body Type`)
test$`Work Rate` = as.factor(test$`Work Rate`)
test$Position = as.factor(test$Position)

todos = rbind(train,test)

todos$Club = as.factor(todos$Club)
todos$`Preferred Foot` = as.factor(todos$`Preferred Foot`)
todos$`Body Type` = as.factor(todos$`Body Type`)
todos$`Work Rate` = as.factor(todos$`Work Rate`)
todos$Position = as.factor(todos$Position)

todos$Wage = NULL

library(dummies)

new_todos <- dummy.data.frame(todos, sep = ".")
names(new_todos)

x_train = new_todos[1:7947,]
x_test = new_todos[7948:9947,]

xxx = test
xxx$Wage = NULL
sum(is.na(train))
sum(is.na(test))
aggr(xxx,prop=F,numbers=T)
