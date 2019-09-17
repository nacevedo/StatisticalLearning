library(psych)
library(corrplot)
library(leaps)
library(pls)
library(glmnet)

## Leer el archivo
data = read.table("communities.data", sep = ",")
colnames(data) = c(
  "state",
  "county",
  "community",
  "communityname",
  "fold",
  "population",
  "householdsize",
  "racepctblack",
  "racePctWhite",
  "racePctAsian",
  "racePctHisp",
  "agePct12t21",
  "agePct12t29",
  "agePct16t24",
  "agePct65up",
  "numbUrban",
  "pctUrban",
  "medIncome",
  "pctWWage",
  "pctWFarmSelf",
  "pctWInvInc",
  "pctWSocSec",
  "pctWPubAsst",
  "pctWRetire",
  "medFamInc",
  "perCapInc",
  "whitePerCap",
  "blackPerCap",
  "indianPerCap",
  "AsianPerCap",
  "OtherPerCap",
  "HispPerCap",
  "NumUnderPov",
  "PctPopUnderPov",
  "PctLess9thGrade",
  "PctNotHSGrad",
  "PctBSorMore",
  "PctUnemployed",
  "PctEmploy",
  "PctEmplManu",
  "PctEmplProfServ",
  "PctOccupManu",
  "PctOccupMgmtProf",
  "MalePctDivorce",
  "MalePctNevMarr",
  "FemalePctDiv",
  "TotalPctDiv",
  "PersPerFam",
  "PctFam2Par",
  "PctKids2Par",
  "PctYoungKids2Par",
  "PctTeen2Par",
  "PctWorkMomYoungKids",
  "PctWorkMom",
  "NumIlleg",
  "PctIlleg",
  "NumImmig",
  "PctImmigRecent",
  "PctImmigRec5",
  "PctImmigRec8",
  "PctImmigRec10",
  "PctRecentImmig",
  "PctRecImmig5",
  "PctRecImmig8",
  "PctRecImmig10",
  "PctSpeakEnglOnly",
  "PctNotSpeakEnglWell",
  "PctLargHouseFam",
  "PctLargHouseOccup",
  "PersPerOccupHous",
  "PersPerOwnOccHous",
  "PersPerRentOccHous",
  "PctPersOwnOccup",
  "PctPersDenseHous",
  "PctHousLess3BR",
  "MedNumBR",
  "HousVacant",
  "PctHousOccup",
  "PctHousOwnOcc",
  "PctVacantBoarded",
  "PctVacMore6Mos",
  "MedYrHousBuilt",
  "PctHousNoPhone",
  "PctWOFullPlumb",
  "OwnOccLowQuart",
  "OwnOccMedVal",
  "OwnOccHiQuart",
  "RentLowQ",
  "RentMedian",
  "RentHighQ",
  "MedRent",
  "MedRentPctHousInc",
  "MedOwnCostPctInc",
  "MedOwnCostPctIncNoMtg",
  "NumInShelters",
  "NumStreet",
  "PctForeignBorn",
  "PctBornSameState",
  "PctSameHouse85",
  "PctSameCity85",
  "PctSameState85",
  "LemasSwornFT",
  "LemasSwFTPerPop",
  "LemasSwFTFieldOps",
  "LemasSwFTFieldPerPop",
  "LemasTotalReq",
  "LemasTotReqPerPop",
  "PolicReqPerOffic",
  "PolicPerPop",
  "RacialMatchCommPol",
  "PctPolicWhite",
  "PctPolicBlack",
  "PctPolicHisp",
  "PctPolicAsian",
  "PctPolicMinor",
  "OfficAssgnDrugUnits",
  "NumKindsDrugsSeiz",
  "PolicAveOTWorked",
  "LandArea",
  "PopDens",
  "PctUsePubTrans",
  "PolicCars",
  "PolicOperBudg",
  "LemasPctPolicOnPatr",
  "LemasGangUnitDeploy",
  "LemasPctOfficDrugUn",
  "PolicBudgPerPop",
  "ViolentCrimesPerPop")
head(data)

#Quitar columnas y datos nulos (correr sólo una vez)
s = seq(97,116) # Crear secuencia de valores
data = data[,c(-1,-2,-3,-4,-5,-s,-117,-118,-119,-120,-122,-123,-124,-125,-127)]
data = na.omit(data)

r = describeBy(data) # Descripción de datos

#varible a predecir: V128 (posición 94 del arreglo)

#creación de train y test

x = as.data.frame(data[,-94])
y = data$ViolentCrimesPerPop

N = length(data$population)
ss = seq(1:N)
ss = sample(ss,N,replace=F)
ss1 = ss[1:1593]
ss2 = ss[1594:N]

y_train = y[ss1]
x_train = x[ss1,]
y_val = y[ss2]
x_val = x[ss2,]

########################  Métodos ######################## 

###### PCA ###### 

pca = pcr(y_train~., data = x_train, scale = T, validation = "CV")
summary(pca)

predpca = predict(pca, x_val)    
msePCA = mean((y_val-predpca)^2)
msePCA


###### PLS ###### 

<<<<<<< HEAD
pls = plsr(y_train~., data = x_train, scale = T, validation = "CV")
=======
pls = plsr(data$ViolentCrimesPerPop~., data = data, scale = T, validation = "CV")
>>>>>>> 8510417b4117a1ade3dcb77081dba0f774d34188
summary(pls)

predPLS = predict(pls, x_val, ncomp = 1:93)    

msePLS = mean((y_val-predPLS)^2)
msePLS 

###### Ridge ###### 

cvmodRIDGE = cv.glmnet(as.matrix(x), y, alpha = 0)
cvmodRIDGE$lambda.min
plot(cvmodRIDGE)
cvmodRIDGE$lambda.min #Lambda mínimo

mod_penRIDGE = glmnet(as.matrix(x), y, alpha = 0, lambda = cvmodRIDGE$lambda.min)
coef(mod_penRIDGE)

predRIDGE = predict(mod_penRIDGE, as.matrix(x_val))    
mseRIDGE = mean((y_val-predRIDGE)^2)
mseRIDGE 


###### Lasso #######

cvmodLASSO = cv.glmnet(as.matrix(x), y, alpha = 1)
cvmodLASSO$lambda.min
plot(cvmodLASSO)
cvmodLASSO$lambda.min #Lambda mínimo

mod_penLASSO = glmnet(as.matrix(x), y, alpha = 1, lambda = cvmodLASSO$lambda.min)
coef(mod_penLASSO)

predLASSO = predict(mod_penLASSO, as.matrix(x_val))    
mseLASSO = mean((y_val-predLASSO)^2)
mseLASSO


##### Comparación MSE #####

msePCA
msePLS
mseRIDGE
mseLASSO #El mejor

<<<<<<< HEAD
x = c("PCA", "PLS", "Ridge", "Lasso")
y = c(0.02052191, 0.01793674, 0.0182319, 0.01768556)
library(lattice)
dotplot(y~x, ylab = 'MSE', cex = 2)
=======
##### Variables Significativas con LASSO #####

betas = as.matrix(coef(mod_penLASSO))
betas = as.data.frame(betas)
betas[betas >= 0.0001 | betas <= -0.0001] #valor de los betas significativos
row.names(betas)[betas >= 0.0001 | betas <= -0.0001] #nombre de las variables de los betas significativos


>>>>>>> 8510417b4117a1ade3dcb77081dba0f774d34188

