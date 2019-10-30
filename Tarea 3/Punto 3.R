# Punto 3
Punto3 <- function(x, y){
  # Fit
  fit=glm(y~.,data=x,family = "binomial")
  summary(fit)
  
  # Numero de variables
  k = dim(x)[2]
  
  #Numero de datos
  n = dim(x)[1]
  
  filas = 0
  
  # Crear matriz de combinaciones posibles
  for (i in 1:k){
    library(combinat)
    combi= ncol(combn(k,i))
    if (is.null(combi)){
      combi = 1
    }
    filas=filas+combi
  }
  matriz = matrix(0,filas,3)
  
  
  j = 1
  for (i in 1:k){
    combinacion=combn(k,i)
    # Cuantas combinaciones posibles?
    columnas = ncol(combinacion)
    
    # Si es solo 1 combinacion va a ser la de todas las variables
    if (is.null(columnas)){
      columnas = 1
      matriz[j,1] = paste(combinacion, collapse = " ")
      fit = glm(y~.,data=x,family = "binomial")
      matriz[j,2] = fit$aic
      
      extractAIC(fit, show.option=TRUE)
      options(AIC="BIC")
      matriz[j,3] = extractAIC(fit)[2]
      
      j = j+1
      
      
    } else{
      # Numero de combinaiones
      fil = nrow(combinacion)
      for (l in 1:columnas){
        if (is.null(fil) || fil == 1) {
          matriz[j,1] = paste(combinacion[,l], collapse = " ")
          fit = glm(y~x[,combinacion[l]],family = "binomial")
          matriz[j,2] = fit$aic
          
          extractAIC(fit, show.option=TRUE)
          options(AIC="BIC")
          matriz[j,3] = extractAIC(fit)[2]
          
          j = j+1
          
        } else{
          valores = c(rep(0,fil))
          xnew = matrix(0L,nrow=nrow(x), ncol=fil)
          xnew = as.data.frame(xnew)
          
          for (m in 1:fil){
            valores[m] = combinacion[m,l]
            xnew[,m] = x[,valores[m]]
            
          }
          fit = glm(y~.,data=xnew,family = "binomial")
          matriz[j,1] = paste(combinacion[,l], collapse = " ")
          matriz[j,2] = fit$aic
          
          extractAIC(fit, show.option=TRUE)
          options(AIC="BIC")
          matriz[j,3] = extractAIC(fit)[2]
          
          j = j+1
        }
      }
    }
  }
  
  valores = as.data.frame(matriz)
  colnames(valores) <- c("Variables","AIC", "BIC") 
  
  return(valores)
  return(minAIC)
  return(BIC)
  
  # Se usa el del AIC porque el deviance tiene mucha varianza
  minAIC = valores[which.min(valores$AIC),]
  minBIC = valores[which.min(valores$BIC),]
}


# b
library(ISLR)
library(tree)
library(pROC)

car=Carseats
attach(car)
High=ifelse(Sales<mean(Sales),0,1)
detach(car)

car = car[,-1]

Punto3(car,High)
# 1 = CompPrice
# 2 = Income
# 3 = Advertising
# 4 = Population
# 5 = Price
# 6 = ShelveLoc
# 7 = Age
# 8 = Education
# 9 = Urban
# 10 = US

# c Metodo forward
lr=glm(High~.,data=car,family="binomial")
summary(lr)

null_model=glm(High~1,data=car,family=binomial)
summary(null_model)

forward=step(null_model,scope=list(lower=formula(null_model),
                                   upper=formula(lr)),direction="forward")
# Deja ShelveLoc, Price, CompPrice, Advertising, Age, Income, US
# 6, 5, 1, 3, 7, 2, 10
# AIC = 204.61


fit_forward=glm(formula(forward),data=car,family=binomial)
summary(fit_forward)

# Da igual esta vez
