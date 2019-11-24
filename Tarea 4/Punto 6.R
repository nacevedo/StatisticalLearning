# Punto 6
library(readxl)
CTG <- read_excel("CTG.xls", sheet = "Raw Data")

data = CTG[-1,]
library(tidyverse)
data = data %>% select(LB,AC,FM,UC,DL,DS,DP,ASTV,MSTV,ALTV,MLTV,Width,Min,Max,Nmax,Nzeros,Mode,Mean,Median,Variance,Tendency)

# 1.Datos atipicos mvoutlier -------
library(mvoutlier)
colorData <- color.plot(data, quan=1/2, alpha=0.025)
chisq.plot(data) # QQ Plot. Quitar outliers manualmente
uni.plot(data) # Ver outliers como se comportan en cada variable
# Lo hace sacando la distancia mahanalobis
# Va a poner c?digo con labels

# 2.Datos atipicos LOF -------