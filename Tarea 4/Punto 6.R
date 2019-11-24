# Punto 6
library(readxl)
CTG <- read_excel("CTG.xls", sheet = "Raw Data")

data = CTG[-1,]
library(tidyverse)
data = data %>% select(LB,AC,FM,UC,DL,DS,DP,ASTV,MSTV,ALTV,MLTV,Width,Min,Max,Nmax,Nzeros,Mode,Mean,Median,Variance,Tendency)
