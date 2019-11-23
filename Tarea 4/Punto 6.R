# Punto 6

library(readxl)
CTG <- read_excel("CTG.xls", sheet = "Raw Data")

data = CTG[-1,]
