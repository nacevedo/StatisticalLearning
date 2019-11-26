# Punto 6
library(readxl)
CTG <- read_excel("CTG.xls", sheet = "Raw Data")

data = CTG[-1,]
library(tidyverse)
data = data %>% select(LB,AC,FM,UC,ASTV,MSTV,ALTV)


# 1.Datos atipicos mvoutlier -------
library(mvoutlier)
data$FM = NULL
data$ALTV = NULL
## ESTÁ BIEN QUITAR ESTOS???


uni.plot(as.matrix(data), na.omit = TRUE) # Ver outliers como se comportan en cada variable
# Lo hace sacando la distancia mahanalobis
# Va a poner c?digo con labels

graph = uni.plot(as.matrix(data), symb = TRUE)
# Variables: AC, UC, MSTV



# 2.Datos atipicos LOF -------
library(DMwR)
data = CTG[-1,]
library(tidyverse)
data = data %>% select(LB,AC,FM,UC,ASTV,MSTV,ALTV)

outlier.scores <- lofactor(data, k=5) ## CUÁNTOS VECINOS USAR??
plot(density(outlier.scores))

# pick top 5 as outliers
outliers <- order(outlier.scores, decreasing=T)[1:5]
labels <- 1:n
labels[-outliers] <- "."
biplot(prcomp(data), cex=.8, xlabs=labels)
# who are outliers
print(outliers)
print(data[outliers,])

n <- nrow(data)
pch <- rep(".", n)
pch[outliers] <- "+"
col <- rep("black", n)
col[outliers] <- "red"
pairs(data, pch=pch, col=col)

# Variable principal es FM