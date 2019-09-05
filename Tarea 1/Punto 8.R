library(psych)

## Leer el archivo

data = read.table("communities.data", sep = ",")
head(data)

data = data[,c(-1,-2,-3,-4,-5)]

data = na.omit(data)
head(data)

describeBy(data)
is.na(data)
