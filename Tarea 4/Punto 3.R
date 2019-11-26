# Punto 3

library(readr)
library(cluster)
puntos <- read_table2("puntos.txt", col_names = FALSE)

costo_dist <- function(x1,x2,y1,y2){ 
  dist = abs(x1-x2) + abs(y1-y2)
  res = 10 * dist^2
  return(res) 
}

###################################### Literal 1 #####################################

#####################################################################
#K MEDIAS : 3 sitios

KM = kmeans(puntos,3)

#funcion el libreria cluster
clusplot(puntos,KM$cluster)  #la misma grafica
plot(puntos)
points(KM$centers, col="red", pch = 2, cex = 2)

centros3 = KM$centers

###################################### Literal 2 #####################################

costo_total = 0

for (i in 1:3){
  
  x_cd = centros3[i,1]
  y_cd = centros3[i,2]
  
  for (j in 1:80){
    
    if(KM$cluster[j] == i){
      x_tienda = puntos$X1[j]
      y_tienda = puntos$X2[j]
      costo_total = costo_total + costo_dist(x_cd,x_tienda,y_cd,y_tienda)
    }
  }
}

costo_total


###################################### Literal 3 #####################################

#####################################################################
#K MEDIAS : 2 sitios

KM = kmeans(puntos,2)

#funcion el libreria cluster
clusplot(puntos,KM$cluster)  #la misma grafica
plot(puntos)
points(KM$centers,col="red", pch = 2, cex = 2)

centros2 = KM$centers

costo_total2 = 0

for (i in 1:2){
  
  x_cd2 = centros2[i,1]
  y_cd2 = centros2[i,2]
  
  for (j in 1:80){
    
    if(KM$cluster[j] == i){
      x_tienda2 = puntos$X1[j]
      y_tienda2 = puntos$X2[j]
      costo_total2 = costo_total2 + costo_dist(x_cd2,x_tienda2,y_cd2,y_tienda2)
    }
  }
}

costo_total2

###################################### Literal 4 #####################################

#Number of means (K)
costo_cds <- rep(0,times=8)

for (k in 1:8){
  
  ct = 0
  
  km_temp = kmeans(puntos, centers = k)
  
  for (i in 1:k){
    
    x_cd = km_temp$centers[i,1]
    y_cd = km_temp$centers[i,2]
    
    for (j in 1:80){
      
      if(km_temp$cluster[j] == i){
        x_tienda = puntos$X1[j]
        y_tienda = puntos$X2[j]
        ct = ct + costo_dist(x_cd,x_tienda,y_cd,y_tienda)
      }
    }
  }
  
  
  costo_cds[i] = ct + 500 * k
  
} 

plot(1:8, costo_cds, type="b", xlab="Number of Clusters", ylab="Cost")  
min(costo_cds)
which.min(costo_cds)

