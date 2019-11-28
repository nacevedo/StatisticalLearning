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
library(factoextra)

KM = kmeans(puntos,3)

#funcion el libreria cluster
clusplot(puntos,KM$cluster, main = "Grupos de puntos de venta")  #la misma grafica
plot(puntos, main = "Puntos de venta y bodegas")
points(KM$centers, col="red", pch = 2, cex = 2)
fviz_cluster(KM, puntos)



###################################### Literal 2 #####################################
costos = rep(0,nrow(puntos))
costos_totales = rep(0,nrow(puntos))
costo_total = 0
costos_bodegas = rep(0, 3)
costo_bodega = 0

for (i in 1:3){
  
  x_cd = centros3[i,1]
  y_cd = centros3[i,2]
  
  for (j in 1:80){
    
    if(KM$cluster[j] == i){
      x_tienda = puntos$X1[j]
      y_tienda = puntos$X2[j]
      costo_total = costo_total + costo_dist(x_cd,x_tienda,y_cd,y_tienda)
      costos[j] = costo_dist(x_cd,x_tienda,y_cd,y_tienda)
      costos_totales[j] = costo_total
      costo_bodega = costo_bodega + costo_dist(x_cd,x_tienda,y_cd,y_tienda)
    }
  }
  costos_bodegas[i] = costo_bodega
  costo_bodega = 0
}

costo_total

plot(costos, pch = 16, col = KM$cluster+1, ylab = "Costo", xlab = "Punto de Venta", main = 'Costo por punto de venta', yaxt="n")
legend(0, 80, legend=c("Bodega 1", "Bodega 2", "Bodega 3"),
       col=c(2,3,4), lty=1, cex=0.8)
axis(2, at=axTicks(2), labels=sprintf("$%s", axTicks(2)), las=1)


###################################### Literal 3 #####################################

#####################################################################
#K MEDIAS : 2 sitios

KM = kmeans(puntos,2)


clusplot(puntos,KM$cluster, main = "Grupos de puntos de venta")  #la misma grafica
plot(puntos,main = "Puntos de venta y bodegas")
points(KM$centers, col="red", pch = 2, cex = 2)
fviz_cluster(KM, puntos )

centros2 = KM$centers

costo_total2 = 0
costos2 = rep(0,nrow(puntos))

for (i in 1:2){
  
  x_cd2 = centros2[i,1]
  y_cd2 = centros2[i,2]
  
  for (j in 1:80){
    
    if(KM$cluster[j] == i){
      x_tienda2 = puntos$X1[j]
      y_tienda2 = puntos$X2[j]
      costo_total2 = costo_total2 + costo_dist(x_cd2,x_tienda2,y_cd2,y_tienda2)
      costos2[j] = costo_dist(x_cd,x_tienda,y_cd,y_tienda)
    }
  }
}

costo_total2
plot(costos, pch = 16, col = KM$cluster+1, ylab = "Costo", xlab = "Punto de Venta", main = 'Costo por punto de venta', yaxt="n")
legend(0, 80, legend=c("Bodega 1", "Bodega 2"),
       col=c(2,3), lty=1, cex=0.8)
axis(2, at=axTicks(2), labels=sprintf("$%s", axTicks(2)), las=1)


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

plot(1:8, costo_cds, type="b", ylab="Costo",yaxt="n", main = "Costo total", xlab = "Número de bodegas")  
axis(2, at=axTicks(2), labels=sprintf("$%s", axTicks(2)), las=1)
min(costo_cds)
which.min(costo_cds)




KM = kmeans(puntos,4)


clusplot(puntos,KM$cluster, main = "Grupos de puntos de venta")  #la misma grafica
plot(puntos,main = "Puntos de venta y bodegas")
points(KM$centers, col="red", pch = 2, cex = 2)
fviz_cluster(KM, puntos )

centros2 = KM$centers

costo_total2 = 0
costos2 = rep(0,nrow(puntos))

for (i in 1:2){
  
  x_cd2 = centros2[i,1]
  y_cd2 = centros2[i,2]
  
  for (j in 1:80){
    
    if(KM$cluster[j] == i){
      x_tienda2 = puntos$X1[j]
      y_tienda2 = puntos$X2[j]
      costo_total2 = costo_total2 + costo_dist(x_cd2,x_tienda2,y_cd2,y_tienda2)
      costos2[j] = costo_dist(x_cd,x_tienda,y_cd,y_tienda)
    }
  }
}

costo_total2+500*4
plot(costos, pch = 16, col = KM$cluster+1, ylab = "Costo", xlab = "Punto de Venta", main = 'Costo por punto de venta', yaxt="n")
legend(0, 80, legend=c("Bodega 1", "Bodega 2", "Bodega 3", "Bodega 4"),
       col=c(2,3,4,5), lty=1, cex=0.8)
axis(2, at=axTicks(2), labels=sprintf("$%s", axTicks(2)), las=1)

