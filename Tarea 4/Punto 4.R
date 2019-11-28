# Punto 4
comidas = c("Bandeja paisa", "Ensalada César",
"Arroz de coco y pescado", "Ensalada de Atún",
"Paella de Mariscos", "Ajiaco",
"Sancocho de Costilla", "Ceviche de Corvina",
"Ajiaco Santafereño", "Raviolis de Ricotta",
"Quesadilla", "Pizza Napolitana")

puntaje = matrix(0,length(comidas), length(comidas))
puntaje[1,] = c(0,90,50,70,80,50,50,80,50,90,80,90)
puntaje[2,] = c(0,0,90,20,70,90,90,40,90,90,80,90)
puntaje[3,] = c(0,0,0,40,20,50,50,30,50,80,80,90)
puntaje[4,] = c(0,0,0,0,30,80,80,20,80,90,90,90)
puntaje[5,] = c(0,0,0,0,0,70,70,40,70,80,80,80)
puntaje[6,] = c(0,0,0,0,0,0,20,70,5,80,90,80)
puntaje[7,] = c(0,0,0,0,0,0,0,70,20,90,90,90)
puntaje[8,] = c(0,0,0,0,0,0,0,0,70,90,90,90)
puntaje[9,] = c(0,0,0,0,0,0,0,0,0,80,90,80)
puntaje[10,] = c(0,0,0,0,0,0,0,0,0,0,40,10)
puntaje[11,] = c(0,0,0,0,0,0,0,0,0,0,0,30)

colnames(puntaje) = comidas
rownames(puntaje) = comidas

puntaje[lower.tri(puntaje)] = t(puntaje)[lower.tri(puntaje)]

distancias = dist(puntaje)


### 1. 2 Clusters -------
hc <- hclust(dist(puntaje), method="ave")  #con distancia promedio entre clusters
hc <- hclust(dist(puntaje), method="single") #con distancia minima
hc <- hclust(dist(puntaje), method="complete")  #con distancia maxima

#Graficando el Dendograma
plot(hc,hang=-1)

# cut tree into 3 clusters
rect.hclust(hc, k=2)  #Definir los k clusters
groups <- cutree(hc, k=2)  #Graficar los k clusters


## 2. 3 Clusters -------
hc <- hclust(dist(puntaje), method="ave")  #con distancia promedio entre clusters
hc <- hclust(dist(puntaje), method="single") #con distancia minima
hc <- hclust(dist(puntaje), method="complete")  #con distancia maxima

#Graficando el Dendograma
plot(hc,hang=-1)

# cut tree into 3 clusters
rect.hclust(hc, k=3)  #Definir los k clusters
groups <- cutree(hc, k=3)  #Graficar los k clusters
# Visualizar la separacion de las especias


# 3. K Medioides -------
dis=daisy(puntaje,metric="gower") # Calcular matrices de distancia entre puntos
#K mediodes especificando la medida de distancia
km=pam(dis,k=3,diss=T,metric="gower") # No entra la mtriz de X sino la matriz de distancia
km
#grafica de siluetas: Diagrama de qu? tan bien funciona el cluester
plot(km)
# No muestra el cross plot

