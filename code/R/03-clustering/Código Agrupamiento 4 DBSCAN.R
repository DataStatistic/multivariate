
# Código ejemplo agrupamiento por DBSCAN

library(readxl) # cargue de datos
library(dplyr)  # manipulación de datos
library(dbscan) # algoritmo dbscan
library(mclust) # medidas de validación
library(plotly) # gráficos de superficie y contornos
library(factoextra) # gráficos de los grupos
# library(fpc)  # algoritmo dbscan, no cargar

# Conjunto de datos
datos = read_xlsx("Fish.xlsx")

# Variable Species, solo para comparar
species = factor(datos$Species) ; species

# Variables numéricas estandarizadas
datos = datos %>%
  select(Weight:Width) %>%
  mutate(across(everything(), ~ scale(.x)))

# Densidad bivariada
den = MASS::kde2d(datos$Length1, datos$Length3, n = 515)
#den = MASS::kde2d(datos$Length1, datos$Height, n = 515)
plot_ly(x=~den$x, y=~den$y, z = ~den$z) %>% add_contour()
plot_ly(x=~den$x, y=~den$y, z = ~den$z) %>% add_surface()

# Distancia
D = dist(datos) # datos debe estar estandarizado

# Determinación de los hiperparámetros
p = ncol(datos) ; p
MinPts = 2*p # > p y recomendado 2p. Mínimo número de puntos en el vecindario para ser núcleo
kNNdistplot(D, k = MinPts-1)
eps = 0.9 # radio de vecindad (hay que calibrarlo)
abline(h = eps, lty = 2)

# Agrupamiendo con DBSCAN
ag.dbs = dbscan(D, eps=eps, minPts=MinPts)

# Asignación de los individuos a los grupos
grupos = ag.dbs$cluster ; grupos # 0 indica atípico o ruido
table(grupos)

# Validación: índice dbcv, debe ser positivo
if(length(table(grupos))>2)  dbcv(datos, ag.dbs)$score

# Representación gráfica sobre una ACP
clplot(datos, ag.dbs, pch = 20, cex = 2)

# Simulación para obtener los hiperparámetros

# Distancia
D = dist(datos) # datos debe estar estandarizado
Dmat = as.matrix(D)

# Grilla de parámetros
p = ncol(datos)
grid = expand.grid(eps = seq(0.5, 1.5, by = 0.05),
                   MinPts = seq(p+1, 3*p))

# Función para calcular DBCV sin errores
calc_dbcv = function(D, grupos, p){
  k = sum(names(table(grupos)) != "0") # núm. de grupos (sin ruido)
  if(k < 2) return(NA) else dbcv(D, grupos, d = p)$score
}

# Función que calcula la estabilidad por submuestreo con ARI
# comparando con grupos iniciales del dbscan sobre la muestra.
# Se selecciona el p*100% de la muestra en cada una de las B muestras
calc_ari = function(Dmat, grupos_base, eps, MinPts, B = 100, prop = 0.80) {
  n = length(grupos_base)
  ari = numeric(B)
  for(b in seq_len(B)) {
    id = sample(seq_len(n), size = floor(prop*n), replace = FALSE)
    D.b = as.dist(Dmat[id, id])
    ag.b = dbscan(D.b, eps = eps, minPts = MinPts)
    ari[b] = adjustedRandIndex(grupos_base[id], ag.b$cluster) # ARI
  }
  mean(ari, na.rm = TRUE) # ARI promedio sobre las B muestras
}

# Evaluación de la grilla
set.seed(123)
res = data.frame(eps = grid$eps, MinPts = grid$MinPts,
                 dbcv = NA, k = NA, ruido = NA, ari = NA)

for(i in seq_len(nrow(grid))){
  ag = dbscan(D, eps = grid$eps[i], minPts = grid$MinPts[i])
  grupos = ag$cluster
  res$k[i] = sum(names(table(grupos)) != "0")
  res$ruido[i] = mean(grupos == 0)*100 # porcentaje de ruido
  res$dbcv[i] = calc_dbcv(D, grupos, p)
  if(res$k[i] >= 2){
    res$ari[i] = calc_ari(Dmat = Dmat, grupos_base = grupos,
                          eps = grid$eps[i], MinPts = grid$MinPts[i], 
                          B = 100, prop = 0.80)}
}

# Ordenar por DBCV
# res = res[order(-res$dbcv), ]
res = res[res$k>2,]
res[order(-res$dbcv, -res$ari, res$ruido), ]

# Agrupamiento seleccionado

MinPts = 7
eps = 0.85
ag.dbs = fpc::dbscan(D, eps=eps, MinPts=MinPts, method = "dist")
ag.dbs # seed -> nucleo
grupos = ag.dbs$cluster ; grupos # 0 indica atípico o ruido
table(grupos)

# Comparando con Species
table(grupos, species)

# Gráfico
fviz_cluster(ag.dbs, data = datos, stand = FALSE)

# Estadísticas de validación

# Recodificar ruido para cluster.stats()
grupos_cs = grupos
if(any(grupos_cs == 0)) grupos_cs[grupos_cs == 0] = max(grupos_cs) + 1

val = fpc::cluster.stats(d = D, clustering = grupos_cs,
                         noisecluster = any(grupos == 0))

val$avg.silwidth       # silueta promedio, maximizar
val$dunn               # índice de Dunn, maximizar
val$ch                 # Calinski-Harabasz, maximizar

