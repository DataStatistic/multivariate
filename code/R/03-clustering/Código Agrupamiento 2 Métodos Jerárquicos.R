
# Métodos de agrupamiento
# Algoritmos de agrupamiento jerárquicos

library(readxl)        # importar datos de excel
library(dplyr)         # manipulación de datos
library(factoextra)    # métodos de agrupamiento, matrices de distancia y gráficos de agrupamiento
library(cluster)       # métodos de agrupamiento y medidas de distancia
library(fpc)           # métodos de agrupamiento y medidas de validación
library(clusterSim)    # medidas de validación
library(clValid)       # medidas de validación
library(clusterCrit)   # medidas de validación

# Cargue y depuración del conjunto de datos
datos = read_xlsx("Trabajadores.xlsx") %>% 
  mutate(across(c(1,3,8,12), factor),
         Escolaridad = ordered(Escolaridad,
                               levels = c("Técnico","Pregrado","Posgrado")),
         Estrato = ordered(Estrato),
         Situación = ordered(Situación,
                             levels = c("Difícil","Regular","Buena")))

# Agrupamiento jerárquico

# Variables numéricas

# Método de Ward
ag.jer = datos %>%
  dplyr::select(where(is.numeric)) %>%
  scale() %>% # estandarización
  dist(method = "euclidean") %>%
  hclust(method = "ward.D2") # función básica

# Dendrograma (función básica)
plot(ag.jer, hang = -1, main = "", sub = "", 
     xlab = "Trabajadores", ylab = "Distancia")

# Agrupamiento (función mejorada)
# Asignación de los trabajadores a k grupos
k = 2 # número de grupos (se debe determinar)
ag.jer = datos %>%
  dplyr::select(where(is.numeric)) %>%
  get_dist(method = "euclidean", stand = TRUE) %>%
  hcut(k, isdiss = TRUE, hc_func = "hclust", hc_method = "ward.D2") 

# Asignación de los trabajadores a los grupos
grupos = ag.jer$cluster ; grupos
head(data.frame(grupos, datos), 10)

# Dendrograma con la partición en k grupos
p = fviz_dend(ag.jer, cex = 0.3, main = "", # linewidth = 0.5,
          xlab = "Trabajadores", ylab = "Distancia") +
  theme(legend.position = "none")
p$layers[[1]]$aes_params$linewidth <- 0.5 ; p

# Representación de los grupos en un ACP
datos %>%
  dplyr::select(where(is.numeric)) %>%
  fviz_cluster(ag.jer, stand = TRUE, data = .)

# Validación

# Distancia
D = datos %>%
  dplyr::select(where(is.numeric)) %>%
  scale() %>%
  dist(method = "euclidean")

# Validación interna
val = cluster.stats(D, clustering = grupos)
val$avg.silwidth   # Silhouette promedio
val$dunn           # Dunn
val$ch             # Calinski-Harabasz

val = datos %>%
  dplyr::select(where(is.numeric)) %>%
  scale() %>%
  intCriteria(part = as.integer(grupos), crit = "Davies_Bouldin")
val$davies_bouldin  # Davies–Bouldin

val = connectivity(as.matrix(D), clusters = grupos, neighbSize = 10)
val # Connectivity

set.seed(123)
val = datos %>%
  dplyr::select(where(is.numeric)) %>%
  scale() %>%
  clusGap(FUNcluster = hcut, K.max = k, B = 100,
          hc_func = "hclust", hc_method = "ward.D2")
val$Tab[k, "gap"] # Gap statistic

# Medidas de estabilidad
val = datos %>%
  dplyr::select(where(is.numeric)) %>%
  scale() %>%
  clValid(nClust = k, validation = "stability", 
          clMethods = "hierarchical", method = "ward",
          metric = "euclidean")
measures(val)[1,,] # APN
measures(val)[4,,] # FOM

set.seed(123)
val = datos %>%
  dplyr::select(where(is.numeric)) %>%
  prediction.strength(Gmax = k, M = 100,
                      clustermethod = hclustCBI, method = "ward.D2",
                      classification = "centroid",
                      scaling = TRUE, 
                      count = FALSE)
val$mean.pred[k] # Prediction strength

set.seed(123)
val = clusterboot(D, B = 100, bootmethod = "boot", 
                  clustermethod = disthclustCBI, k = k,
                  cut = "number",  method = "ward.D2",
                  seed = 123, count = FALSE)
val$bootmean # Bootstrap Jaccard stability
mean(val$bootmean)

# Gráficas

# Silhouette o Gap
datos %>%
  dplyr::select(where(is.numeric)) %>%
  scale() %>%
  fviz_nbclust(hcut, method = "sil") # "sil" o "gap"

sil = silhouette(grupos, dist = D) # Silhouette por individuo
fviz_silhouette(sil)

#############

# Variables numéricas, binarias, categóricas y ordinales

# Método de Ward
k = 3 # número de grupos
ag.jer = datos %>%
  mutate(Ayuda = ifelse(Ayuda=="Sí", 1, 0)) %>% # binarias deben ser 0,1
  daisy(metric = "gower", 
        type = list(numeric = c(2,4,7,9,10,13), # default
                    asymm = c(8),
                    factor = c(1,3,12),         # default
                    ordered = c(5,6,11))) %>%   # default
  hcut(k = k, isdiss = TRUE, hc_func = "hclust", hc_method = "ward.D2") 

# Asignación de los trabajadores a los grupos
grupos = ag.jer$cluster ; grupos
head(data.frame(grupos, datos), 10)

# Dendrograma (k grupos)
p = fviz_dend(ag.jer, cex = 0.3, main = "", # linewidth = 0.5,
              xlab = "Trabajadores", ylab = "Distancia") +
  theme(legend.position = "none")
p$layers[[1]]$aes_params$linewidth <- 0.5 ; p

# Validación

# Disimilitud
D = datos %>%
  mutate(Ayuda = ifelse(Ayuda == "Sí", 1, 0)) %>%
  daisy(
    metric = "gower",
    type = list(
      numeric = c(2, 4, 7, 9, 10, 13),
      asymm   = c(8),
      factor  = c(1, 3, 12),
      ordered = c(5, 6, 11)))

# Validación interna
val = cluster.stats(d = D, clustering = grupos)
val$avg.silwidth   # Silhouette promedio
val$dunn           # Dunn
val$ch             # Calinski-Harabasz

val = connectivity(distance = as.matrix(D), clusters = grupos, neighbSize = 10)
val # Connectivity

# Davies–Bouldin no aplica
# Gap no aplica

# Medidas de estabilidad
set.seed(123)
val = clusterboot(data = D, B = 100, bootmethod = "boot", 
                  clustermethod = disthclustCBI, k = k,
                  cut = "number",  method = "ward.D2",
                  seed = 123, count = FALSE)
val$bootmean # Bootstrap Jaccard stability
mean(val$bootmean)

# APN no aplica
# FOM no aplica
# Prediction Strength no aplica

# Gráfica

# Silhouette
sil = silhouette(grupos, dist = D)
fviz_silhouette(sil)

