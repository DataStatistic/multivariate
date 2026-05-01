
# Métodos de agrupamiento
# Algoritmos de agrupamiento por k-medias

library(readxl)        # importar datos de excel
library(dplyr)         # manipulación de datos
library(factoextra)    # gráficos de agrupamiento, matrices de distancia, métodos de agrupamiento y medidas de validación
library(cluster)       # métodos de agrupamiento y medidas de distancia
library(fpc)           # métodos de agrupamiento y medidas de validación
library(clusterSim)    # medidas de validación
library(clValid)       # medidas de validación
library(clusterCrit)   # medidas de validación
library(klaR)          # agrupamiento categóricas
library(clustMixType)  # agrupamiento mixtura de variables
library(e1071)         # agrupamiento difuso
library(fclust)        # agrupamiento difuso con mixtura de variables

# Cargue y depuración del conjunto de datos
datos = read_xlsx("Trabajadores.xlsx") %>% 
  mutate(across(c(1,3,8,12), factor),
         Escolaridad = ordered(Escolaridad,
                               levels = c("Técnico","Pregrado","Posgrado")),
         Estrato = ordered(Estrato),
         Situación = ordered(Situación,
                             levels = c("Difícil","Regular","Buena")))

# Métodos basados en centroides

# Variables numéricas

# Agrupamiento por k-medias
k = 2 # número de grupos (se debe determinar)
ag.km = datos %>%
  dplyr::select(where(is.numeric)) %>%
  scale() %>%
  kmeans(centers = k, iter.max = 30, nstart = 10, algorithm = "Hartigan-Wong")

# Asignación de los individuos a los grupos
grupos = ag.km$cluster ; grupos
head(data.frame(grupos, datos), 10)

# Representación de los grupos mediante un ACP
datos %>%
  dplyr::select(where(is.numeric)) %>%
  fviz_cluster(ag.km, data = ., stand = TRUE)

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
  clusGap(FUNcluster = kmeans, algorithm = "Hartigan-Wong", 
          K.max = k, B = 100, nstart = 10)
val$Tab[k, "gap"] # Gap statistic

# Medidas de estabilidad
val = datos %>%
  dplyr::select(where(is.numeric)) %>%
  scale() %>%
  clValid(nClust = k, validation = "stability", 
          clMethods = "kmeans", algorithm = "Hartigan-Wong",
          metric = "euclidean")
measures(val)[1,,] # APN
measures(val)[4,,] # FOM

set.seed(123)
val = datos %>%
  dplyr::select(where(is.numeric)) %>%
  prediction.strength(Gmax = k, M = 100,
                      clustermethod = kmeansCBI,
                      classification = "centroid",
                      scaling = TRUE, 
                      count = FALSE,
                      runs = 10)
val$mean.pred[k] # Prediction strength

set.seed(123)
val = val = datos %>%
  dplyr::select(where(is.numeric)) %>%
  clusterboot(B = 100, bootmethod = "boot", 
              clustermethod = kmeansCBI, k = k, runs = 10,
              scaling = TRUE, seed = 123, count = FALSE)
val$bootmean # Bootstrap Jaccard stability
mean(val$bootmean)

# Gráficas

# Silhouette o Gap
datos %>%
  dplyr::select(where(is.numeric)) %>%
  scale() %>%
  fviz_nbclust(kmeans, method = "sil", nstart = 10) # "sil" o "gap"

sil = silhouette(grupos, dist = D) # Silhouette por individuo
fviz_silhouette(sil)

###########

# hk-means
ag.hkm = datos %>%
  dplyr::select(where(is.numeric)) %>%
  scale() %>%
  hkmeans(k, hc.metric = "euclidean", hc.method = "ward.D2",
          iter.max = 30, km.algorithm = "Hartigan-Wong")

grupos = ag.hkm$cluster ; grupos

##########

# k-medoides
set.seed(123)
ag.kmed = datos %>%
  dplyr::select(where(is.numeric)) %>%
  pam(k = k, stand = TRUE, metric = "euclidean")

grupos = ag.kmed$clustering ; grupos

##########

# Fuzzy k-means
set.seed(123)
ag.fkm = datos %>%
  dplyr::select(where(is.numeric)) %>%
  scale() %>%
  cmeans(centers = k, m = 2, iter.max = 30,
         method = "cmeans", dist = "euclidean")

# Asignación a los grupos
ag.fkm$membership # grados de pertenencia
grupos = ag.fkm$cluster ; grupos # grupos duros

##########

# Variables categóricas

# Agrupamiento por k-modas
k = 2 # número de grupos (se debe determinar)
ag.kmo = datos %>%
  dplyr::select(where(is.factor)) %>% # usando ordinales como categóricas
  as.data.frame() %>%
  kmodes(modes = k, iter.max = 15) # no hay tratamiento de binarias asimétricas

# Asignación de los individuos a los grupos
grupos = ag.kmo$cluster ; grupos

##########

# Mixtura de variables

# Agrupamiento por k-prototipos
k = 2 # número de grupos (se debe determinar)
ag.kp = datos %>%
  as.data.frame() %>%
  kproto(k = k, iter.max = 15, type = "gower", verbose = FALSE) # no hay tratamiento de binarias asimétricas

# Asignación de los individuos a los grupos
grupos = as.vector(ag.kp$cluster) ; grupos
head(data.frame(grupos, datos), 10)

# Validación

# Validación interna
val = validation_kproto(method = "silhouette", object = ag.kp)
val # Silhouette promedio

val = validation_kproto(method = "dunn", object = ag.kp)
val # Dunn

# Calinski-Harabasz no aplica
# Connectivity no aplica
# Davies–Bouldin no aplica
# Gap no aplica

# Medidas de estabilidad
set.seed(123)
val = stability_kproto(method = "jaccard", object = ag.kp, B = 100)
val[[1]] # Bootstrap Jaccard stability

# APN no aplica
# FOM no aplica
# Prediction Strength no aplica

#############

# k-medoides

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

set.seed(123)
ag.kmed = pam(D, k = k, diss = TRUE)

grupos = ag.kmed$clustering ; grupos

#############

# Agrupamiento difuso

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

ag.dif = NEFRC(D, k = k, m = 2, seed = 123,
               RS = 10, index = "SIL.F")

# Asignación a los grupos
ag.dif$U # grados de pertenencia
grupos = ag.dif$clus[, 1] ; grupos # grupos duros
