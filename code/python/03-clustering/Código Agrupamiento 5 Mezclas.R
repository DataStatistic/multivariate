
# Código ejemplo agrupamiento por mezclas gaussianas

library(readxl)  # cargue de datos
library(dplyr)   # manipulación de datos
library(mclust)  # agrupamiento por mezclas gaussianas
library(factoextra) # gráficos de los grupos

# Conjunto de datos
datos = read_xlsx("Fish.xlsx")

# Variable Species, solo para comparar
species = factor(datos$Species) ; species

# Variables numéricas estandarizadas
datos = datos %>%
  select(Weight:Width) %>%
  mutate(across(everything(), ~ scale(.x)))

# Modelo de mezcla gaussiana
ag.gmm = Mclust(datos, G = 1:10)

# Grupos asignados
ag.gmm$G
grupos = ag.gmm$classification
table(grupos)

# Probabilidades de pertenencia
round(ag.gmm$z, 5)

# Comparando con Species
table(grupos, species)

# Gráficos
plot(ag.gmm, what = "classification")

fviz_cluster(ag.gmm, datos, stand = FALSE,
             geom = "point", ellipse.type = "convex")

# Validación

ag.gmm$bic # debe ser maximizado (es automático)

# Distancia
D = dist(datos) # datos debe estar estandarizado
val = fpc::cluster.stats(d = D, clustering = grupos)

val$avg.silwidth       # silueta promedio, maximizar
val$dunn               # índice de Dunn, maximizar
val$ch                 # Calinski-Harabasz, maximizar
