
# Ejemplo agrupamiento con datos faltantes

library(ClustImpute)
library(miclust)
library(mice)
library(FactoMineR)
library(factoextra)
library(NbClust)
library(fpc)
library(cluster)
library(dplyr)
library(readxl)

# Datos World Development Indicators

wdi = read_excel("WDI.xlsx") %>% 
  tibble::column_to_rownames("Country") %>%
  mutate(Income = ordered(Income,
                          levels = c("Low income",
                                     "Lower middle income",
                                     "Upper middle income",
                                     "High income"),
                          labels = c("Low","Lower middle",
                                     "Upper middle","High")),
         Region = factor(Region))

View(wdi)

# Patrones de faltantes
md.pattern(wdi, rotate.names = T)

# Variables numéricas
datos = select(wdi, where(is.numeric))

# Algoritmo ClustImpute
z = data.frame(scale(datos))
ci = ClustImpute(z, nr_cluster = 4, n_end = 10, nr_iter = 15, c_steps = 30)
ci$complete_data # datos imputados
ci$clusters # pertenecia a cada grupo
table(ci$clusters)

# Validación
val = cqcluster.stats(d = dist(ci$complete_data), 
              clustering = ci$clusters)
val$dunn # Estadística Dunn - maximizar
val$ch   # Calinski-Harabasz - minimizar
val$asw  # Silhouette - maximizar
val$clus.avg.silwidths
sil = silhouette(ci$clusters, dist(ci$complete_data))
fviz_silhouette(sil)

# Gráfico
pca = PCA(ci$complete_data, graph = FALSE)
fviz_pca_ind(pca, habillage = factor(ci$clusters), repel = T, addEllipses = TRUE)

