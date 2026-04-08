
# Métodos de reducción de dimensionalidad
# Aálisis factorial de datos mixtos

library(readxl)     # importar datos de excel
library(dplyr)      # manipulación de datos
library(tibble)     # manipulación de datos
library(ggplot2)    # gráficos avanzados
library(FactoMineR) # métodos de reducción de dimensionalidad
library(factoextra) # gráficos de mrd

options(digits = 3)

# Datos trabajadores
datos = read_xlsx("Trabajadores.xlsx")
View(datos)
dim(datos)

# Análisis factorial de datos mixtos

# Uso de la función FAMD
afdm = FAMD(datos, ncp = 10, graph = FALSE)

# Valores propios y porcentajes de inercia
colnames(afdm$eig) = c("v. propio", "inercia", "iner. acum.")
afdm$eig

# Histograma de valores propios (Scree Plot)
fviz_screeplot(afdm) + 
  xlab("Dimensiones") + 
  ylab("% de varianza explicada")

# Coordenadas de los individuos
afdm$ind$coord

# Representación de los individuos
fviz_famd_ind(afdm, axes = c(1,2), geom = c("point"), title = "Individuos", col.ind = 1)

# Coordenadas de las variables numéricas
afdm$quanti.var$coord

# Representación de las variables numéricas
fviz_famd_var(afdm, axes = c(2,3), choice = "quanti.var", title = "Numéricas",
              repel = TRUE, col.var = "black") +
  theme(legend.position="none")

# Coordenadas de las modalidades de las variables categóricas
afdm$quali.var$coord

# Representación de las modalidades de las variables categóricas
col.var = factor(rep(1:7, c(2,5,3,4,2,3,3))) # identificación de variables
fviz_famd_var(afdm, axes = c(2,3), choice = "quali.var", title = "Modalidades",
             repel = TRUE, col.var = col.var, mean.point = FALSE) +
  theme(legend.position="none")

# Cosenos cuadrados
round(afdm$quanti.var$cos2, 3) # var. numéricas
round(afdm$quali.var$cos2, 3)  # var. categóricas 

# Contribuciones
round(afdm$quanti.var$contrib, 3) # var. numéricas
round(afdm$quali.var$contrib, 3)  # var. categóricas 

