
# Métodos de agrupamiento 1
# Cálculo de medidas de distancia

library(readxl)      # importar datos de excel
library(dplyr)       # manipulación de datos
library(factoextra)  # gráficos de matrices de distancia
library(cluster)     # distancia de gower

# Datos trabajadores
# Cargue y depuración del conjunto de datos
datos = read_xlsx("Trabajadores.xlsx") %>% 
  mutate(across(c(1,3,8,12), factor),
         Escolaridad = ordered(Escolaridad,
                               levels = c("Técnico","Pregrado","Posgrado")),
         Estrato = ordered(Estrato),
         Situación = ordered(Situación,
                             levels = c("Difícil","Regular","Buena")))

# Ilustrando el cálculo de distancias
# para diferentes tipos de variables

# Distancia para variables númericas

# Matriz de distancias - variables numéricas

# Distancia de Manhattan
D = datos %>%
  select(where(is.numeric)) %>%
  mutate(across(everything(), function(x) (x-mean(x))/sd(x))) %>%
  dist(method = "manhattan")
# get_dist(select(datos, where(is.numeric)), method = "manhattan", stand = TRUE)
D

# Gráfico de la matriz de distancias
fviz_dist(D, gradient = list(low = "#0e5eb3",
                             mid = "white",
                             high = "#FC4E07"))

# Distancia Euclidiana
D = datos %>%
  select(where(is.numeric)) %>%
  get_dist(method = "euclidean", stand = TRUE)
D

# Gráfico de la matriz de distancias
fviz_dist(D, gradient = list(low = "#0e5eb3",
                             mid = "white",
                             high = "#FC4E07"))

# Distancia para variables binarias

# Distancia de Jaccard
D = datos %>%
  select(Créditos, Ayuda, Hijos) %>%
  mutate(Créditos = ifelse(Créditos=="Sin Créditos", 0, 1),
         Ayuda = ifelse(Ayuda=="Sí", 1, 0),
         Hijos = ifelse(Hijos>0, 1, 0)) %>%
  get_dist(method = "binary")
D

# Gráfico de la matriz de distancias
fviz_dist(D, gradient = list(low = "#0e5eb3",
                             mid = "white",
                             high = "#FC4E07"))

# Distancia de Afinidad
D = datos %>% 
  select(Créditos, Ayuda, Hijos) %>%
  mutate(Créditos = ifelse(Créditos=="Sin Créditos", 0, 1),
         Ayuda = ifelse(Ayuda=="Sí", 1, 0),
         Hijos = ifelse(Hijos>0, 1, 0)) %>%
  daisy(metric = "gower", type = list(symm = 1:3))

# Gráfico de la matriz de distancias
fviz_dist(D, gradient = list(low = "#0e5eb3",
                             mid = "white",
                             high = "#FC4E07"))

# Distancia para variables categóricas

D = datos %>% 
  select(Sexo, Estado, Créditos) %>%
  mutate(Sexo = factor(Sexo),
         Estado = factor(Estado),
         Créditos = factor(Créditos)) %>%
  daisy(metric = "gower", type = list(factor = 1:3))

# Gráfico de la matriz de distancias
fviz_dist(D, gradient = list(low = "#0e5eb3",
                             mid = "white",
                             high = "#FC4E07"))

# Distancia para variables ordinales
D = datos %>% 
  select(Escolaridad, Estrato, Situación) %>%
  mutate(Escolaridad = ordered(Escolaridad, levels=c("Técnico", "Pregrado", "Posgrado"), labels = c("Técnico", "Pregrado", "Posgrado")),
         Estrato = ordered(Estrato, levels=c("1", "2", "3", "4+"), labels = c("1", "2", "3", "4+")),
         Situación = ordered(Situación, levels=c("Difícil", "Regular", "Buena"), labels = c("Difícil", "Regular", "Buena"))) %>%
  daisy(metric = "gower", type = list(ordered = 1:3))

# Gráfico de la matriz de distancias
fviz_dist(D, gradient = list(low = "#0e5eb3",
                             mid = "white",
                             high = "#FC4E07"))

# Distancia para mixtura de variables (Gower)
D = datos %>% 
  mutate(Ayuda = ifelse(Ayuda=="Sí", 1, 0),
         Sexo = factor(Sexo),
         Estado = factor(Estado),
         Créditos = factor(Créditos),
         Escolaridad = ordered(Escolaridad, levels=c("Técnico", "Pregrado", "Posgrado"), labels = c("Técnico", "Pregrado", "Posgrado")),
         Estrato = ordered(Estrato, levels=c("1", "2", "3", "4+"), labels = c("1", "2", "3", "4+")),
         Situación = ordered(Situación, levels=c("Difícil", "Regular", "Buena"), labels = c("Difícil", "Regular", "Buena"))) %>%
  daisy(metric = "gower", type = list(asymm = 8,factor = c(1,3,11), ordered = 5:6))

# Gráfico de la matriz de distancias
fviz_dist(D, gradient = list(low = "#0e5eb3",
                             mid = "white",
                             high = "#FC4E07"))
