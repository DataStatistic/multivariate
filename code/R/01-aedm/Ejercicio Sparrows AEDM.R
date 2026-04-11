
library(readxl)    # importar datos de excel
library(dplyr)     # manipulación de datos
library(tidyr)     # manipulación de datos
library(tibble)    # manipulación de datos
library(forcats)   # manipulación de datos
library(ggplot2)   # gráficos avanzados
library(plotly)    # gráficos avanzados e interactivos
library(pcaPP)     # Mediana multivariada
library(ppcor)     # correlaciones parciales
library(energy)    # covarianza y correlación de distancia
library(corrplot)  # gráficos de matrices de correlación
library(DescTools) # bagplot
library(mnormt)    # distribución normal multivariada
library(psych)     # asimetría y curtosis
library(MVN)       # test de normalidad multivariada
library(MASS)      # densidad kernel bivariada
library(naniar)    # descripción de datos faltantes
library(mice)      # tratamiento de datos faltantes

options(digits = 3)

# Datos multivariados:
# World Development Indicators

datos = read_excel("Sparrows.xlsx") %>% 
  dplyr::select(-Bird) %>%
  mutate(Sobrevivieron = factor(Sobrevivieron))

View(datos)
dim(datos)

# AEDM

# Función que calcula los vectores de medidas de localización central
med.loc = function(X){
  med.loc = data.frame(
    matrix(c(colMeans(X, na.rm = TRUE),
             apply(X, 2, median, na.rm = TRUE),
             l1median(cc(X))), nrow = 3, byrow = T)
  )
  colnames(med.loc) = colnames(X)
  rownames(med.loc) = c("Promedios","Medianas", "Med. Mult.")
  return(med.loc)
}

# Medidas de posición central
datos %>%
  select_if(is.numeric) %>%
  med.loc()

# Matriz de covarianzas
S = datos %>% 
  select_if(is.numeric) %>%
  cov() # matriz de covarianzas
S

# Matriz de correlaciones
R = datos %>% 
  select_if(is.numeric) %>%
  cor() # matriz de correlaciones
R

# Gráfico de correlaciones
corrplot(R, method = "ellipse", type="lower", diag = FALSE, 
         addCoef.col = T, order = "original", tl.col = 1)

# Matriz de correlaciones parciales
Rp = datos %>% 
  select_if(is.numeric) %>%
  pcor() # correlaciones parciales
Rp$estimate

# Gráfico de correlaciones parciales
corrplot(Rp$estimate, method = "ellipse",
         type="lower", diag = FALSE, addCoef.col = TRUE,
         order = "original", tl.col = 1)

# Varianza generalizada y total
med.cov = function(X, g = FALSE){
  if(g){
    grupo = X[,g]
    X = X[,-g]
    Vg = tapply(X, grupo, function(Y) det(cov(Y, use = "pairwise.complete.obs"))) # var. generalizada
    VT = tapply(X, grupo, function(Y) sum(diag(cov(Y, use = "pairwise.complete.obs")))) # var. total
  }
  list(Vg = Vg, VT= VT)
}

datos %>% data.frame() %>%
  med.cov(g = 6)

# Matriz de correlaciones de spearman
R = datos %>% 
  select_if(is.numeric) %>%
  cor(method = "spearman")

# Gráfico de correlaciones
corrplot(R, method = "ellipse", type="lower", diag = FALSE, 
         addCoef.col = TRUE, order = "original", tl.col = 1)

# Matriz de correlaciones de kendall
R = datos %>% 
  select_if(is.numeric) %>%
  cor(method = "kendall")

# Gráfico de correlaciones
corrplot(R, method = "ellipse", type="lower", diag = FALSE, 
         addCoef.col = TRUE, order = "original", tl.col = 1)

# Covarianza y correlación de distancia
# Seleccionando los grupos de variables
X = scale(datos[,c("total", "alar")])
Y = scale(datos[,c("pico", "humero", "quilla")])

# Covarianza de distancia
dcov(X,Y)

# Correlación de distancia
dcor(X,Y)

# Explorando la normalidad multivariada

# Asimetría y curtosis
m = datos %>%
  select_if(is.numeric) %>%
  psych::mardia(plot = FALSE)
asim = m$b1p ; asim # asim = 0?
curt = m$b2p ; curt # curt = 35 (5*(5+2))?

# Prueba de normalidad multivariada
m = datos %>% 
  select_if(is.numeric) %>%
  data.frame() %>%
  mvn(mvn_test = "mardia", 
      univariate_test = "AD", desc = FALSE)
m$multivariate_normality

# qqplot para normalidad multivariada
datos %>% 
  select_if(is.numeric) %>%
  data.frame() %>%
  multivariate_diagnostic_plot(type = "qq")

# Detección de atípicos
datos %>% 
  select_if(is.numeric) %>%
  data.frame() %>%
  mv_outlier(method = "quan")

# Normalidad multivariada por sobrevivencia
m = datos %>% 
  data.frame() %>%
  mvn(mvn_test = "mardia", subset = "Sobrevivieron",
      univariate_test = "AD", desc = FALSE)
m$multivariate_normality

# Gráficos multivariados

# Diagrama de dispersión
ggplot(datos) + 
  geom_point(aes(x = total, y = alar, 
                 color = Sobrevivieron, size = humero)) +
  scale_color_brewer(palette = "Set2")

# Diagrama de cajas bivariado
bagplot = datos %>% 
  dplyr::select(total, alar) %>%
  PlotBag(show.whiskers = FALSE, 
          show.looppoints = FALSE, show.bagpoints = FALSE,
          show.loophull = TRUE, show.baghull = TRUE, 
          pch = 19, cex = 0.8, show.outlier = TRUE)

# Atípicos
bagplot$pxy.outlier

# Cálculo de la densidad empírica bivariada
den = with(datos, kde2d(total, alar, n = 515)) 
Densidad = den$z
total = den$x
alar = den$y

# Gráfico de la densidad empírica bivariada
plot_ly(x=~total, y=~alar, z = ~Densidad) %>% add_surface()

# Gráfico de contornos
plot_ly(x=~total, y=~alar, z = ~Densidad) %>% add_contour()
