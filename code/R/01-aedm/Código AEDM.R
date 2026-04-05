
# Código ejemplo AEDM

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

wdi = read_excel("WDI.xlsx") %>%
  column_to_rownames("Country") %>%
  mutate(Income = ordered(Income,
                          levels = c("Low income",
                                     "Lower middle income",
                                     "Upper middle income",
                                     "High income"),
                          labels = c("Low","Lower middle",
                                     "Upper middle","High")),
         Region = factor(Region))

View(wdi)
dim(wdi)

# Casos completos (sin faltantes)
datos = cc(wdi) # eliminando individuos con faltantes
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
  cov(use = "pairwise.complete.obs") # matriz de covarianzas
S

# Matriz de correlaciones
R = datos %>% 
  select_if(is.numeric) %>%
  cor(use = "pairwise.complete.obs") # matriz de correlaciones
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
  dplyr::select(Income | where(is.numeric)) %>%
  med.cov(g = 1)

# Asignando valores 1,2,... a las categorías
# de los factores ordinales
datos2 = datos %>% 
  mutate(Income = as.numeric(ordered(Income,
            levels = c("Low","Lower middle","Upper middle","High"),
            labels = 1:4))) %>%
  dplyr::select(-Region)
View(datos2)

# Matriz de correlaciones de spearman
R = cor(datos2, method = "spearman")

# Gráfico de correlaciones
corrplot(R, method = "ellipse", type="lower", diag = FALSE, 
         addCoef.col = TRUE, order = "original", tl.col = 1)

# Matriz de correlaciones de kendall
R = cor(datos2, method = "kendall")

# Gráfico de correlaciones
corrplot(R, method = "ellipse", type="lower", diag = FALSE, 
         addCoef.col = TRUE, order = "original", tl.col = 1)

# Covarianza y correlación de distancia
# Seleccionando los grupos de variables
X = scale(datos[,c("Education", "Sanitation", "Health", "Urban")])
Y = scale(datos[,c("Inflation", "GDP", "Mortality", "Labor")])

# Covarianza de distancia
dcov(X,Y)

# Correlación de distancia
dcor(X,Y)

# Distribución normal multivariada

# Gráficos de densidad teóricos
# Generando los valores
x1 <- seq(-3.0, 8.5, 0.1) 
x2 <- seq(-5.0, 8.5, 0.1)
media <- c(3, 2)
covar <- matrix(c(2.0, -1.5, -1.5, 3.0), nrow=2) ; covar
f <- function(x1, x2) dmnorm(cbind(x1, x2), media, covar)
y <- outer(x1, x2, f)

# Densidad bivariada
plot_ly(x=~x1, y=~x2, z = ~y) %>% add_surface()

# Contornos
plot_ly(x=~x1, y=~x2, z = ~y) %>% add_contour()

# Explorando la normalidad multivariada

# Asimetría y curtosis
m = datos %>%
  select_if(is.numeric) %>%
  psych::mardia(plot = FALSE)
asim = m$b1p ; asim # asim = 0?
curt = m$b2p ; curt # curt = 80 (8*(8+2))?

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

# Pruebas de normalidad univariadas
m$univariate_normality

# Detección de atípicos
datos %>% 
  select_if(is.numeric) %>%
  data.frame() %>%
  mv_outlier(method = "quan")

# Gráficos multivariados

# Diagrama de dispersión
ggplot(datos) + 
  geom_point(aes(x = Sanitation, y = Mortality, 
                 color = Income, size = Urban)) +
  scale_color_brewer(palette = "Set2")

# Diagrama de cajas bivariado
bagplot = datos %>% 
  dplyr::select(Sanitation, Mortality) %>%
  PlotBag(show.whiskers = FALSE, 
          show.looppoints = FALSE, show.bagpoints = FALSE,
          show.loophull = TRUE, show.baghull = TRUE, 
          pch = 19, cex = 0.8, show.outlier = TRUE)

# Atípicos
bagplot$pxy.outlier

# Cálculo de la densidad empírica bivariada
den = with(datos, kde2d(Sanitation, Mortality, n = 515)) 
Densidad = den$z
Sanitation = den$x
Mortality = den$y

# Gráfico de la densidad empírica bivariada
plot_ly(x=~Sanitation, y=~Mortality, z = ~Densidad) %>% add_surface()

# Gráfico de contornos
plot_ly(x=~Sanitation, y=~Mortality, z = ~Densidad) %>% add_contour()

# Variables categóricas

# Selección de las variables
datos2 = datos %>%
  mutate(GDP = ordered(cut(GDP, 3),
           labels = c("Low", "Medium", "High")),
         Health = ordered(cut(Health, 3),
           labels = c("Low", "Medium", "High")))

# Tabla de contingencias
tc = with(datos2, table(GDP, Health))
addmargins(tc)

# Frecuencias relativas condicionales
frc = prop.table(tc, 1) 
frc*100 # como un %

# Gráfico de frecuencias condicionales
as.data.frame(frc) %>%
  ggplot(aes(x=GDP, y = Freq, fill=Health)) + 
  geom_bar(stat="identity") + ylab("% de trabajadores") +
  scale_fill_brewer(palette = 3, type = "qual")

# Prueba chi-cuadrado
chisq.test(tc)

# mejorando el p-valor
chisq.test(tc, simulate.p.value = TRUE)

# Coeficiente de contingencia
ContCoef(tc)
# Coeficiente V de Cramer
CramerV(tc)

# Datos faltantes

# Descripción de los datos faltantes

# % de celdas, de variables y de países con faltantes
faltantes = miss_prop_summary(wdi)*100
colnames(faltantes) = c("celdas","variables","países")
faltantes

# % de faltantes por variable
gg_miss_var(wdi, show_pct = TRUE) +
  labs(y = "% de faltantes", x = "")

# Patrones de valores faltantes
gg_miss_upset(wdi, nsets = ncol(wdi), 
              mainbar.y.label = "# de faltantes",
              sets.x.label = "# de faltantes") 

# Prueba de faltantes MCAR
wdi %>%
  select_if(is.numeric) %>%
  mcar_test()

# Medidas de posición central
wdi %>%
  select_if(is.numeric) %>%
  med.loc() # med. mult. solo considera cc

# Matriz de correlaciones
R = wdi %>% 
  select_if(is.numeric) %>%
  cor(use = "pairwise.complete.obs") # pares de obs. completas
R

# Gráfico de correlaciones
corrplot(R, method = "ellipse", type="lower", diag = FALSE, 
         addCoef.col = T, order = "original", tl.col = 1)

# Matriz de correlaciones parciales
Rp = cc(wdi) %>% # solo cc
  select_if(is.numeric) %>%
  pcor() # correlaciones parciales
Rp$estimate

# Gráfico de correlaciones parciales
corrplot(Rp$estimate, method = "ellipse",
         type="lower", diag = FALSE, addCoef.col = TRUE,
         order = "original", tl.col = 1)

# Varianza generalizada y total
wdi %>% data.frame() %>%
  dplyr::select(Income | where(is.numeric)) %>%
  med.cov(g = 1)

# Covarianza y correlación de distancia
# Seleccionando los grupos de variables
X = scale(cc(datos)[,c("Education", "Sanitation", "Health", "Urban")]) # solo cc
Y = scale(cc(datos)[,c("Inflation", "GDP", "Mortality", "Labor")]) # solo cc

# Correlación de distancia
dcor(X,Y)

