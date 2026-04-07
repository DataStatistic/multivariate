
# Métodos de reducción de dimensionalidad
# Análisis de componentes principales (ACP)

library(readxl)     # importar datos de excel
library(dplyr)      # manipulación de datos
library(tidyr)      # manipulación de datos
library(tibble)     # manipulación de datos
library(forcats)    # manipulación de datos
library(ggplot2)    # gráficos avanzados
library(gridExtra)  # páneles gráficos
library(FactoMineR) # métodos de reducción de dimensionalidad
library(factoextra) # gráficos de métodos de reducción de dimensionalidad
library(MASS) # matriz de covarianzas robusta
library(pcaPP)      # acp mediante projection-pursuit 
library(kernlab)    # acp por kernels
library(skewsamp)   # cuantiles empíricos
library(mice)       # imputación de datos faltantes
library(missMDA)    # manejo de faltantes en mrd 

options(digits = 3)

# Datos: World Development Indicators
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

# Casos completos (sin faltantes)
datos = cc(wdi) # eliminando individuos con faltantes
View(datos)
dim(datos)

# ACP "a mano"

# Matriz estandarizada y ponderada
Z = datos %>%
  select_if(is.numeric) %>%
  mutate(across(everything(), 
                function(x) (x-mean(x))/(sd(x)*sqrt(length(x)-1)))) %>%
  as.matrix()

# DVS (Z = P%*%D%*%t(Q))
dvs = svd(Z)
D = diag(dvs$d) ; D

# Valores propios de Rx
lamb = dvs$d^2 ; lamb # ¿>1?

# Vectores propios de Z*t(Z)
P = dvs$u ; head(P, 15) # primears filas de P

# Vectores propios de t(Z)*Z
Q = dvs$v ; Q # (para construir la fórmula de las componentes)

# Componentes principales
F = data.frame(Z%*%Q)
colnames(F) = paste("CP", 1:ncol(F), sep = "")
head(F, 15) # primeras filas de F

# Primer plano principal
ggplot(F, aes(x = CP1, y = CP2, label = rownames(F))) +
  geom_point() +
  geom_text(vjust = -1, size = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2)

# Correlaciones entre componentes y variables
Rcv = cor(Z,F) ; Rcv

# Primer plano principal (k=1 y l=2)
ggplot(Rcv, aes(x = CP1, y = CP2, label = rownames(Rcv))) +
  geom_text(vjust = 1) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_segment(aes(x = 0, y = 0, xend = CP1, yend = CP2),
               arrow = arrow(length = unit(0.1, "inches"))) +
  annotate("path",
           x=cos(seq(0,2*pi,length.out=100)),
           y=sin(seq(0,2*pi,length.out=100))) +
  xlim(c(-1,1)) + ylim(c(-1,1)) +
  coord_fixed()

# cos2 de las variables
cos2v = Rcv^2 ; cos2v # ¿>0.3?

# cos2 de los individuos
cos2i = t(apply(F, 1, function(x) x^2/sum(x^2)))
head(cos2i, 10) # primeras filas

# Contribuciones de las variables
ctrv = apply(Rcv, 2, function(x) x^2/sum(x^2))
ctrv # ¿>1/p%?

# Contribuciones de los individuos
ctri = apply(F, 2, function(x) x^2/sum(x^2))
head(ctri, 10) # primeras filas

# Inercia de cada componente
inercia = lamb/sum(lamb)*100 ; inercia

# Inercia acumulada
cumsum(inercia) # ¿>70%?

# Usando librerías

# ACP
acp = PCA(datos, 
          scale.unit = T, # estandariza los datos
          ncp = 4, # número de componentes a mostar (no el seleccionado)
          quali.sup = c(1,2), # variables cualitativas de grupo
          graph = FALSE)

# Valores propios y porcentajes de inercia
colnames(acp$eig) = c("v. propio", "inercia", "iner. acum.")
acp$eig

# Histograma de valores propios (Scree Plot)
fviz_screeplot(acp) + 
  xlab("Componentes") + 
  ylab("% de varianza explicada")

# Vectores propios (Q)
rownames(acp$svd$V) = rownames(acp$var$coord)
colnames(acp$svd$V) = paste("CP", 1:ncol(acp$svd$V), sep = "")
acp$svd$V # Q 

# Correlaciones entre variables y componentes
acp$var$cor

# cos2 entre variables y componentes
round(acp$var$cos2,3)

# Representación de las variables
# Componentes 1 y 2
fviz_pca_var(acp, axes = c(1,2), col.var = "cos2", 
             title = "Variables",repel = TRUE) +
  scale_color_gradient2(low="red", mid="blue", high="black", midpoint=.3)

# Componentes 1 y 3
fviz_pca_var(acp, axes = c(1,3), col.var = "cos2", 
             title = "Variables",repel = TRUE) +
  scale_color_gradient2(low="red", mid="blue", high="black", midpoint=.3)

# Componentes 2 y 3
fviz_pca_var(acp, axes = c(2,3), col.var = "cos2", 
             title = "Variables",repel = TRUE) +
  scale_color_gradient2(low="red", mid="blue", high="black", midpoint=.3)

# Contribuciones de las variables
acp$var$contrib

# Gráfico de contribuciones de las variables
c1 = fviz_contrib(acp, choice = "var", axes = 1, title = "CP1") + 
  ylab("Contribuciones (%)")
c2 = fviz_contrib(acp, choice = "var", axes = 2, title = "CP2") + 
  ylab("Contribuciones (%)")
c3 = fviz_contrib(acp, choice = "var", axes = 3, title = "CP3") + 
  ylab("Contribuciones (%)")
grid.arrange(c1, c2, c3, ncol=2, nrow = 2)

# Coordenadas de los individuos (componentes principales)
head(acp$ind$coord, 15) # primeras filas de sqrt(n)*F

# Representación de los individuos
# Componentes 1 y 2
fviz_pca_ind(acp, axes = c(1,2), geom = c("point","text"), 
             habillage = 2, addEllipses = FALSE, 
             title = "Individuos") +
  scale_color_brewer(palette = "Set1")

# Componentes 1 y 3
fviz_pca_ind(acp, axes = c(1,3), geom = c("point","text"), 
             habillage = 2, addEllipses = TRUE,
             title = "Individuos") +
  scale_color_brewer(palette = "Set1")

# Componentes 2 y 3
fviz_pca_ind(acp, axes = c(2,3), geom = c("point","text"), 
             habillage = 2, addEllipses = TRUE,
             title = "Individuos") +
  scale_color_brewer(palette = "Set1")

# Representación simultánea de variables e individuos
# Componentes 1 y 2
fviz_pca_biplot(acp, axes = c(1,2), geom = c("point","text"),
                col.var = "#3c3c3c",
                habillage = 2, addEllipses = TRUE,
                title = "Variables e individuos") +
  scale_color_brewer(palette = "Set1")

# Componentes 1 y 3
fviz_pca_biplot(acp, axes = c(1,3), geom = c("point","text"),
                col.var = "#3c3c3c",
                habillage = 2, addEllipses = TRUE,
                title = "Variables e individuos") +
  scale_color_brewer(palette = "Set1")

# Componentes 2 y 3
fviz_pca_biplot(acp, axes = c(2,3), geom = c("point","text"),
                col.var = "#3c3c3c",
                habillage = 2, addEllipses = TRUE,
                title = "Variables e individuos") +
  scale_color_brewer(palette = "Set1")

# Validación cruzada
datos %>%
  select_if(is.numeric) %>%
  estim_ncp(ncp.min=0, ncp.max=5, # búsqueda entre 0 y 5 componentes
            method="GCV")

# Construcción de indicadores
indicadores = function(acp, k, alpha = 3){
  f = data.frame(acp$ind$coord[,1:k])
  for(j in 1:k){
    Q = quantile(f[,j], c(0.25, 0.75))
    RIC = diff(Q)
    a = min(min(f[,j]), Q[1] - alpha*RIC)
    b = max(Q[2] + alpha*RIC, max(f[,j]))
    f[,j] = (f[,j] - a)/(b-a)
  }
  colnames(f) = paste("I", 1:k, sep="")
  return(f)
}

# Primeras 3 componentes como indicadores
I = indicadores(acp, k=3)
head(I, 10) # primeras 10 filas

# Primer indicador
# Individuos con los 5 valores mínimos
slice_min(I, order_by = I1, n = 5)

# Individuos con los 5 valores máximos
slice_max(I, order_by = I1, n = 5)

# Distribución del 1er indicador
ggplot(I, aes(x = I1)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 7,
                 colour = 1, fill = "gray") +
  geom_density() +
  ylab("Densidad") + xlab("Dim1")

# Datos atípicos

# ACP con todas las componentes
acp = PCA(datos, scale.unit = TRUE, ncp = ncol(datos)-2,
          quali.sup = 1:2, graph = FALSE)

# Primeras componentes

# Estadística d2^2
d12 = function(acp, p1, alpha=0.05){
  F = acp$ind$coord # Componentes principales
  F1 = scale(F) # componentes estandarizadas
  d1i2 = apply(F1, 1, function(f1) sum(f1[1:p1]^2)) # distancia
  cuantil = qemp(1-alpha, d1i2) # cuantil empírico
  # Identificación de atípicos
  d12 = data.frame(Observación = 1:nrow(F),
                   d12 = d1i2,
                   Cuantil = cuantil, 
                   Atípico = d1i2>cuantil)
  d12
}

# Considerando las dos primeras componentes
est.d12 = d12(acp, p1=3, alpha=0.05) 

# Atípicos
dplyr::filter(est.d12, Atípico==TRUE)

ggplot(est.d12, aes(x=Observación, xend=Observación, y=0, yend=d12)) + 
  geom_segment() +
  geom_hline(yintercept=est.d12$Cuantil, linetype="dashed") +
  geom_text(aes(x=Observación, y=d12, label=Observación),
            cex = 2, vjust = -1) +
  ylab("Estadística")

# Últimas componentes

# Estadística d2^2
d22 = function(acp, q, alpha=0.05){
  F = acp$ind$coord # Componentes principales
  F1 = scale(F) # componentes estandarizadas
  p = ncol(F) # número de componentes
  d2i2 = apply(F1, 1, function(f1) sum(f1[(p-q+1):p]^2)) # distancia
  cuantil = qemp(1-alpha, d2i2) # cuantil empírico
  # Identificación de atípicos
  d22 = data.frame(Observación = 1:nrow(F),
                   d22 = d2i2,
                   Cuantil = cuantil, 
                   Atípico = d2i2>cuantil)
  d22
}

# Considerando la última componente
est.d22 = d22(acp, q=1, alpha=0.05) 

# Atípicos
dplyr::filter(est.d22, Atípico==TRUE)

ggplot(est.d22, aes(x=Observación, xend=Observación, y=0, yend=d22)) + 
  geom_segment() +
  geom_hline(yintercept=est.d22$Cuantil, linetype="dashed") +
  geom_text(aes(x=Observación, y=d22, label=Observación),
            cex = 2, vjust = -1) +
  ylab("Estadística")

# Considerando las dos últimas componentes
est.d22 = d22(acp, q=2, alpha=0.05) 

# Atípicos
dplyr::filter(est.d22, Atípico==TRUE)

ggplot(est.d22, aes(x=Observación, xend=Observación, y=0, yend=d22)) + 
  geom_segment() +
  geom_hline(yintercept=est.d22$Cuantil, linetype="dashed") +
  geom_text(aes(x=Observación, y=d22, label=Observación),
            cex = 2, vjust = -1) +
  ylab("Estadística")

# ACP robusto
acp.r = datos %>%
  select_if(is.numeric) %>%
  princomp(., cor = TRUE, scores = TRUE, 
           covmat = cov.rob(., method = "mve", cor = TRUE))

# Valores propios
v.propios = get_eigenvalue(acp.r)
colnames(v.propios) = c("v. propio", "inercia", "iner. acum.")
v.propios

# Histograma de valores propios (Scree Plot)
fviz_screeplot(acp.r) +
  xlab("Componentes")+
  ylab("% de varianza explicada")

# Gráfico de las componentes 1 y 2
fviz_pca_ind(acp.r, axes = c(1,2), geom = c("point","text"),
             col.ind = datos$Income, addEllipses = TRUE, 
             title = "Individuos") +
  scale_color_brewer(palette = "Set1")

# ACP mediante Projection-Pursuit 
acp.pp = datos %>%
  select_if(is.numeric) %>%
  PCAproj(k = 6, method = "mad", center = median,
          scale = mad, maxit = 30)
rownames(acp.pp$scores) = rownames(datos)

# Valores propios
v.propios = get_eigenvalue(acp.pp)
colnames(v.propios) = c("v. propio", "inercia", "iner. acum.")
v.propios

# Histograma de valores propios (Scree Plot)
fviz_screeplot(acp.pp)

# Gráfico de las componentes 1 y 2
fviz_pca_ind(acp.pp, axes = c(1,2), geom = c("point","text"), 
             col.ind = datos$Income, addEllipses = TRUE,
             title = "Individuos") +
  scale_color_brewer(palette = "Set1")

# Análisis de componentes principales con kernels

# Estandarización
Z = datos %>%
  select_if(is.numeric) %>%
  scale() %>%
  data.frame()

# ACP con kernels
acp.k = kpca(~., Z, kernel = "rbfdot")

# Objeto princomp
acp.k = list(sdev = eig(acp.k),
             loadings = cor(Z, pcv(acp.k), method = "kendall"),
             scores = pcv(acp.k),
             center = rep(0, ncol(Z)),
             scale = rep(1, ncol(Z)))
class(acp.k) = "princomp"

# Valores propios
v.propios = get_eigenvalue(acp.k)
colnames(v.propios) = c("v. propio", "inercia", "iner. acum.")
v.propios[1:ncol(Z),]

# Histograma de valores propios (Scree Plot)
fviz_screeplot(acp.k, ncp = ncol(Z))

# Gráfico de las componentes 1 y 2
rownames(acp.k$scores) = rownames(datos)
fviz_pca_ind(acp.k, axes = c(1,2), geom = c("point","text"),
             col.ind = datos$Income, addEllipses = TRUE,
             title = "Individuos") +
  scale_color_brewer(palette = "Set1")

# Datos faltantes

# Número de componentes para la reconstrucción
wdi %>%
  select_if(is.numeric) %>%
  estim_ncpPCA(ncp.min=0, ncp.max=5, # búsqueda entre 0 y 5 componentes
            method="Regularized")

# Imputación simple para PCA
imp = imputePCA(wdi, 
    scale = T, # estandariza los datos
    ncp = 1, # número de componentes a mostar (no el seleccionado)
    quali.sup = c(1,2)) # variables cualitativas de grupo

head(imp$completeObs, 10) # conjunto de datos imputados

acp = PCA(imp$completeObs, # datos imputados
          scale.unit = T, # estandariza los datos
          ncp = 4, # número de componentes a mostar (no el seleccionado)
          quali.sup = c(1,2), # variables cualitativas de grupo
          graph = FALSE)

# Valores propios y porcentajes de inercia
colnames(acp$eig) = c("v. propio", "inercia", "iner. acum.")
acp$eig

# Representación de las variables
# Componentes 1 y 2
fviz_pca_var(acp, axes = c(1,2), col.var = "cos2", 
             title = "Variables",repel = TRUE) +
  scale_color_gradient2(low="red", mid="blue", high="black", midpoint=.3)


# Representación de los individuos
# Componentes 1 y 2
fviz_pca_ind(acp, axes = c(1,2), geom = c("point","text"), 
             habillage = 2, addEllipses = FALSE, 
             title = "Individuos") +
  scale_color_brewer(palette = "Set1")

