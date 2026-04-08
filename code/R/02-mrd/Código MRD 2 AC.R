
# Métodos de reducción de dimensionalidad
# Aálisis de correspondencias

library(readxl)     # importar datos de excel
library(dplyr)      # manipulación de datos
library(tibble)     # manipulación de datos
library(ggplot2)    # gráficos avanzados
library(FactoMineR) # métodos de reducción de dimensionalidad
library(factoextra) # gráficos de mrd
library(expm)       # matriz raíz cuadrada
library(nnet)       # matriz indicadora

options(digits = 3)

# Datos trabajadores
datos = read_xlsx("Trabajadores.xlsx")
View(datos)
dim(datos)

# Análisis de correspondencias simple

# Tabla de contingencia
N = datos %>%
  dplyr::select(Estrato, Situación) %>%
  table()
N

# Tabla de frecuencias relativas
P = prop.table(N) ; P*100

# Función DVSG para AC
dvsg = function(P){
  s = min(dim(P)) - 1
  pf = rowSums(P) 
  pc = colSums(P)
  M = sqrtm(solve(diag(pf))) 
  W = sqrtm(solve(diag(pc))) 
  Z = (P - pf%*%t(pc))
  dvs = svd(M%*%Z%*%W)
  dvsg = list(d = dvs$d[1:s],
              u = solve(M)%*%dvs$u[,1:s],
              v = solve(W)%*%dvs$v[,1:s])
  return(dvsg)
}

# Descomposición en valores singulares generalizada
dvs = dvsg(P)

# Valores propios
lamb = dvs$d^2 ; lamb

# Matriz Delta
D = diag(dvs$d) ; D

# Vectores propios filas
U = dvs$u ; U

# Vectores propios columnas
V = dvs$v ; V

# Dimensiones fila
pf = rowSums(P) 
Dr = diag(pf)
F = solve(Dr)%*%U%*%D
colnames(F) = paste("Dim", 1:ncol(F), sep = "")
rownames(F) = rownames(P)
F

# Dimensiones columna
pc = colSums(P)
Dc = diag(pc) 
G = solve(Dc)%*%V%*%D
colnames(G) = paste("Dim", 1:ncol(G), sep = "")
rownames(G) = colnames(P)
G

# Representación de filas y columnas
# Primeras dos dimensiones
ggplot(NULL, aes(x = Dim1, y = Dim2)) +
  geom_point(data=F, col = "red") +
  geom_point(data=G, col = "blue") +
  geom_text(data = F, vjust = -.5, size = 4, 
            label = rownames(F), col = "red") +
  geom_text(data = G, vjust = -.5, size = 4, 
            label = rownames(G), col = "blue") +
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2)

# cosenos cuadrados de las filas
cos2f = t(apply(F, 1, function(x) x^2/sum(x^2)))
cos2f

# cosenos cuadrados de las columnas
cos2c = t(apply(G, 1, function(x) x^2/sum(x^2)))
cos2c

# Contribuciones de las filas
ctrf = apply(F, 2, function(x) x^2/sum(x^2))
round(ctrf*100, 1)

# Contribuciones de las columnas
ctrc = apply(G, 2, function(x) x^2/sum(x^2))
round(ctrc*100, 1)

# Inercia de cada dimensión
inercia = lamb/sum(lamb)*100 ; inercia

# Inercia total
sum(lamb) * sum(N) # = chisq.test(N)$statistic

# Uso de la función CA
ac = CA(N, ncp = 2, graph = FALSE)

# Valores propios y porcentajes de inercia
colnames(ac$eig) = c("v. propio", "inercia", "iner. acum.")
ac$eig

# Histograma de valores propios (Scree Plot)
fviz_screeplot(ac) + 
  xlab("Dimensiones") + 
  ylab("% de varianza explicada")

# Coordenadas de las filas y columnas
ac$row$coord # filas
ac$col$coord # columnas

# Representación de filas y columnas
fviz_ca_biplot(ac, axes = c(1,2), arrows = c(FALSE, FALSE),
               title = "Modalidades", repel = TRUE)

# Cosenos cuadrados
ac$row$cos2 # filas
ac$col$cos2 # columnas

# Contribuciones
ac$row$contrib # filas
ac$col$contrib # columnas

# Análisis de correspondencias múltiple

# Selección de variables categóricas
datos.acm = datos %>%
  dplyr::select(Estrato, Situación, Créditos)

# Matriz indicadora 
Z = as.matrix(with(datos.acm, bind_cols(class.ind(Estrato), class.ind(Situación), class.ind(Créditos))))

head(Z, 10)

# Matriz de probabilidad
Y = Z/nrow(Z)

# Perfiles
pf = rowSums(Y) 
pc = colSums(Y)
M = sqrtm(solve(diag(pf))) 
W = sqrtm(solve(diag(pc))) 

# DVS
dvs = svd(M%*%(Y - pf%*%t(pc))%*%W)

# Valores propios
lamb = dvs$d[2:8]^2 ; lamb

# Matriz Delta
D = diag(dvs$d[2:8]) ; D

# Vectores propios de los individuos
U = dvs$u[,2:8] ; head(U, 10) # primeras 10 filas

# Vectores propios de las modalidades
V = dvs$v[,2:8] ; V

# Dimensiones de los individuos
F = data.frame(M%*%U%*%D)
colnames(F) = paste("Dim", 1:ncol(F), sep = "")
rownames(F) = rownames(datos.acm)
head(F, 10) # primeras 10 filas

# Dimensiones de las modalidades
G = data.frame(W%*%V%*%D)
colnames(G) = paste("Dim", 1:ncol(G), sep = "")
rownames(G) = colnames(Y)
G

# Representación de los individuos
# Primeras dos dimensiones
ggplot(F, aes(x = Dim1, y = Dim2)) +
  geom_point() +
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_text(vjust = -.5, size = 3, label = rownames(F))

# Representación de las modalidades
# Primeras dos dimensiones
ggplot(G, aes(x = Dim1, y = Dim2)) +
  geom_point() +
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_text(vjust = -.5, size = 3, label = rownames(G))

# Matriz de Burt
B = t(Z)%*%Z
B

# Tabla de frecuencias relativas
P = prop.table(B) ; P

# DVSG
dvs = dvsg(P)

# Valores propios
lamb = dvs$d^2 ; lamb[1:7]

# Matriz Delta
D = diag(dvs$d) ; D

# Vectores propios filas
U = dvs$u ; U[,1:7]

# Dimensiones fila
pf = rowSums(P) 
Dr = diag(pf)
F = solve(Dr)%*%U%*%D
colnames(F) = paste("Dim", 1:ncol(F), sep = "")
rownames(F) = rownames(P)
F[,1:7]

# Representación de las modalidades
# Primeras dos dimensiones
ggplot(NULL, aes(x = Dim1, y = Dim2)) +
  geom_point(data=F) +
  geom_text(data = F, vjust = -.5, size = 4, 
            label = rownames(F)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2)

# Uso de la función MCA
acm = MCA(datos.acm, ncp = 5, method = "Indicator", graph = FALSE)

# Valores propios y porcentajes de inercia
colnames(acm$eig) = c("v. propio", "inercia", "iner. acum.")
acm$eig

# Histograma de valores propios (Scree Plot)
fviz_screeplot(acm) + 
  xlab("Dimensiones") + 
  ylab("% de varianza explicada")

# Coordenadas de los individuos
acm$ind$coord

# Representación de los individuos
fviz_mca_ind(acm, axes = c(1,2), title = "Individuos", col.ind = 1)

# Coordenadas de las modalidades de las variables
acm$var$coord

# Representación de las modalidades
col.var = factor(rep(1:3, c(4,3,3))) # identificación de variables
fviz_mca_var(acm, axes = c(1,2), title = "Modalidades",
             repel = TRUE, col.var = col.var, mean.point = FALSE) +
  theme(legend.position="none")

# Cosenos cuadrados
round(acm$var$cos2, 3)

# Contribuciones
round(acm$var$contrib, 3)

