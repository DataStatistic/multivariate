
# Métodos de reducción de dimensionalidad
# Aálisis Factorial

library(readxl)       # importar datos de excel
library(dplyr)        # manipulación de datos
library(lavaan)       # análisis factorial confirmatorio
library(semPlot)      # visualización del afc
library(psych)        # afc

options(digits = 3)

# Conjunto de datos
marca = read_xlsx("Brand.xlsx")
View(marca)

# Análisis Factorial Exploratorio
af = marca %>%
  dplyr::select(-(1:3)) %>%
  factanal(factors = 3, scores = "regression", rotation = "none")

af

print(af, cutoff = .3)

# Rotaciones (varimax)
af = marca %>%
  dplyr::select(-(1:3)) %>%
  factanal(factors = 4, scores = "regression", rotation = "varimax")

print(af, cutoff = .5)

# Rotaciones (promax)
af = marca %>%
  dplyr::select(-(1:3)) %>%
  factanal(factors = 4, scores = "regression", rotation = "promax")

print(af, cutoff = .5)

# Gráfico
af = marca %>%
  dplyr::select(-(1:3)) %>%
  fa(nfactors = 4, rotate = "promax")

fa.diagram(af)
modelo = "
  F1 =~ CS1 + CS2 + CS3 + CS4 + CS5
  F2 =~ BP1 + BP2 + BP3 + BP4
  F3 =~ BA1 + BA2 + BA3 + PI1
  F4 =~ PI2 + PI3 + PI4
"

ajuste = lavaan::cfa(modelo, data = marca, std.lv = TRUE) 
semPaths(ajuste, what = "std", layout = "tree", edge.label.cex = 0.8)

# Análisis Factorial Confirmatorio

# Modelo teórico
modelo = "
  CS =~ CS1 + CS2 + CS3 + CS4 + CS5
  BP =~ BP1 + BP2 + BP3 + BP4
  BA =~ BA1 + BA2 + BA3
  PI =~ PI1 + PI2 + PI3 + PI4
"

# Ajuste del modelo
ajuste = lavaan::cfa(modelo, data = marca, std.lv = TRUE) 

summary(ajuste, fit.measures = TRUE, standardized = TRUE)

# Chi-square test: idealmente no significativa
# Comparative Fit Index (CFI): > 0.95
# Tucker-Lewis Index (TLI): > 0.95
# Error cuadrático medio de aproximación (RMSEA): < 0.06
# Residuo estandarizado cuadrático medio (SRMR): < 0.08
# Latent Variables:
## Std.lv (Carga factorial estandarizada) 
## Std.all (correlación entre el ítem y su factor)
# Covariances:
## Std.all: Correlación entre los factores latentes
# Variances:
## Std.all: Proporción de varianza no explicada por el factor

# Diagrama del modelo
semPaths(ajuste, what = "std", layout = "tree", edge.label.cex = 0.8)

