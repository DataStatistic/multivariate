
# Métodos de reducción de dimensionalidad
# Análisis de correlación canónica (ACC)

library(readxl)    # importar datos de excel
library(dplyr)     # manipulación de datos
library(tibble)    # manipulación de datos
library(stringr)   # manipulación de datos
library(tidyr)     # manipulación de datos
library(CCP)       # pruebas de significancia para acc
library(CCA)       # Análisis de correlación canónica

options(digits = 3)

# Datos: Índice de Capacidad Estadística
# Territorial (ICET) 2023.
# https://www.dane.gov.co/files/operaciones/ICET/anex-ICET-2023.xlsx

# Por departamento
icet_dpto = read_excel("anex-ICET-2023.xlsx",
                       range = "A8:Q40", # resultados por dimensión y subdimensiones
                       sheet = "ICET_C1_Dptos") %>% # resultados por dptos
  mutate(`...1` = substr(`...1`, 1, 13)) %>%
  column_to_rownames("...1") %>% # nombres de dpto
  dplyr::select(-"...2") %>%
  rename(ICET = "...3", # Índice de Capacidad Estadística Territorial
         # Resultado por dimensión
         EI = "Entorno Institucional",
         INF = "Infraestructura",
         ME = "Metodología estadística",
         AU = "Accesibilidad y uso",
         # Resultado por subdimensiones
         # - Dimensión entorno institucional
         MI = "Marco institucional",
         GC = "Gestión del conocimiento",
         # - Dimensión infraestructura
         PSRH = "Percepción suficiencia de recursos humanos, tecnológicos y financieros",
         UHP = "Uso de herramientas de procesamiento",
         # - Dimensión metodología estadística
         FI = "Formulación de indicadores",                                           
         ARA = "Aprovechamiento de Registros Administrativos",
         OE = "Operaciones estadísticas",
         # - Dimensión accesibilidad y uso de la información estadística
         Disp = "Disponibilidad",
         Acc = "Accesibilidad",
         Uso = "Uso")

# Por municipios
icet_ciudades = read_excel("anex-ICET-2023.xlsx",
   range = "A8:Q28", # resultados por dimensión y subdimensiones
   sheet = "ICET_C2_Ciudades")

icet_pdet = read_excel("anex-ICET-2023.xlsx",
   range = "A8:Q173", # resultados por dimensión y subdimensiones
   sheet = "ICET_C3_PDET")

icet_resto = read_excel("anex-ICET-2023.xlsx",
   range = "A8:Q904", # resultados por dimensión y subdimensiones
   sheet = "ICET_C4_Resto_país")

icet_munic = bind_rows(icet_ciudades, icet_pdet, icet_resto) %>%
  dplyr::select(-c("...1","...2")) %>%
  rename(ICET = "...3", # Índice de Capacidad Estadística Territorial
         # Resultado por dimensión
         EI = "Entorno Institucional",
         INF = "Infraestructura",
         ME = "Metodología estadística",
         AU = "Accesibilidad y uso",
         # Resultado por subdimensiones
         # - Dimensión entorno institucional
         MI = "Marco institucional",
         GC = "Gestión del conocimiento",
         # - Dimensión infraestructura
         PSRH = "Percepción suficiencia de recursos humanos, tecnológicos y financieros",
         UHP = "Uso de herramientas de procesamiento",
         # - Dimensión metodología estadística
         FI = "Formulación de indicadores",                                           
         ARA = "Aprovechamiento de Registros Administrativos",
         OE = "Operaciones estadísticas",
         # - Dimensión accesibilidad y uso de la información estadística
         Disp = "Disponibilidad",
         Acc = "Accesibilidad",
         Uso = "Uso")

# Estandarización
# Para departamentos
icet_z = icet_dpto %>%
  mutate(across(where(is.numeric), \(x) (x-mean(x))/sd(x)))

# Para municipios (quitar #)
icet_z = icet_munic %>%
  mutate(across(where(is.numeric), \(x) (x-mean(x))/sd(x)))

# Grupos de variables
X = icet_z %>%
  # Entorno institucional e infraextructura
  dplyr::select(MI, GC, PSRH, UHP)
  
Y = icet_z %>%
  # Metodología estadística, y accesibilidad y uso de la información estadística
  dplyr::select(FI, ARA, OE, Disp, Acc, Uso)

# Análisis de correlación canónica
acc = cc(X, Y)
acc$cor  # correlaciones canónicas

# Matriz de coeficientes canónicos (¿>0.3?)
acc$xcoef  # para X
acc$ycoef  # para Y

# Pruebas de significancia de los coeficientes de correlación canónica
pval = p.asym(acc$cor, N = nrow(X), 
              p = ncol(X), q = ncol(Y),
              tstat = "Wilks")
round(pval$p.value, 3)
