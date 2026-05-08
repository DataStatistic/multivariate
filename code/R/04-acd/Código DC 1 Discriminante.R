
# Ejemplo Análisis Discriminante

library(readxl) # cargue del dataset
library(dplyr)  # manipulación del dataset
library(MASS, exclude="select") # Linear and Quadratic Discriminant Analysis
library(mda)    # Mixture and Flexible Discriminant Analysis
library(klaR)   # Regularized Discriminant Analysis       
library(nnet)   # Multinomial regression     
library(caret)  # Classification and Regression Training
library(earth)  # complemento

# Conjunto de datos
datos = read_excel("HOMA IR in overweight obesity.xlsx")

datos = datos %>%
  # variables numéricas y variable de clasificación
  select("Obesity category", # Sarcopenia: Normal, sarcpenia (Presarcopenia), Sarcopenia, severe sarcopenia (Severa),
         "Age", # Edad (en años cumplidos)
         "BMI", # Índice de masa corporal (en kg/m², Peso/Estatura(mts)^2)
         "Hand grip test", # Prueba de fuerza de agarre manual (en kg)
         "Gait speed", # Velocidad de la marcha (en m/s)
         "BIA", # Análisis de Impedancia Bioeléctrica (en kg/m²)
         "HOMA IR") %>% # Resistencia a la Insulina (adimensional)
  mutate(`Obesity category` = factor(`Obesity category`,
                                     levels = c("Overweight","Grade 1 Obesity","Grade 2 Obesity"),
                                     labels = c("Overweight","Grade 1","Grade 2"))) %>%
  rename(HGT = `Hand grip test`,
         Gait = `Gait speed`,
         IR = `HOMA IR`,
         Obesity = `Obesity category`) %>%
  na.omit()

# Variable de grupo a clasificar
datos$Obesity = factor(datos$Obesity)
table(datos$Obesity)
y = "Obesity"

# Predictores
xvars = setdiff(names(datos), y) ; xvars

# Partición estratificada entrenamiento/prueba
part = 0.70 # proporción muestra de entrenamiento
set.seed(123)
idx_train = createDataPartition(y=datos[[y]], p=part, list=FALSE) ; idx_train

train = datos[idx_train, ] # muestra de entrenamiento
test  = datos[-idx_train, ] # muestra de prueba

# Tamaños
table(datos[[y]])
table(train[[y]])
table(test[[y]])

# Revisión inicial
ng = table(train[[y]]) ; ng
p = length(xvars) ; p
ng > p # preferible

# Colinealidad (¿k < 30?)
R = cor(train[xvars]) ; R
kappa_R = kappa(R, exact = TRUE) ; kappa_R

# Estandarización usando solo entrenamiento
medias = sapply(train[xvars], mean)
desv   = sapply(train[xvars], sd)

train_st = train
test_st  = test

train_st[xvars] = as.data.frame(
  scale(train[xvars], center = medias, scale = desv))

test_st[xvars] = as.data.frame(
  scale(test[xvars], center = medias, scale = desv))

# Ajuste del análisis discriminante

# Fórmula
formula_ad = as.formula(paste(y, "~ .")) ; formula_ad

# función lda, qda, mda, fda y rda
ld = function(metodo = c("lda", "qda", "mda", "fda", "rda"), ...){
  metodo = match.arg(metodo)
  switch(
    metodo,
    lda = lda(...),
    qda = qda(...),
    mda = mda(...),
    fda = fda(...),
    rda = rda(...)
  )
}

# Método
metodo = "qda" # "lda", "qda", "mda", "fda", "rda"
mod_ad = ld(metodo, formula = formula_ad, data = train_st) 

# Validación cruzada K-fold estratificada
K = min(10, min(table(train_st[[y]]))) ; K ; # folds

set.seed(123)
folds = createFolds(train_st[[y]], k = K, returnTrain = TRUE)
ctrl = trainControl(method = "cv", number = K, index = folds, savePredictions = "final")
cv_ad = train(formula_ad, data = train_st, trControl = ctrl,
              method = metodo)

# Matriz de confusión
MC_cv_ad = table(Real = cv_ad$pred$obs, Predicho = cv_ad$pred$pred) ; MC_cv_ad

# Exactitud, mal clasificación, sensibilidad y exactitud balanceada
accuracy_cv_ad = sum(diag(MC_cv_ad)) / sum(MC_cv_ad) ; accuracy_cv_ad
tmc_cv_ad = 1 - accuracy_cv_ad ; tmc_cv_ad
sens_cv_ad = diag(MC_cv_ad) / rowSums(MC_cv_ad) ; sens_cv_ad
balanced_cv_ad = mean(sens_cv_ad) ; balanced_cv_ad

# Sensibilidad, Precisión y F1
sens_cv_ad = diag(MC_cv_ad) / rowSums(MC_cv_ad) ; sens_cv_ad # qué tanto detecta bien un grupo real
prec_cv_ad = diag(MC_cv_ad) / colSums(MC_cv_ad) ; prec_cv_ad # qué tan confiable es una asignación a cada grupo
f1_cv_ad = 2 * sens_cv_ad * prec_cv_ad / (sens_cv_ad + prec_cv_ad) ; f1_cv_ad # equilibrio entre sensibilidad y precisión
f1_macro_cv_ad = mean(f1_cv_ad, na.rm = TRUE) ; f1_macro_cv_ad # equilibrio entre sensibilidad y precisión general

# Predicción en prueba
if(is.element(metodo, c("lda", "qda", "rda"))) 
  pred_ad = predict(mod_ad, newdata = test_st)$class else
    pred_ad = predict(mod_ad, newdata = test_st)

# Evaluación final en prueba
MC_test_ad = table(Real = test_st[[y]], Predicho = pred_ad) ; MC_test_ad

# Exactitud, mal clasificación, sensibilidad y exactitud balanceada
accuracy_test_ad = sum(diag(MC_test_ad)) / sum(MC_test_ad) ; accuracy_test_ad
tmc_test_ad = 1 - accuracy_test_ad ; tmc_test_ad
sens_test_ad = diag(MC_test_ad) / rowSums(MC_test_ad) ; sens_test_ad
balanced_test_ad = mean(sens_test_ad) ; balanced_test_ad

# Sensibilidad, Precisión y F1
sens_test_ad = diag(MC_test_ad) / rowSums(MC_test_ad) ; sens_test_ad # qué tanto detecta bien un grupo real
prec_test_ad = diag(MC_test_ad) / colSums(MC_test_ad) ; prec_test_ad # qué tan confiable es una asignación a cada grupo
f1_test_ad = 2 * sens_test_ad * prec_test_ad / (sens_test_ad + prec_test_ad) ; f1_test_ad # equilibrio entre sensibilidad y precisión
f1_macro_test_ad = mean(f1_test_ad, na.rm = TRUE) ; f1_macro_test_ad # equilibrio entre sensibilidad y precisión general

# Resumen para comparar con otro método
resumen_ad = data.frame(
  Metodo = metodo,
  Accuracy_CV = accuracy_cv_ad,
  TMC_CV = tmc_cv_ad,
  Balanced_CV = balanced_cv_ad,
  Accuracy_Test = accuracy_test_ad,
  TMC_Test = tmc_test_ad,
  Balanced_Test = balanced_test_ad
)

resumen_ad

# Datos completos etiquetados

# Estandarización usando toda la muestra etiquetada
medias_final = sapply(datos[xvars], mean)
desv_final   = sapply(datos[xvars], sd)

datos_final = datos

datos_final[xvars] = as.data.frame(
  scale(datos[xvars], center = medias_final, scale = desv_final))

# Regla discriminante final
# función lda, qda, mda, fda y rda
mod_ad_final = ld(metodo, formula = formula_ad, data = datos_final)

# Nuevos individuos
nuevos = data.frame(
  Age  = c(30, 50, 60),
  BMI  = c(41.5, 31.2, 30.5),
  HGT  = c(45.1, 17.4, 22.8),
  Gait = c(1.0, 1.0, 0.8),
  BIA  = c(6.7, 5.3, 7.9),
  IR   = c(3.0, 1.7, 2.1)
)

# Estandarización
nuevos_st = nuevos
nuevos_st[xvars] = as.data.frame(
  scale(nuevos[xvars], center = medias_final, scale = desv_final))

# Asignación de los nuevos individuos
if(is.element(metodo, c("lda", "qda", "rda"))) 
  pred_nuevos = predict(mod_ad_final, newdata = nuevos_st)$class else
    pred_ad = predict(mod_ad_final, newdata = nuevos_st)
pred_nuevos

