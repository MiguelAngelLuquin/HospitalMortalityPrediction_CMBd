# MODELO CON INTERACCIONES DE EDAD Y SEXO

# --- CARGAR LIBRERÍAS NECESARIAS ---
library(readr)      # Lectura de archivos CSV y delimitados
library(dplyr)      # Manipulación y transformación de datos
library(tidyr)      # Transformación de formatos (wide/long)
library(purrr)      # Funciones funcionales (map2)
library(stringr)    # Manipulación de cadenas de texto
library(lubridate)  # Manejo de fechas
library(caret)      # Partición de datos para entrenamiento y test
library(pROC)       # Curvas ROC y AUC
library(ggplot2)    # Visualización gráfica
library(scales)     # Formatos para ejes y etiquetas
library(binom)      # Intervalos de confianza para proporciones
library(pracma)     # Integración numérica (trapz)
library(PRROC)      # Curva Precisión-Recall y AUC-PR

# --- FUNCIONES AUXILIARES ---

# Expandir rangos de códigos tipo "A01-A05" en secuencia individual
expand_codigos <- function(inicio, fin) {
  if (inicio == fin) {
    return(inicio)
  } else {
    letra <- substr(inicio, 1, 1)
    num_inicio <- as.integer(substr(inicio, 2, 3))
    fin_num_str <- substr(fin, 2, 3)
    num_fin <- if (str_detect(fin_num_str, "[A-Za-z]")) 9 else as.integer(fin_num_str)
    codigos <- str_c(letra, str_pad(num_inicio:num_fin, 2, pad = "0"))
    return(codigos)
  }
}

# --- DIRECTORIO DE TRABAJO ---
setwd("C:/Users/luqui/Desktop/GitHub_MLuquin/HospitalMortalityPrediction_CMBd")

# --- CARGA Y PROCESADO DE TABLAS AUXILIARES ---

# Secciones CIE10 con expansión de rangos de códigos
Secciones_CIE10 <- read_csv("Tablas/Secciones_CIE10.csv")

tabla_secciones <- Secciones_CIE10 %>% 
  separate(CIE10, into = c("inicio", "fin"), sep = "-") %>%
  mutate(fin = if_else(is.na(fin), inicio, fin)) %>%
  mutate(codigo = map2(inicio, fin, expand_codigos)) %>%
  unnest(codigo) %>%
  distinct(codigo, .keep_all = TRUE) %>%
  select(id, codigo, Seccion)

# --- CARGA Y PREPARACIÓN DE DATOS CMBD ---

CMBDS <- read_delim("Tablas/CMBD_HOS_ANONIMO_20160101_20161231.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(SEXO != 3)  # Eliminar sexo inválido

# Columnas de diagnóstico y procedimiento
cols_poac <- grep("^POAC\\d+", names(CMBDS), value = TRUE)
cols_diagnostico <- grep("^C\\d+", names(CMBDS), value = TRUE)

# Tabla base con id y columnas diagnósticas
tabla_base <- CMBDS %>%
  select(all_of(cols_diagnostico), all_of(cols_poac), CIPA_anonimo) %>%
  mutate(id = CIPA_anonimo) %>%
  select(-CIPA_anonimo)

# Limpieza de códigos diagnósticos (eliminar decimales)
tabla_base[cols_diagnostico] <- lapply(tabla_base[cols_diagnostico], function(col) {
  sub("\\..*", "", as.character(col))
})

# Convertir a formato largo y mapear secciones
tabla_larga <- tabla_base %>%
  pivot_longer(
    cols = starts_with("C") | starts_with("POAC"),
    names_to = c(".value", "num"),
    names_pattern = "(C|POAC)(\\d+)"
  ) %>%
  select(-num) %>%
  filter(!is.na(C), C != "", !is.na(POAC), POAC != "") %>%
  left_join(tabla_secciones %>% select(id_codigo = id, codigo), by = c("C" = "codigo")) %>%
  mutate(diagnostico = id_codigo) %>%
  filter(POAC %in% c("S", "N")) %>%
  drop_na() %>%
  distinct(id, diagnostico, POAC) %>%
  mutate(diagnostico = paste0(POAC, diagnostico)) %>%
  select(-POAC)

# Crear variables dummy para diagnóstico + procedimiento
tabla_dummies <- tabla_larga %>%
  mutate(valor = 1) %>%
  distinct(id, diagnostico, .keep_all = TRUE) %>%
  pivot_wider(
    id_cols = id,
    names_from = diagnostico,
    values_from = valor,
    values_fill = 0
  )

# --- VARIABLES DEMOGRÁFICAS Y UNIÓN FINAL ---

tabla_variables <- CMBDS %>%
  mutate(
    FECING = dmy_hm(FECING),
    FECALT = dmy_hm(FECALT)
  ) %>%
  transmute(
    id = CIPA_anonimo,
    FALLECIMIENTO = if_else(TIPALT == 4, 1, 0),
    FECNAC = dmy(FECNAC),
    edad = floor(interval(FECNAC, ymd("2016-01-01")) / years(1)),
    SEXO = SEXO - 1,
    tiempo_estancia_dias = as.numeric(difftime(FECALT, FECING, units = "days"))
  ) %>%
  select(-FECNAC)

# Unir datos demográficos y diagnósticos
tabla_modelo <- tabla_dummies %>%
  left_join(tabla_variables, by = "id") %>%
  na.omit() %>%
  filter(edad >= 20)

# Variables seleccionadas para modelo
significativas <- c(
  "S271", "S218", "S200", "S120", "S114", "S102", "S198", "S75", "S159", "S277",
  "S143", "S24", "S37", "S144", "S146", "S129", "S232", "S57", "S21", "S265",
  "S44", "N228", "N126", "S61", "S156", "S122", "S112", "S128", "S13", "S124",
  "N159", "N46", "S202", "S106", "S77", "S139", "S81", "S115", "S104", "N106",
  "N202", "S228", "S204", "S116", "N52", "N77", "S58", "N201", "S79", "S16",
  "S40", "S160", "S205", "S212", "S54", "N96", "S87", "S25", "S82", "S31",
  "N120", "S150", "S163", "S208", "S47", "S158", "N204", "S147", "S33", "S4",
  "N218", "S157", "S63", "S38", "S125", "N103", "S32", "N109", "S68", "S197",
  "S96", "N115", "S30", "S23", "S91", "N156", "N129", "N81", "N128", "N108",
  "S119", "S210", "N4", "S73", "S110", "N82", "S90", "N116", "S123", "S164",
  "S209", "N47", "S223", "S185", "N223", "S132", "N40", "N87", "S245", "N178",
  "N72", "S273", "N22", "S190", "SEXO", "edad2", "tiempo_estancia_dias2"
)

tabla_modelo$edad2 <- tabla_modelo$edad^2
tabla_modelo$tiempo_estancia_dias2 <- tabla_modelo$tiempo_estancia_dias^2

tabla_modelo <- tabla_modelo[, c("FALLECIMIENTO", significativas)]

# --- DIVISIÓN ENTRENAMIENTO / TEST ---

set.seed(777)  # Reproducibilidad
trainIndex <- createDataPartition(tabla_modelo$FALLECIMIENTO, p = 0.7, list = FALSE)
datos_train <- tabla_modelo[trainIndex, ]
datos_test  <- tabla_modelo[-trainIndex, ]

datos_train$SEXO <- factor(datos_train$SEXO)
datos_test$SEXO  <- factor(datos_test$SEXO)

# --- AJUSTE DEL MODELO LOGÍSTICO CON INTERACCIONES ---

# Variables predictoras excluyendo 'edad2' y variable respuesta 'FALLECIMIENTO'
vars <- setdiff(significativas, c("edad2", "FALLECIMIENTO"))

# Crear términos de interacción con edad2
interacciones <- paste0(vars, ":edad2")

# Combinar términos principales y de interacción
terminos <- c(vars, "edad2", interacciones)

# Crear fórmula para el modelo logístico
formula_modelo <- as.formula(paste("FALLECIMIENTO ~", paste(terminos, collapse = " + ")))
print(formula_modelo)

# Ajustar modelo logístico con datos_test
modelo_logit_interacciones <- glm(formula_modelo, data = datos_test, family = binomial)
summary(modelo_logit_interacciones)


# --- EVALUACIÓN DEL MODELO ---

# Predecir probabilidades en test
prob_test <- predict(modelo_logit_interacciones, newdata = datos_test, type = "response")

# Curva ROC y AUC
roc_obj <- roc(datos_test$FALLECIMIENTO, prob_test)
plot(roc_obj, main = "Curva ROC")
cat("AUC:", auc(roc_obj), "\n")

# Umbral óptimo según Youden's J
umbral_optimo <- coords(roc_obj, "best", best.method = "youden")["threshold"]
cat("Umbral óptimo (Youden's J):", umbral_optimo, "\n")
umbral_optimo <- as.numeric(umbral_optimo)

# Predicción binaria usando umbral óptimo
predicciones_binarias <- ifelse(prob_test >= umbral_optimo, 1, 0)

# Matriz de confusión
conf_matrix <- table(Predicho = predicciones_binarias, Real = datos_test$FALLECIMIENTO)
print(conf_matrix)

# Calcular métricas: Sensibilidad, Especificidad y Accuracy
TP <- conf_matrix["1", "1"]
TN <- conf_matrix["0", "0"]
FP <- conf_matrix["1", "0"]
FN <- conf_matrix["0", "1"]

sensibilidad <- TP / (TP + FN)
especificidad <- TN / (TN + FP)
accuracy <- (TP + TN) / sum(conf_matrix)

cat(sprintf("Sensibilidad (Recall): %.3f\n", sensibilidad))
cat(sprintf("Especificidad: %.3f\n", especificidad))
cat(sprintf("Accuracy: %.3f\n", accuracy))


# --- DISTRIBUCIÓN DE LAS PREDICCIONES ---

datos_test$ygorro <- prob_test

# Densidad de probabilidad (sin transformación)
ggplot(datos_test, aes(x = ygorro, fill = as.factor(FALLECIMIENTO))) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = umbral_optimo, linetype = "dashed", color = "blue") +
  labs(title = "Distribución de probabilidades predichas por fallecimiento",
       x = "Probabilidad predicha",
       y = "Densidad",
       fill = "Fallecimiento") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon")) +
  theme_minimal()

# Densidad con transformación raíz cuarta
ggplot(datos_test, aes(x = ygorro^(1/4), fill = as.factor(FALLECIMIENTO))) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = umbral_optimo^(1/4), linetype = "dashed", color = "blue") +
  labs(title = "Distribución de raíz cuarta de probabilidades predichas",
       x = "Raíz cuarta de probabilidad",
       y = "Densidad",
       fill = "Fallecimiento") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon")) +
  theme_minimal()

# Cálculo del índice de Jaccard basado en densidades de probabilidad

grupo0 <- datos_test$ygorro[datos_test$FALLECIMIENTO == 0]
grupo1 <- datos_test$ygorro[datos_test$FALLECIMIENTO == 1]

x <- seq(min(datos_test$ygorro), max(datos_test$ygorro), length.out = 10000)
dens0 <- density(grupo0, from = min(x), to = max(x), n = length(x))
dens1 <- density(grupo1, from = min(x), to = max(x), n = length(x))

min_dens <- pmin(dens0$y, dens1$y)
max_dens <- pmax(dens0$y, dens1$y)

ovl_area <- trapz(x, min_dens)
union_area <- trapz(x, max_dens)
jaccard_index <- ovl_area / union_area

cat("Área de solapamiento (intersección):", ovl_area, "\n")
cat("Área de unión:", union_area, "\n")
cat("Índice de Jaccard:", jaccard_index, "\n")


# --- CURVA PRECISIÓN-RECALL ---

pr <- pr.curve(scores.class0 = prob_test[datos_test$FALLECIMIENTO == 1],
               scores.class1 = prob_test[datos_test$FALLECIMIENTO == 0],
               curve = TRUE)

cat(sprintf("AUC de la curva Precisión-Recall: %.4f\n", pr$auc.integral))

plot(pr,
     main = "Curva Precisión-Recall",
     color = "darkred",
     lwd = 2,
     auc.main = FALSE)


# --- LOG-LOSS (Cross-Entropy) ---

eps <- 1e-15
prob_clipped <- pmin(pmax(prob_test, eps), 1 - eps)

log_loss <- -mean(
  datos_test$FALLECIMIENTO * log(prob_clipped) +
    (1 - datos_test$FALLECIMIENTO) * log(1 - prob_clipped)
)

cat(sprintf("Log-Loss (Cross-Entropy): %.4f\n", log_loss))


# --- CURVA DE CALIBRACIÓN ---

brier_score <- mean((prob_test - datos_test$FALLECIMIENTO)^2)
cat(sprintf("Brier Score: %.4f\n", brier_score))

# Binning con cortes regulares
bins <- cut(prob_test, breaks = seq(0, 1, by = 0.05), include.lowest = TRUE)

calibration_data <- data.frame(bin = bins,
                               y_real = datos_test$FALLECIMIENTO,
                               prob = prob_test) %>%
  group_by(bin) %>%
  summarise(media_predicha = mean(prob),
            frecuencia_observada = mean(y_real),
            n = n()) %>%
  ungroup()

# Gráfico calibración simple
ggplot(calibration_data, aes(x = media_predicha, y = frecuencia_observada)) +
  geom_point(aes(size = n), color = "darkred", alpha = 0.7) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Curva de calibración del modelo",
       x = "Probabilidad media predicha",
       y = "Frecuencia real de fallecimientos",
       size = "N casos en bin") +
  theme_minimal()

# Curva calibración con IC, usando bins más finos en rangos bajos
breaks_finos <- seq(0, 0.15, by = 0.005)
breaks_normales <- seq(0.15, 0.5, by = 0.05)
breaks <- sort(unique(c(breaks_finos, breaks_normales)))

idx <- prob_test <= 0.5

bins <- cut(prob_test[idx], breaks = breaks, include.lowest = TRUE, right = TRUE)

calibration_data <- data.frame(bin = bins,
                               y_real = datos_test$FALLECIMIENTO[idx],
                               prob = prob_test[idx]) %>%
  group_by(bin) %>%
  summarise(media_predicha = mean(prob),
            frecuencia_observada = mean(y_real),
            n = n()) %>%
  rowwise() %>%
  mutate(ci = list(binom.confint(frecuencia_observada * n, n, method = "wilson"))) %>%
  unnest_wider(ci, names_sep = "_") %>%
  ungroup()

# Gráfico calibración con IC y zoom
ggplot(calibration_data, aes(x = media_predicha, y = frecuencia_observada)) +
  geom_point(aes(size = n), color = "darkred", alpha = 0.7) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "darkred", alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.8), expand = c(0, 0)) +
  labs(title = "Curva de calibración del modelo (hasta 50%) con IC",
       x = "Probabilidad media predicha",
       y = "Frecuencia real de fallecimientos",
       size = "N casos en bin") +
  theme_minimal()

ggplot(calibration_data, aes(x = media_predicha, y = frecuencia_observada)) +
  geom_point(aes(size = n), color = "darkred", alpha = 0.7) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "darkred", alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.15), expand = c(0, 0)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.3), expand = c(0, 0)) +
  labs(title = "Curva de calibración del modelo (zoom 0-15%) con IC",
       x = "Probabilidad media predicha",
       y = "Frecuencia real de fallecimientos",
       size = "N casos en bin") +
  theme_minimal()


# --- GRÁFICOS POR EDAD ---

# Crear carpeta para guardar resultados
if (!dir.exists("ResultadosModelo4")) dir.create("ResultadosModelo4")

grupos_edad <- seq(20, 100, by = 10)

for (i in 1:(length(grupos_edad) - 1)) {
  edad_min <- grupos_edad[i]
  edad_max <- grupos_edad[i + 1]
  
  # Subconjunto para grupo de edad
  datos_grupo <- subset(datos_test, edad2 >= edad_min^2 & edad2 < edad_max^2)
  if (nrow(datos_grupo) == 0) next
  
  # Gráfico de densidad para grupo de edad
  grafico <- ggplot(datos_grupo, aes(x = ygorro^(1/4), fill = as.factor(FALLECIMIENTO))) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon")) +
    labs(
      title = paste0("Distribución de (ygorro)^(1/4) por fallecimiento\nEdad ", edad_min, "-", edad_max),
      x = "Raíz cuarta de probabilidad",
      y = "Densidad",
      fill = "Fallecimiento"
    ) +
    theme_minimal()
  
  # Guardar gráfico en carpeta
  ggsave(filename = paste0("ResultadosModelo4/Diagram", edad_min, "-", edad_max, ".png"),
         plot = grafico, width = 10, height = 6, bg = "white")
}