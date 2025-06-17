#### CARGA DE LIBRERÍAS ####
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(purrr)
library(pROC)
library(caret)
library(pheatmap)

#### FUNCIÓN AUXILIAR PARA EXPANDIR INTERVALOS DE CÓDIGOS CIE10 ####
expand_codigos <- function(inicio, fin) {
  if (inicio == fin) {
    return(inicio)
  } else {
    letra <- substr(inicio, 1, 1)
    num_inicio <- as.integer(substr(inicio, 2, 3))
    fin_num_str <- substr(fin, 2, 3)
    if (str_detect(fin_num_str, "[A-Za-z]")) {
      num_fin <- 9
    } else {
      num_fin <- as.integer(fin_num_str)
    }
    codigos <- str_c(letra, str_pad(num_inicio:num_fin, 2, pad = "0"))
    return(codigos)
  }
}

#### CARGA Y PREPARACIÓN DE TABLA DE SECCIONES CIE10 ####

setwd("C:/Users/luqui/Desktop/IACS/MuerteHospitalariaModelo")

Secciones_CIE10 <- read_csv("Tablas/Secciones_CIE10.csv")

tabla_secciones <- Secciones_CIE10 %>% 
  separate(CIE10, into = c("inicio", "fin"), sep = "-") %>%
  mutate(fin = if_else(is.na(fin), inicio, fin))

tabla_secciones_exp <- tabla_secciones %>%
  mutate(codigo = map2(inicio, fin, expand_codigos)) %>%
  unnest(codigo) %>% 
  dplyr::select(id, codigo, Seccion) %>%
  distinct(codigo, .keep_all = TRUE)

#### CARGA Y PREPARACIÓN DE DATOS CMBD ####

CMBDS <- read_delim("Tablas/CMBD_HOS_ANONIMO_20160101_20161231.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(SEXO != 3)  # Eliminar valores no válidos

cols_diagnostico <- grep("^C\\d+", names(CMBDS), value = TRUE)

tabla_base <- CMBDS %>%
  dplyr::select(all_of(cols_diagnostico), CIPA_anonimo) %>%
  mutate(id = CIPA_anonimo)

# Limpiar códigos de diagnóstico
tabla_base[cols_diagnostico] <- lapply(tabla_base[cols_diagnostico], function(col) {
  col <- as.character(col)
  sub("\\..*", "", col)
})

tabla_base <- tabla_base %>% dplyr::select(-CIPA_anonimo)

#### TRANSFORMACIÓN A FORMATO LARGO Y ASIGNACIÓN DE SECCIONES ####

tabla_larga <- tabla_base %>%
  pivot_longer(
    cols = all_of(cols_diagnostico),
    names_to = "diagnostico_col",
    values_to = "diagnostico"
  ) %>%
  filter(!is.na(diagnostico), diagnostico != "") %>%
  dplyr::select(-diagnostico_col)

tabla_larga <- tabla_larga %>%
  left_join(tabla_secciones_exp %>% dplyr::select(id_codigo = id, codigo), 
            by = c("diagnostico" = "codigo")) %>%
  mutate(diagnostico = id_codigo) %>%
  dplyr::select(id, diagnostico) %>%
  drop_na()

#### CREACIÓN DE MATRIZ DUMMIES PARA CÓDIGOS DE DIAGNÓSTICO ####

tabla_dummies <- tabla_larga %>%
  mutate(valor = 1) %>%
  distinct(id, diagnostico, .keep_all = TRUE) %>%
  pivot_wider(
    id_cols = id,
    names_from = diagnostico,
    values_from = valor,
    values_fill = 0
  )

#### CREACIÓN DE VARIABLES DEMOGRÁFICAS Y DE SALIDA ####

tabla_variables <- CMBDS %>%
  transmute(
    id = CIPA_anonimo,
    FALLECIMIENTO = if_else(TIPALT == 4, 1, 0),
    FECNAC = dmy(FECNAC),
    edad = floor(interval(FECNAC, ymd("2016-01-01")) / years(1)),
    grupo_edad = cut(edad, breaks = seq(0, 100, by = 10), 
                     right = FALSE, include.lowest = TRUE, labels = 0:9),
    SEXO = SEXO - 1
  ) %>%
  dplyr::select(-FECNAC, -edad)


#### UNIÓN FINAL DE VARIABLES PARA EL MODELO ####

tabla_modelo <- tabla_dummies %>%
  left_join(tabla_variables, by = "id") %>%
  na.omit()

#### DIVISIÓN EN ENTRENAMIENTO Y TEST ####

set.seed(123)
trainIndex <- createDataPartition(tabla_modelo$FALLECIMIENTO, p = 0.7, list = FALSE)
datos_train <- tabla_modelo[trainIndex, ]
datos_test  <- tabla_modelo[-trainIndex, ]

#### MODELO LOGÍSTICO CON VARIABLES FILTRADAS (SELECCIONADAS) ####

# Lista de variables significativas seleccionadas manualmente
vars_significativas <- c("271", "218", "200", "120", "114", "102", "75", "159", "277", "143", 
                         "24", "37", "130", "144", "146", "129", "232", "57", "272", "265", 
                         "44", "16", "228", "126", "139", "61", "156", "122", "112", "128", 
                         "124", "46", "202", "106", "65", "81", "115", "64", "104", "211", 
                         "204", "116", "62", "58", "205", "197", "40", "160", "109", "212", 
                         "54", "145", "25", "82", "31", "208", "47", "158", "147", "33", 
                         "219", "4", "157", "135", "38", "125", "32", "68", "215", "132", 
                         "23", "91", "53", "110", "119", "210", "73", "90", "266", "123", 
                         "223", "207", "231", "185", "17", "245", "235", "255")

tabla_filtrada <- datos_train %>%
  dplyr::select(id, SEXO, grupo_edad, FALLECIMIENTO, all_of(vars_significativas)) %>%
  mutate(SEXO = factor(SEXO))

modelo_logit_filtrado <- glm(FALLECIMIENTO ~ ., 
                             data = tabla_filtrada %>% dplyr::select(-id), 
                             family = binomial(link = "logit"))
summary(modelo_logit_filtrado)

#### EVALUACIÓN DEL MODELO EN CONJUNTO DE TEST ####

datos_test_filtrados <- datos_test %>%
  dplyr::select(id, SEXO, grupo_edad, FALLECIMIENTO, all_of(vars_significativas)) %>%
  mutate(SEXO = factor(SEXO))

prob_test <- predict(modelo_logit_filtrado, newdata = datos_test_filtrados, type = "response")

roc_obj <- roc(datos_test_filtrados$FALLECIMIENTO, prob_test)
plot(roc_obj)
auc(roc_obj)

#### GENERACIÓN DE HEATMAPS DE PROBABILIDAD CON COMBINACIONES DE VARIABLES ####

variables_modelo <- names(coef(modelo_logit_filtrado))[-1]
vars_categoricas <- c("SEXO", "grupo_edad")
vars_dummy <- as.character(sort(as.integer(vars_significativas)))
pares_dummy <- combn(vars_dummy, 2, simplify = FALSE)
sexos <- c("0", "1")
grupos_edad <- as.character(0:9)

#Esta función calcula todos los heatmaps de pares de variables para edad y sexo, es muy costosa temporalmente
for (sexo in sexos) {
  for (grupo in grupos_edad) {
    
    df_base <- data.frame(
      SEXO = factor(sexo, levels = levels(datos_test_filtrados$SEXO)),
      grupo_edad = factor(grupo, levels = levels(datos_train$grupo_edad))
    )
    df_base[vars_dummy] <- 0
    
    matriz_pares <- matrix(NA, nrow = length(vars_dummy), ncol = length(vars_dummy),
                           dimnames = list(vars_dummy, vars_dummy))
    
    for (i in 1:length(vars_dummy)) {
      for (j in i:length(vars_dummy)) {
        df_pred <- df_base
        df_pred[[vars_dummy[i]]] <- 1
        df_pred[[vars_dummy[j]]] <- 1
        df_pred[vars_dummy] <- lapply(df_pred[vars_dummy], as.integer)
        
        matriz_pares[i, j] <- predict(modelo_logit_filtrado, newdata = df_pred, type = "response")
      }
    }
    
    nombre_archivo <- paste0("ResultadosModelo2/heatmap_", sexo, "_", grupo, ".png")
    png(filename = nombre_archivo, width = 1200, height = 1000)
    pheatmap(matriz_pares,
             cluster_rows = FALSE,
             cluster_cols = FALSE,
             main = paste("Heatmap - SEXO", sexo, "- Grupo Edad", grupo),
             color = colorRampPalette(c("white", "red"))(200))
    dev.off()
  }
}

#### MAPAS DE DENSIDAD ####

# Añade la columna de probabilidades predichas (ygorro)
datos_test$ygorro <- prob_test

# Gráfico de densidad de ygorro para todo el conjunto de test con una linea vertical de los cuantiles

cuts_ygorro <- quantile(datos_test$ygorro, probs = c(0.3,0.7,0.9), na.rm = TRUE)
for (i in seq_along(cuts_ygorro)) {
  cat(sprintf("Percentil %s: Probabilidad menor de %.2f%% de muerte hospitalaria.\n", names(cuts_ygorro)[i], cuts_ygorro[i]*100))
}
for (valor in cuts_ygorro) {
  pct_0 <- mean(datos_test$ygorro[datos_test$FALLECIMIENTO == 0] <= valor) * 100
  pct_1 <- mean(datos_test$ygorro[datos_test$FALLECIMIENTO == 1] <= valor) * 100
  cat(sprintf("Corte %.3f: %.1f%% de FALLECIMIENTO=0, %.1f%% de FALLECIMIENTO=1 por debajo\n", 
              valor, pct_0, pct_1))
}

# Plot de densidad para ygorro por grupo de fallecimiento
ggplot(datos_test, aes(x = ygorro, fill = as.factor(FALLECIMIENTO))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribución de probabilidades predichas (ygorro)",
    x = "Probabilidad predicha (ygorro)",
    y = "Densidad",
    fill = "Fallecimiento"
  ) +
  geom_vline(xintercept = cuts_ygorro, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon")) +
  theme_minimal()

# Raiz cuarta de ygorro
ggplot(datos_test, aes(x = (ygorro)^(1/4), fill = as.factor(FALLECIMIENTO))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribución de (ygorro)^(1/4) por fallecimiento",
    x = "Raíz cuarta de probabilidad",
    y = "Densidad",
    fill = "Fallecimiento"
  ) +
  geom_vline(xintercept = (cuts_ygorro)^(1/4), linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon")) +
  theme_minimal()

# Crear carpeta si no existe
if (!dir.exists("ResultadosModelo2")) {
  dir.create("ResultadosModelo2")
}

# Secuencia de edades de 10 en 10
grupos_edad <- seq(0, 9, by = 1)
grupos_edad <- as.character(grupos_edad)
# Loop para crear y guardar gráficos
for (i in 1:(length(grupos_edad))) {
  
  # Crear gráfico con ggplot
  grafico <- ggplot(subset(datos_test, grupo_edad == grupos_edad[i]), aes(x = (ygorro)^(1/4), fill = as.factor(FALLECIMIENTO))) +
    geom_density(alpha = 0.5) +
    labs(
      title = paste0("Distribución de (ygorro)^(1/4) por fallecimiento\nGupo edad ", grupos_edad[i]),
      x = "Raíz cuarta de probabilidad",
      y = "Densidad",
      fill = "Fallecimiento"
    ) +
    geom_vline(xintercept = (cuts_ygorro)^(1/4), linetype = "dashed", color = "red") +
    scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon")) +
    theme_minimal()
  
  # Guardar gráfico
  nombre_archivo <- paste0("ResultadosModelo2/Diagram_", grupos_edad[i], ".png")
  ggsave(nombre_archivo, plot = grafico, width = 10, height = 6, bg = "white")
}
