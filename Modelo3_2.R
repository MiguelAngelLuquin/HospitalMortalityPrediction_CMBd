# MODELO CON INTERACCIONES DE EDAD Y SEXO

# Librerías necesarias
library(readr)      # Lectura de archivos CSV
library(dplyr)      # Manipulación de datos
library(tidyr)      # Transformación de datos
library(purrr)      # Funciones funcionales (map2)
library(stringr)    # Manipulación de strings
library(lubridate)  # Fechas
library(caret)      # Partición de datos
library(pROC)       # Curvas ROC y AUC
library(ggplot2)    # Gráficos
library(grDevices)  # Para funciones gráficas como png() y colorRampPalette()
library(pheatmap)   # Para crear heatmaps

# Función para expandir códigos tipo "A01-A05" a A01, A02, ..., A05
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

# Directorio de trabajo
setwd("C:/Users/luqui/Desktop/IACS/MuerteHospitalariaModelo")

# --- CARGA Y PROCESADO DE TABLAS AUXILIARES ---

# Tabla de secciones CIE10
Secciones_CIE10 <- read_csv("Tablas/Secciones_CIE10.csv")

# Separar rangos de códigos y expandirlos
tabla_secciones <- Secciones_CIE10 %>% 
  separate(CIE10, into = c("inicio", "fin"), sep = "-") %>%
  mutate(fin = if_else(is.na(fin), inicio, fin)) %>%
  mutate(codigo = map2(inicio, fin, expand_codigos)) %>%
  unnest(codigo) %>%
  distinct(codigo, .keep_all = TRUE) %>%
  dplyr::select(id, codigo, Seccion)

# --- CARGA Y PREPARACIÓN DE DATOS DEL CMBD ---

# Leer fichero del CMBD
CMBDS <- read_delim("Tablas/CMBD_HOS_ANONIMO_20160101_20161231.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(SEXO != 3)  # Eliminar registros con sexo inválido

# Extraer columnas de diagnóstico y preparar tabla base
cols_diagnostico <- grep("^C\\d+", names(CMBDS), value = TRUE)

tabla_base <- CMBDS %>%
  dplyr::select(all_of(cols_diagnostico), CIPA_anonimo) %>%
  mutate(id = CIPA_anonimo) %>%
  dplyr::select(-CIPA_anonimo)

# Limpiar códigos de diagnóstico (eliminar ".xxx")
tabla_base[cols_diagnostico] <- lapply(tabla_base[cols_diagnostico], function(col) {
  col <- as.character(col)
  sub("\\..*", "", col)
})

# Lista de diagnósticos seleccionados
vars_significativas <- c("271", "218", "200", "120", "114", "102", "75", "159", "277", "143", 
                         "24", "37", "130", "144", "146", "129", "232", "57", "272", "265", 
                         "44", "16", "228", "126", "139", "61", "156", "122", "112", "128", 
                         "124", "46", "202", "106", "65", "81", "115", "64", "104", "211", 
                         "204", "116", "62", "58", "205", "197", "40", "160", "109", "212", 
                         "54", "145", "25", "82", "31", "208", "47", "158", "147", "33", 
                         "219", "4", "157", "135", "38", "125", "32", "68", "215", "132", 
                         "23", "91", "53", "110", "119", "210", "73", "90", "266", "123", 
                         "223", "207", "231", "185", "17", "245", "235", "255")

# Convertir tabla base a formato largo y mapear a IDs de sección
tabla_larga <- tabla_base %>%
  pivot_longer(cols = all_of(cols_diagnostico), names_to = "diagnostico_col", values_to = "diagnostico") %>%
  filter(!is.na(diagnostico), diagnostico != "") %>%
  dplyr::select(-diagnostico_col) %>%
  left_join(tabla_secciones %>% dplyr::select(id_codigo = id, codigo), 
            by = c("diagnostico" = "codigo")) %>%
  mutate(diagnostico = id_codigo) %>%
  drop_na() %>%
  distinct(id, diagnostico)

# Agrupar diagnósticos: si no está en top 100, se marca como "otro"
tabla_larga <- tabla_larga %>%
  mutate(diagnostico = ifelse(diagnostico %in% vars_significativas, diagnostico, "otro"))

# Crear variables dummy
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
  transmute(
    id = CIPA_anonimo,
    FALLECIMIENTO = if_else(TIPALT == 4, 1, 0),
    FECNAC = dmy(FECNAC),
    edad = floor(interval(FECNAC, ymd("2016-01-01")) / years(1)),
    SEXO = SEXO - 1  # Convertir a 0 (hombre) y 1 (mujer)
  ) %>%
  dplyr::select(-FECNAC)

# Unir datos clínicos y demográficos
tabla_modelo <- tabla_dummies %>%
  left_join(tabla_variables, by = "id") %>%
  na.omit()

# --- PARTICIÓN DEL DATASET EN ENTRENAMIENTO Y TEST ---

set.seed(123)  # Para reproducibilidad
trainIndex <- createDataPartition(tabla_modelo$FALLECIMIENTO, p = 0.7, list = FALSE)
datos_train <- tabla_modelo[trainIndex, ]
datos_test  <- tabla_modelo[-trainIndex, ]

datos_train$SEXO <- factor(datos_train$SEXO)
datos_test$SEXO  <- factor(datos_test$SEXO)

# --- AJUSTE DEL MODELO LOGÍSTICO CON INTERACCIONES ---

# Crear fórmula con efectos principales + interacciones con edad y sexo
diag_vars <- vars_significativas[vars_significativas %in% names(tabla_modelo)]
diag_vars <- paste0("`", diag_vars, "`")
diag_vars <- append(diag_vars, "otro")

fmla <- as.formula(
  paste("FALLECIMIENTO ~", 
        paste(c(diag_vars, "SEXO", "edad",
                paste0(diag_vars, ":SEXO"),
                paste0(diag_vars, ":edad")), collapse = " + "))
)

# Ajustar modelo
modelo_logit_interacciones <- glm(fmla, data = datos_train %>% dplyr::select(-id), family = binomial(link = "logit"))
summary(modelo_logit_interacciones)

# --- EVALUACIÓN DEL MODELO ---

# Predicción en test
prob_test <- predict(modelo_logit_interacciones, newdata = datos_test, type = "response")

# Curva ROC y AUC
roc_obj <- roc(datos_test$FALLECIMIENTO, prob_test)
plot(roc_obj)
auc(roc_obj)

# --- ANÁLISIS DE LA DISTRIBUCIÓN DE PREDICCIONES ---

datos_test$ygorro <- prob_test
cuts_ygorro <- quantile(datos_test$ygorro, probs = c(0.35, 0.7, 0.90), na.rm = TRUE)

for (i in seq_along(cuts_ygorro)) {
  cat(sprintf("Percentil %s: Probabilidad menor de %.2f%% de muerte hospitalaria.\n", 
              names(cuts_ygorro)[i], cuts_ygorro[i]*100))
}

for (valor in cuts_ygorro) {
  pct_0 <- mean(datos_test$ygorro[datos_test$FALLECIMIENTO == 0] <= valor) * 100
  pct_1 <- mean(datos_test$ygorro[datos_test$FALLECIMIENTO == 1] <= valor) * 100
  cat(sprintf("Corte %.3f: %.1f%% de FALLECIMIENTO=0, %.1f%% de FALLECIMIENTO=1 por debajo\n", 
              valor, pct_0, pct_1))
}

datos_test$edad <- datos_test$edad +1

# Gráfico de densidad transformado (raíz cuarta)
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
if (!dir.exists("ResultadosModelo3_2")) {
  dir.create("ResultadosModelo3_2")
}

# Secuencia de edades de 10 en 10
grupos_edad <- seq(0, 100, by = 10)
# Loop para crear y guardar gráficos
for (i in 1:(length(grupos_edad) - 1)) {
  edad_min <- grupos_edad[i]
  edad_max <- grupos_edad[i + 1]
  
  # Subconjunto del test para ese grupo de edad
  datos_grupo <- subset(datos_test, edad >= edad_min & edad < edad_max)
  
  # Salta si el subconjunto está vacío
  if (nrow(datos_grupo) == 0) next
  
  # Crear gráfico con ggplot
  grafico <- ggplot(datos_grupo, aes(x = (ygorro)^(1/4), fill = as.factor(FALLECIMIENTO))) +
    geom_density(alpha = 0.5) +
    labs(
      title = paste0("Distribución de (ygorro)^(1/4) por fallecimiento\nEdad ", edad_min, " a ", edad_max),
      x = "Raíz cuarta de probabilidad",
      y = "Densidad",
      fill = "Fallecimiento"
    ) +
    geom_vline(xintercept = (cuts_ygorro)^(1/4), linetype = "dashed", color = "red") +
    scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon")) +
    theme_minimal()
  
  # Guardar gráfico
  nombre_archivo <- paste0("ResultadosModelo3_2/Diagram", edad_min, "-", edad_max, ".png")
  ggsave(nombre_archivo, plot = grafico, width = 10, height = 6, bg = "white")
}



##### CREAR HEATMAPS #######

vars_dummy <- vars_significativas[vars_significativas %in% names(tabla_modelo)]
vars_dummy <- vars_dummy[order(as.numeric(vars_dummy))]  # solo números como strings, sin comillas extras
vars_dummy <- append(vars_dummy, "otro")  # Añadir la categoría "otro"
vars_dummy <- as.character(vars_dummy)
pares_dummy <- combn(vars_dummy, 2, simplify = FALSE)

sexos <- c("0", "1")
grupos_edad <- as.character(seq(10,90, by = 10))  # Grupos de edad de 0 a 100 años con intervalos de 10 años
grupos_edad

# Paleta con 5 colores principales
colores_principales <- c("#ffffff", "#fdd0a2", "#fc8d59", "#e34a33", "#b30000")

# Crear paleta interpolada
colores_gradiente <- colorRampPalette(colores_principales)(200)

# Crear vector de breaks fijo de 0 a 1
breaks <- seq(0, 1, length.out = 201)

# 1. Crear lista para almacenar todas las matrices
matrices_pares_lista <- list()

# 2. Generar y guardar matrices
for (sexo in sexos) {
  for (grupo in grupos_edad) {
    print(paste("Procesando sexo:", sexo, "y edad:", grupo))
    
    # Crear dataframe base (sin cambios)
    df_base <- data.frame(
      SEXO = factor(sexo, levels = levels(datos_test$SEXO)),
      edad = as.numeric(grupo)
    )
    
    for (var in vars_dummy) {
      df_base[[var]] <- 0
    }
    
    # Generar matriz de predicciones (sin cambios)
    matriz_pares <- matrix(NA, nrow = length(vars_dummy), ncol = length(vars_dummy),
                           dimnames = list(vars_dummy, vars_dummy))
    
    for (i in 1:length(vars_dummy)) {
      print(paste("Procesando variable:", vars_dummy[i]))
      for (j in i:length(vars_dummy)) {
        df_pred <- df_base
        df_pred[[vars_dummy[i]]] <- 1
        df_pred[[vars_dummy[j]]] <- 1
        df_pred[vars_dummy] <- lapply(df_pred[vars_dummy], as.integer)
        
        matriz_pares[i, j] <- predict(modelo_logit_interacciones, newdata = df_pred, type = "response")
      }
    }
    
    # Guardar matriz en la lista con nombre único
    nombre_matriz <- paste0("matriz_", sexo, "_", grupo)
    matrices_pares_lista[[nombre_matriz]] <- matriz_pares
  }
}

# 3. Función para plotear heatmaps
plot_heatmaps <- function(matrices_lista) {
  for (nombre in names(matrices_lista)) {
    print(paste("Generando heatmap para:", nombre))
    
    # Extraer sexo y grupo del nombre
    partes <- strsplit(nombre, "_")[[1]]
    sexo <- partes[2]
    grupo <- paste(partes[3], partes[4], sep = "_")
    
    # Configurar archivo de salida
    nombre_archivo <- paste0("ResultadosModelo3_2/heatmap_", sexo, "_", grupo, ".png")
    png(filename = nombre_archivo, width = 1200, height = 1000)
    
    # Generar heatmap
    pheatmap(matrices_lista[[nombre]],
             cluster_rows = FALSE,
             cluster_cols = FALSE,
             main = paste("Heatmap - SEXO", sexo, "- Grupo Edad", grupo),
             color = colores_gradiente,
             breaks = breaks)
    
    dev.off()
  }
}

# 4. Ejecutar función de ploteo
plot_heatmaps(matrices_pares_lista)

# 5. Función matrices como CSV para no tener que volver a calcular
guardar_matrices_csv <- function(matrices_lista, carpeta_destino = "matrices_csv") {
  # Crear la carpeta si no existe
  if (!dir.exists(carpeta_destino)) {
    dir.create(carpeta_destino)
  }
  
  for (nombre in names(matrices_lista)) {
    # Construir nombre de archivo
    nombre_archivo <- paste0(carpeta_destino, "/", nombre, ".csv")
    # Guardar matriz como .csv
    write.csv(matrices_lista[[nombre]], file = nombre_archivo, row.names = TRUE)
  }
  print(paste("Se han guardado", length(matrices_lista), "matrices en la carpeta", carpeta_destino))
}
guardar_matrices_csv(matrices_pares_lista)

leer_matrices_csv <- function(directorio = "matrices_csv") {
  archivos <- list.files(path = directorio, pattern = "\\.csv$", full.names = TRUE)
  matrices_pares_lista <- lapply(archivos, read.csv, stringsAsFactors = FALSE)
  names(matrices_pares_lista) <- basename(archivos)
  return(matrices_pares_lista)
}
dev.off()  # Cerrar cualquier dispositivo gráfico abierto
# Uso:
matrices_pares_lista <- leer_matrices_csv()






