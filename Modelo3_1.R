# MODELO CON INTERACCIONES DE EDAD Y SEXO #

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(purrr)
library(MASS)
library(pROC)
library(glmnet)
library(caret)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(pheatmap)

expand_codigos <- function(inicio, fin) {
  if (inicio == fin) {
    return(inicio)
  } else {
    letra <- substr(inicio, 1, 1)
    num_inicio <- as.integer(substr(inicio, 2, 3))
    # Si el tercer carácter de 'fin' es una letra, lo reemplaza por '9'
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

setwd("C:/Users/luqui/Desktop/IACS/MuerteHospitalariaModelo")

# Cargar los datos de Secciones_CIE10
Secciones_CIE10 <- read_csv("Tablas/Secciones_CIE10.csv")

# Separo el intervalo de códigos en inicio y fin
tabla_secciones <- Secciones_CIE10 %>% 
  separate(CIE10, into = c("inicio", "fin"), sep = "-") %>%
  mutate(fin = if_else(is.na(fin), inicio, fin))

#Expando los intervalos para facilitar luego el LEFT JOIN
tabla_secciones_exp <- tabla_secciones %>%
  mutate(codigo = map2(inicio, fin, expand_codigos)) %>%
  unnest(codigo) %>% dplyr::select(id, codigo, Seccion)

# Elimino si han habido duplicaciones
tabla_secciones_exp <- tabla_secciones_exp %>%
  distinct(codigo, .keep_all = TRUE)

# Cargar los datos del CMBD
CMBDS <- read_delim("Tablas/CMBD_HOS_ANONIMO_20160101_20161231.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

CMBDS <- CMBDS %>% filter(SEXO != 3)

cols_diagnostico <- grep("^C\\d+", names(CMBDS), value = TRUE)
tabla_base <- CMBDS %>%
  dplyr::select(all_of(cols_diagnostico), CIPA_anonimo) %>%    # solo las columnas requeridas
  mutate(
    id = CIPA_anonimo                             # crear id único por paciente
  )
tabla_base[cols_diagnostico] <- lapply(tabla_base[cols_diagnostico], function(col) {
  col <- as.character(col)         # convertir a texto
  sub("\\..*", "", col)            # eliminar el punto y todo lo posterior
})

tabla_base <- tabla_base %>% dplyr::select(-CIPA_anonimo)

tabla_larga <- tabla_base %>%
  pivot_longer(
    cols = all_of(cols_diagnostico),
    names_to = "diagnostico_col",
    values_to = "diagnostico"
  ) %>%
  filter(!is.na(diagnostico), diagnostico != "")  # quitar filas sin diagnóstico

tabla_larga <- tabla_larga %>% dplyr::select(-diagnostico_col)

tabla_larga <- tabla_larga %>%
  left_join(tabla_secciones_exp %>% dplyr::select(id_codigo = id, codigo), by = c("diagnostico" = "codigo")) %>%
  mutate(diagnostico = id_codigo) %>%        # Reemplaza el diagnóstico por el id del código
  dplyr::select(id, diagnostico)                    # Deja solo las columnas que te interesan

tabla_larga <- tabla_larga %>%
  drop_na()

# Ordenar por 'id' y 'diagnostico'
tabla_larga <- tabla_larga[order(tabla_larga$id, tabla_larga$diagnostico), ]

# Eliminar duplicados dejando solo la primera ocurrencia de cada combinación 'id' + 'diagnostico'
tabla_larga <- tabla_larga[!duplicated(tabla_larga[, c("id", "diagnostico")]), ]

freq_diag <- tabla_larga %>%
  count(diagnostico, sort = TRUE)

# Seleccionar los 25 más frecuentes
top40_diag <- freq_diag %>%
  slice_max(n, n = 40) %>%
  pull(diagnostico)
top40_diag <- top40_diag[!top40_diag %in% c("270", "274", "170")]

# Filtrar el dataframe original para quedarte solo con esos diagnósticos
tabla_larga <- tabla_larga %>%
  mutate(
    diagnostico = ifelse(
      diagnostico %in% top40_diag,
      diagnostico,
      "otro"
    )
  )

tabla_dummies <- tabla_larga %>%
  mutate(valor = 1) %>%
  distinct(id, diagnostico, .keep_all = TRUE) %>%  # eliminar duplicados (mismo paciente y combo)
  pivot_wider(
    id_cols = id,
    names_from = diagnostico,
    values_from = valor,
    values_fill = 0
  )


tabla_variables <- CMBDS %>%
  transmute(
    id = CIPA_anonimo,
    FALLECIMIENTO = if_else(TIPALT == 4, 1, 0),
    FECNAC = dmy(FECNAC),                               # convertir la fecha
    edad = floor(interval(FECNAC, ymd("2016-01-01")) / years(1)), 
    grupo_edad = cut(edad, breaks = seq(0, 100, by = 10), right = FALSE, include.lowest = TRUE, labels = 0:9),
    SEXO = SEXO-1
  )

tabla_variables <- tabla_variables %>% dplyr::select(-FECNAC,-edad)

tabla_modelo <- tabla_dummies %>%
  left_join(tabla_variables, by = "id")


set.seed(123)  # Para reproducibilidad

tabla_modelo <- na.omit(tabla_modelo)  # Eliminar filas con NA
# Crea índices para el 70% de entrenamiento
trainIndex <- createDataPartition(tabla_modelo$FALLECIMIENTO, p = 0.7, list = FALSE)

# Crea los conjuntos
datos_train <- tabla_modelo[trainIndex, ]
datos_test  <- tabla_modelo[-trainIndex, ]

datos_train$SEXO <- factor(datos_train$SEXO)

diag_vars <- top40_diag[top40_diag %in% names(tabla_modelo)]
diag_vars <- paste0("`", diag_vars, "`")
diag_vars <- append(diag_vars, "otro")  # Asegurarse de incluir "otro" si no está en top40_diag

# Crear la parte principal de la fórmula
main_effects <- c(diag_vars, "SEXO", "grupo_edad")

# Interacciones con SEXO
interactions_sexo <- paste0(diag_vars, ":SEXO")

# Interacciones con grupo_edad
interactions_edad <- paste0(diag_vars, ":grupo_edad")

# Combinar todo
fmla <- as.formula(
  paste("FALLECIMIENTO ~", 
        paste(c(main_effects, interactions_sexo, interactions_edad), collapse = " + "))
)
fmla
modelo_logit_interacciones <- glm(fmla, data = datos_train %>% dplyr::select(-id), family = binomial(link = "logit"))
summary(modelo_logit_interacciones)

##### VERIFICACION DEL MODELO FILTRADO #######

datos_test$SEXO <- factor(datos_test$SEXO)
# Predice probabilidades en test
prob_test <- predict(modelo_logit_interacciones, newdata = datos_test, type = "response")

roc_obj <- roc(datos_test$FALLECIMIENTO, prob_test)
plot(roc_obj)
auc(roc_obj)

##### MODELOS DE DENSIDAD #######

# Añade la columna de probabilidades predichas (ygorro)
datos_test$ygorro <- prob_test

# Gráfico de densidad de ygorro para todo el conjunto de test con una linea vertical de los cuantiles

cuts_ygorro <- quantile(datos_test$ygorro, probs = c(0.35,0.7,0.90), na.rm = TRUE)
for (i in seq_along(cuts_ygorro)) {
  cat(sprintf("Percentil %s: Probabilidad menor de %.2f%% de muerte hospitalaria.\n", names(cuts_ygorro)[i], cuts_ygorro[i]*100))
}
for (valor in cuts_ygorro) {
  pct_0 <- mean(datos_test$ygorro[datos_test$FALLECIMIENTO == 0] <= valor) * 100
  pct_1 <- mean(datos_test$ygorro[datos_test$FALLECIMIENTO == 1] <= valor) * 100
  cat(sprintf("Corte %.3f: %.1f%% de FALLECIMIENTO=0, %.1f%% de FALLECIMIENTO=1 por debajo\n", 
              valor, pct_0, pct_1))
}

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
if (!dir.exists("ResultadosModelo3_1")) {
  dir.create("ResultadosModelo3_1")
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
  nombre_archivo <- paste0("ResultadosModelo3_1/Diagram_", grupos_edad[i], ".png")
  ggsave(nombre_archivo, plot = grafico, width = 10, height = 6, bg = "white")
}


##### CREAR HEATMAPS #######

vars_dummy <- top40_diag
vars_dummy <- append(vars_dummy, "otro")  # Asegurarse de incluir "otro" si no está en top40_diag
vars_dummy <- vars_dummy[order(as.numeric(vars_dummy))]  # solo números como strings, sin comillas extras
vars_dummy <- as.character(vars_dummy)
pares_dummy <- combn(vars_dummy, 2, simplify = FALSE)

sexos <- c("0", "1")
grupos_edad <- as.character(0:9)

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
    print(paste("Procesando sexo:", sexo, "y grupo de edad:", grupo))
    
    # Crear dataframe base (sin cambios)
    df_base <- data.frame(
      SEXO = factor(sexo, levels = levels(datos_test$SEXO)),
      grupo_edad = factor(grupo, levels = levels(datos_train$grupo_edad))
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
    nombre_archivo <- paste0("ResultadosModelo3_1/heatmap_", sexo, "_", grupo, ".png")
    png(filename = nombre_archivo, width = 1200, height = 1000)
    
    # Generar heatmap
    pheatmap(matrices_lista[[nombre]],
             cluster_rows = FALSE,
             cluster_cols = FALSE,
             main = paste("Heatmap - SEXO", sexo, "- Grupo Edad", grupo),
             color = colores_gradiente)
    
    dev.off()
  }
}

# 4. Ejecutar función de ploteo
plot_heatmaps(matrices_pares_lista)

# 5. Función matrices como CSV para no tener que volver a calcular
guardar_matrices_csv <- function(matrices_lista, carpeta_destino = "matrices3_csv") {
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

leer_matrices_csv <- function(directorio = "matrices3_csv") {
  archivos <- list.files(path = directorio, pattern = "\\.csv$", full.names = TRUE)
  matrices_pares_lista <- lapply(
    archivos,
    function(archivo) {
      as.matrix(read.csv(archivo, row.names = 1, check.names = FALSE))
    }
  )
  names(matrices_pares_lista) <- basename(archivos)
  return(matrices_pares_lista)
}

# Uso:
matrices_pares_lista <- leer_matrices_csv()
