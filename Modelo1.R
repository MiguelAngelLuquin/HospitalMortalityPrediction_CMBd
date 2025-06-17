##### CARGA DE LIBRERÍAS NECESARIAS #####
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)
library(pROC)
library(caret)

##### FUNCIONES AUXILIARES #####

# Expande intervalos de códigos CIE10 para convertir rangos como "A00-A09" en vectores explícitos
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

##### PREPARAMOS LOS DATOS #####

# Establecer directorio de trabajo donde está el script
setwd("C:/Users/luqui/Desktop/IACS/MuerteHospitalariaModelo")

# Cargar tabla de secciones de CIE10
Secciones_CIE10 <- read_csv("Tablas/Secciones_CIE10.csv")

# Separar rangos tipo A00-A09 en columnas 'inicio' y 'fin'
tabla_secciones <- Secciones_CIE10 %>% 
  separate(CIE10, into = c("inicio", "fin"), sep = "-") %>%
  mutate(fin = if_else(is.na(fin), inicio, fin))

# Expandir los rangos para hacer el join con códigos individuales
tabla_secciones_exp <- tabla_secciones %>%
  mutate(codigo = map2(inicio, fin, expand_codigos)) %>%
  unnest(codigo) %>%
  dplyr::select(id, codigo, Seccion)

# Eliminar duplicados si los hay
tabla_secciones_exp <- tabla_secciones_exp %>%
  distinct(codigo, .keep_all = TRUE)

##### PROCESAMIENTO DEL CMBD #####

# Cargar el CMBD
CMBDS <- read_delim("Tablas/CMBD_HOS_ANONIMO_20160101_20161231.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Filtrar valores erróneos de sexo
CMBDS <- CMBDS %>% filter(SEXO != 3)

# Seleccionar columnas de diagnóstico y CIPA
cols_diagnostico <- grep("^C\\d+", names(CMBDS), value = TRUE)
tabla_base <- CMBDS %>%
  dplyr::select(all_of(cols_diagnostico), CIPA_anonimo) %>%
  mutate(id = CIPA_anonimo)

# Limpiar los códigos de diagnóstico (quitar decimales)
tabla_base[cols_diagnostico] <- lapply(tabla_base[cols_diagnostico], function(col) {
  col <- as.character(col)
  sub("\\..*", "", col)
})

# Eliminar columna CIPA original
tabla_base <- tabla_base %>% dplyr::select(-CIPA_anonimo)

##### TRANSFORMAR A FORMATO LARGO #####

tabla_larga <- tabla_base %>%
  pivot_longer(
    cols = all_of(cols_diagnostico),
    names_to = "diagnostico_col",
    values_to = "diagnostico"
  ) %>%
  filter(!is.na(diagnostico), diagnostico != "") %>%
  dplyr::select(-diagnostico_col)

# Relacionar diagnósticos con secciones CIE10
tabla_larga <- tabla_larga %>%
  left_join(tabla_secciones_exp %>% dplyr::select(id_codigo = id, codigo), by = c("diagnostico" = "codigo")) %>%
  mutate(diagnostico = id_codigo) %>%
  dplyr::select(id, diagnostico) %>%
  drop_na()

##### CREAR MATRIZ DE VARIABLES DUMMIES #####

tabla_dummies <- tabla_larga %>%
  mutate(valor = 1) %>%
  distinct(id, diagnostico, .keep_all = TRUE) %>%
  pivot_wider(
    id_cols = id,
    names_from = diagnostico,
    values_from = valor,
    values_fill = 0
  )

##### CREACIÓN DE VARIABLES CLÍNICAS #####

tabla_variables <- CMBDS %>%
  transmute(
    id = CIPA_anonimo,
    FALLECIMIENTO = if_else(TIPALT == 4, 1, 0),
    FECNAC = dmy(FECNAC),
    edad = floor(interval(FECNAC, ymd("2016-01-01")) / years(1)),
    grupo_edad = cut(edad, breaks = seq(0, 100, by = 10), right = FALSE, include.lowest = TRUE, labels = 0:9),
    SEXO = SEXO - 1
  ) %>%
  dplyr::select(-FECNAC, -edad)

##### UNIFICAR TABLA PARA EL MODELO #####

tabla_modelo <- tabla_dummies %>%
  left_join(tabla_variables, by = "id")

# Eliminar filas incompletas
tabla_modelo <- na.omit(tabla_modelo)

##### DIVISIÓN EN ENTRENAMIENTO Y TEST #####

set.seed(123)
trainIndex <- createDataPartition(tabla_modelo$FALLECIMIENTO, p = 0.7, list = FALSE)

datos_train <- tabla_modelo[trainIndex, ]
datos_test  <- tabla_modelo[-trainIndex, ]

# Convertir SEXO a factor para el modelo
datos_train$SEXO <- factor(datos_train$SEXO)

##### ENTRENAMIENTO DEL MODELO LOGÍSTICO #####

modelo_logit <- glm(FALLECIMIENTO ~ ., data = datos_train %>% dplyr::select(-id), family = binomial(link = "logit"))
summary(modelo_logit)

##### VERIFICACIÓN DEL MODELO #####

# Preparar test para predicción
datos_test$SEXO <- factor(datos_test$SEXO)

# Predecir probabilidades
prob_test <- predict(modelo_logit, newdata = datos_test, type = "response")

# Evaluar con curva ROC
roc_obj <- roc(datos_test$FALLECIMIENTO, prob_test)
plot(roc_obj)
auc(roc_obj)

##### VER VARIABLES SIGNIFICAIVAS #####

# Extraer resumen como data.frame
coef_summary <- summary(modelo_logit)$coefficients

# Convertir a data.frame y ordenar por p-valor
coef_df <- as.data.frame(coef_summary)
coef_df$variable <- rownames(coef_df)
coef_df <- coef_df[order(coef_df$`Pr(>|z|)`), ]

# Mostrar las variables más significativas
head(coef_df, 10)$variable
