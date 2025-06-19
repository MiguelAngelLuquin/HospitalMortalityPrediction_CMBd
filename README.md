Autor: Miguel Ángel Luquín Guerrero
Datos proporcionados por el Ministerio de Sanidad (CMBD anonimizado)
Clasificación de diagnósticos basada en CIE-10

# Modelo de Predicción de Muerte Hospitalaria

Este repositorio contiene el desarrollo y evaluación de diversos modelos estadísticos implementados en R para predecir la probabilidad de fallecimiento hospitalario en base a datos anonimizados del conjunto CMBD (Conjunto Mínimo Básico de Datos).

## Estructura del proyecto

MuerteHospitalariaModelo/
│
├── Modelo1.R # Primer modelo predictivo
├── Modelo2.R # Segundo modelo (con mejoras o cambios)
├── Modelo3_1.R # Tercer modelo - parte 1
├── Modelo3_2.R # Tercer modelo - parte 2
├── Modelo4.R # Modelo Final
│
├── tabla_modelo.csv # Tabla resumen de características del modelo
│
├── matrices_csv/ # Matrices de entrada por edad y sexo
│
├── ResultadosModelo1/ # Resultados gráficos del Modelo 1 (curva ROC)
│
├── ResultadosModelo2/ # Resultados del Modelo 2: diagramas, heatmaps, curva ROC
│
├── ResultadosModelo3_1/ # Resultados del Modelo 3 parte 1: densidades, diagramas, heatmaps, curva ROC
│
├── ResultadosModelo3_2/ # Resultados del Modelo 3 parte 2: diagramas, heatmaps, curva ROC
│
├── ResultadosModelo4/ # Resultados del Modelo 4: diagramas
│
├── Tablas/
│ ├── CMBD_HOS_ANONIMO_20160101_20161231.csv # Datos originales anonimizados
│ ├── CIE10.txt # Clasificación Internacional de Enfermedades (descripciones)
│ └── Secciones_CIE10.csv # Agrupaciones de diagnósticos CIE10
│
└── README.txt # Documento de texto original

## Descripción de los Modelos

### Modelo 1
- Modelo de regresión logística básico.
- Incluye variables clínicas y sociodemográficas.
- Métrica: curva ROC disponible.

### Modelo 2
- Introduce interacciones y/o nuevas variables.
- Visualizaciones: diagramas por sexo y edad, mapas de calor (heatmaps), densidad y curva ROC.

### Modelo 3.1 y 3.2
- Segmentación más fina por grupos de edad.
- Parte 1 incluye diagramas y heatmaps con datos NA.
- Parte 2 presenta resultados por decenios de edad con diagramas y heatmaps más detallados.

### Modelo 4
- Introduce POA y tiempo de estancia y la interacción de estos con edad al cuadrado.
- Visualizaciones: diagramas, Curva PROC, Curva PR, Log_Loss, Calibration Curve, Overlap Area.

## Requisitos

- **R** (≥ 4.0.0)
- **Paquetes necesarios:**
  - `ggplot2`
  - `pROC`
  - `dplyr`
  - `tidyr`
  - `readr`
  - `PRROC`
  - `scales`
  - `binom`
  - `caTools`

## Uso

1. Colocar los archivos en un directorio de trabajo en R.
2. Ejecutar los scripts en el siguiente orden recomendado:

```R
source("Modelo1.R")
source("Modelo2.R")
source("Modelo3_1.R")
source("Modelo3_2.R")
source("Modelo4.R")
