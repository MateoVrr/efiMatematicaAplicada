================================================================================
EFI - Análisis Predictivo de Resultados de Fútbol Argentino
================================================================================

Autores: Mateo Torres - Alejandro Quiroga - Santiago Garola
Curso: Matemática Aplicada 2025
Proyecto: Predicción de resultados de partidos de la Liga Argentina
Lenguaje: R (RStudio)
Dataset: "Liga Argentina de Fútbol" - Kaggle
Link dataset: https://www.kaggle.com/datasets/estebanmarcelloni/liga-argentina-futbol

--------------------------------------------------------------------------------
1. ESTRUCTURA DE CARPETAS
--------------------------------------------------------------------------------

Proyecto principal: EFI-predR/
│
├── code/                   # Scripts de R que realizan todo el flujo
│   ├── 01_analisis_filtrado_futbol.R
│   ├── 02_modelado_predictivo_futbol.R
│   ├── 03_prediccion_realista_futbol.R
│   ├── 04_visualizacion_resultados_futbol.R
│   └── 05_graficos_adicionales_futbol.R  # gráficos extra
│
├── data/                   # Carpeta para los datasets
│   └── liga-argentina.csv  # Dataset original descargado de Kaggle
│
├── output/                 # Carpeta donde se guardan los resultados
│   ├── liga_argentina_clean.csv   # Dataset limpio
│   ├── modelo_rf_realista.RData   # Modelo Random Forest entrenado
│   ├── figuras y gráficos:
│   │     01_distribucion_goles.png
│   │     02_top_equipos_local.png
│   │     03_mapa_correlacion.png
│   │     04_resultados_predichos.png
│   │     05a_evolucion_goles_anuales.png
│   │     05b_ventaja_local_vs_visitante.png
│   │     05c_predicciones_probabilidades.png
│   └── ...otros gráficos generados
│
├── README.txt              # Este archivo explicativo

--------------------------------------------------------------------------------
2. REQUERIMIENTOS
--------------------------------------------------------------------------------

- R >= 4.0
- RStudio (opcional pero recomendado)
- Librerías de R:
  data.table, dplyr, ggplot2, GGally, corrplot, caret, randomForest, glmnet, xgboost, recipes, tidyverse

Instalación recomendada:
```R
install.packages(c("data.table","dplyr","ggplot2","GGally","corrplot",
                   "caret","randomForest","glmnet","xgboost","recipes","tidyverse"))

