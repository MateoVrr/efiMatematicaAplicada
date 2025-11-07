# ==========================================================
#  EFI Matemática Aplicada - Análisis Predictivo en R
#  Tema: Predicción de resultados del Fútbol Argentino
#  Script 03 - Predicción Realista
# ==========================================================

library(data.table)
library(dplyr)
library(caret)
library(randomForest)


# Cargar datos limpios
df <- fread("../output/liga_argentina_clean.csv")

# Verificamos estructura
cat("Dimensiones del dataset:", dim(df), "\n")


# Crear estadísticas históricas
# Promedios del equipo local
stats_local <- df %>%
  group_by(home_team) %>%
  summarise(
    prom_goles_favor_local = mean(home_goals),
    prom_goles_contra_local = mean(away_goals),
    tasa_victorias_local = mean(resultado == "Local gana"),
    tasa_empates_local = mean(resultado == "Empate"),
    tasa_derrotas_local = mean(resultado == "Visita gana")
  )

# Promedios del equipo visitante
stats_visitante <- df %>%
  group_by(away_team) %>%
  summarise(
    prom_goles_favor_visit = mean(away_goals),
    prom_goles_contra_visit = mean(home_goals),
    tasa_victorias_visit = mean(resultado == "Visita gana"),
    tasa_empates_visit = mean(resultado == "Empate"),
    tasa_derrotas_visit = mean(resultado == "Local gana")
  )


# Unir estadísticas al dataset
df_model <- df %>%
  left_join(stats_local, by = c("home_team")) %>%
  left_join(stats_visitante, by = c("away_team"))

# Eliminamos columnas no predictoras
df_model <- df_model %>%
  select(-torneo, -home_goals, -away_goals)

# Convertimos la variable objetivo a factor
df_model$resultado <- as.factor(df_model$resultado)

# Eliminamos filas con NA
df_model <- na.omit(df_model)

cat("Dimensiones finales del dataset modelado:", dim(df_model), "\n")


# Dividir en train/test
set.seed(123)
trainIndex <- createDataPartition(df_model$resultado, p = 0.8, list = FALSE)
train <- df_model[trainIndex, ]
test <- df_model[-trainIndex, ]


# Entrenar modelo Random Forest
cat("\nEntrenando modelo Random Forest (predicción realista)...\n")

modelo_rf <- randomForest(resultado ~ ., data = train, ntree = 300, mtry = 3, importance = TRUE)

# Evaluación en test
pred_rf <- predict(modelo_rf, newdata = test)
conf_rf <- confusionMatrix(pred_rf, test$resultado)
print(conf_rf)


# Importancia de variables
cat("\nImportancia de variables:\n")
print(importance(modelo_rf))


# Predicción de nuevos partidos
cat("\nEjemplo de predicción de nuevos partidos:\n")

nuevos_partidos <- data.frame(
  home_team = c("Boca Juniors", "Independiente", "Racing Club"),
  away_team = c("River Plate", "San Lorenzo", "Huracan (LH)")
)

# Agregamos estadísticas históricas
nuevos_partidos <- nuevos_partidos %>%
  left_join(stats_local, by = c("home_team")) %>%
  left_join(stats_visitante, by = c("away_team"))

# Predecimos resultados
predicciones <- predict(modelo_rf, newdata = nuevos_partidos)

nuevos_partidos$Resultado_Previsto <- predicciones
print(nuevos_partidos)

# Guardar modelo para visualización posterior
save(modelo_rf, file = "../output/modelo_rf_realista.RData")

cat("\nScript 03 finalizado correctamente.\n")

