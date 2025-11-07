# ==========================================================
#  EFI Matemática Aplicada - Análisis Predictivo en R
#  Tema: Predicción de resultados del Fútbol Argentino
#  Script 02 - Modelado Predictivo
# ==========================================================

# --- Librerías necesarias ---
library(dplyr)
library(caret)
library(randomForest)
library(ggplot2)
library(nnet)

# --- Cargar dataset limpio ---
matches <- read.csv("../output/liga_argentina_clean.csv")

# --- Conversión de tipos ---
matches$resultado <- as.factor(matches$resultado)

# --- Crear variables adicionales ---
matches <- matches %>%
  mutate(
    diferencia_goles = home_goals - away_goals,
    total_goles = home_goals + away_goals
  )

# --- Seleccionar columnas útiles para el modelo ---
data_modelo <- matches %>%
  select(home_goals, away_goals, diferencia_goles, total_goles, resultado)

# --- División entrenamiento / prueba ---
set.seed(123)
trainIndex <- createDataPartition(data_modelo$resultado, p = 0.8, list = FALSE)
train <- data_modelo[trainIndex, ]
test <- data_modelo[-trainIndex, ]

# --- Entrenamiento modelo Random Forest ---
cat("\nEntrenando modelo Random Forest...\n")
modelo_rf <- randomForest(resultado ~ ., data = train, ntree = 200, mtry = 2, importance = TRUE)
print(modelo_rf)

# --- Predicciones con Random Forest ---
pred_rf <- predict(modelo_rf, test)
confusion_rf <- confusionMatrix(pred_rf, test$resultado)

cat("\nMatriz de confusión (Random Forest):\n")
print(confusion_rf$table)
cat("\nPrecisión total (Random Forest):\n")
print(confusion_rf$overall["Accuracy"])

# --- Entrenamiento modelo Regresión Logística ---
cat("\nEntrenando modelo de Regresión Logística...\n")
modelo_log <- multinom(resultado ~ diferencia_goles + total_goles, data = train, trace = FALSE)

# --- Predicciones con Regresión Logística ---
pred_log <- predict(modelo_log, test)
confusion_log <- confusionMatrix(pred_log, test$resultado)

cat("\nMatriz de confusión (Regresión Logística):\n")
print(confusion_log$table)
cat("\nPrecisión total (Regresión Logística):\n")
print(confusion_log$overall["Accuracy"])

# --- Comparar modelos ---
accuracies <- data.frame(
  Modelo = c("Random Forest", "Regresión Logística"),
  Precisión = c(confusion_rf$overall["Accuracy"], confusion_log$overall["Accuracy"])
)

ggplot(accuracies, aes(x = Modelo, y = Precisión, fill = Modelo)) +
  geom_bar(stat = "identity") +
  ylim(0, 1) +
  labs(title = "Comparación de precisión entre modelos",
       y = "Precisión", x = "") +
  theme_minimal()

# --- Predicción de partidos de ejemplo ---
# Simulamos algunos partidos (a futuro)
partidos_nuevos <- data.frame(
  home_goals = c(1, 2, 0),
  away_goals = c(0, 2, 1),
  diferencia_goles = c(1, 0, -1),
  total_goles = c(1, 4, 1)
)

predicciones_futuras <- predict(modelo_rf, partidos_nuevos)
cat("\nPredicción de ejemplo para partidos futuros:\n")
print(cbind(partidos_nuevos, Resultado_Previsto = predicciones_futuras))

cat("\nScript 02 finalizado correctamente.\n")

