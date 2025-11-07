# ==========================================================
#  EFI Matemática Aplicada - Análisis Predictivo en R
#  Tema: Predicción de resultados del Fútbol Argentino
#  Script 04 - Visualizacion de resultados
# ==========================================================

library(data.table)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(GGally)
library(randomForest)

# Crear carpeta de salida
if(!dir.exists("../output/plots")) dir.create("../output/plots", recursive = TRUE)


# Cargar dataset limpio y estadísticas por equipo

df <- fread("../output/liga_argentina_clean.csv")

# Resumen inicial
cat("Total de partidos:", nrow(df), "\n")


# Distribución general de resultados
g1 <- ggplot(df, aes(x = resultado, fill = resultado)) +
  geom_bar() +
  labs(title = "Distribución de resultados",
       x = "Resultado",
       y = "Cantidad de partidos") +
  theme_minimal(base_size = 14)

ggsave("../output/plots/01_distribucion_resultados.png", g1, width = 7, height = 5)


# Promedio de goles por tipo de equipo
promedios <- df %>%
  summarise(
    Promedio_Local = mean(home_goals),
    Promedio_Visitante = mean(away_goals)
  ) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "Tipo", values_to = "Promedio")

g2 <- ggplot(promedios, aes(x = Tipo, y = Promedio, fill = Tipo)) +
  geom_col(width = 0.5) +
  labs(title = "Promedio de goles: local vs visitante") +
  theme_minimal(base_size = 14)

ggsave("../output/plots/02_promedio_goles.png", g2, width = 6, height = 5)


# Top equipos con más victorias históricas
top_equipos <- df %>%
  filter(resultado == "Local gana") %>%
  count(home_team, sort = TRUE) %>%
  slice_head(n = 10)

g3 <- ggplot(top_equipos, aes(x = reorder(home_team, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 equipos con más victorias de local",
       x = "Equipo local", y = "Cantidad de victorias") +
  theme_minimal(base_size = 13)

ggsave("../output/plots/03_top_equipos_local.png", g3, width = 7, height = 5)


# Importancia de variables del modelo
modelo_path <- "../output/modelo_rf_realista.RData"
if (file.exists(modelo_path)) {
  load(modelo_path)
  importancia <- importance(modelo_rf)
  imp_df <- data.frame(Variable = rownames(importancia),
                       MeanDecreaseGini = importancia[, "MeanDecreaseGini"]) %>%
    arrange(desc(MeanDecreaseGini)) %>%
    head(10)
  
  g4 <- ggplot(imp_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
    geom_col(fill = "darkorange") +
    coord_flip() +
    labs(title = "Top 10 variables más importantes",
         x = "Variable", y = "Importancia") +
    theme_minimal(base_size = 13)
  
  ggsave("../output/plots/04_importancia_variables.png", g4, width = 7, height = 5)
} else {
  cat("No se encontró el modelo guardado (modelo_rf_realista.RData)\n")
}


# Mapa de calor de correlaciones entre tasas
# Reutilizamos las tasas del script 03
stats <- df %>%
  group_by(home_team) %>%
  summarise(
    tasa_victorias_local = mean(resultado == "Local gana"),
    tasa_empates_local = mean(resultado == "Empate"),
    tasa_derrotas_local = mean(resultado == "Visita gana"),
    prom_goles_local = mean(home_goals),
    prom_goles_visitante = mean(away_goals)
  )

corr_matrix <- round(cor(stats %>% select(-home_team), use = "pairwise.complete.obs"), 2)

library(ggcorrplot)

g5 <- ggcorrplot(
  corr_matrix,
  hc.order = FALSE,
  type = "lower",
  lab = TRUE,
  lab_size = 5,            # tamaño de los números dentro de las celdas
  method = "circle",
  colors = c("red", "white", "blue"),
  ggtheme = theme_minimal(base_size = 14)
) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  ) +
  labs(title = "Correlación entre métricas de rendimiento")
ggsave("../output/plots/05_mapa_correlacion.png", g5, width = 8.5, height = 6.5, dpi = 300)

cat("\n script 04 finalizado correctamente.\n")

