# ==========================================================
#  EFI Matemática Aplicada - Análisis Predictivo en R
#  Tema: Predicción de resultados en el Fútbol Argentino
#  Script 01 - Análisis y Filtrado de Datos
# ==========================================================

# --- Cargar librerías necesarias ---
library(data.table)
library(dplyr)
library(ggplot2)
library(corrplot)

# --- Cargar dataset ---
df <- fread("../data/liga_argentina.csv")

# --- Mostrar información general ---
cat("Dimensiones del dataset:\n")
print(dim(df))
cat("\nNombres de columnas:\n")
print(names(df))
cat("\nPrimeras filas:\n")
print(head(df, 10))

# --- Seleccionar y renombrar columnas clave ---
matches <- df %>%
  select(date_name, local_team, visitor_team, local_result, visitor_result) %>%
  rename(
    torneo = date_name,
    home_team = local_team,
    away_team = visitor_team,
    home_goals = local_result,
    away_goals = visitor_result
  )

# --- Convertir torneo en texto ---
matches$torneo <- as.character(matches$torneo)

# --- Crear columna con resultado categórico ---
matches <- matches %>%
  mutate(
    resultado = case_when(
      home_goals > away_goals ~ "Local gana",
      home_goals < away_goals ~ "Visita gana",
      TRUE ~ "Empate"
    )
  )

# --- Mostrar estructura ---
cat("\nEstructura de datos limpia:\n")
print(str(matches))

# --- Gráfico: Distribución de resultados ---
ggplot(matches, aes(x = resultado, fill = resultado)) +
  geom_bar() +
  labs(
    title = "Distribución de resultados en la liga argentina",
    x = "Resultado",
    y = "Cantidad de partidos"
  ) +
  theme_minimal()

# --- Promedios generales de goles ---
promedios_global <- matches %>%
  summarise(
    promedio_goles_local = mean(home_goals, na.rm = TRUE),
    promedio_goles_visitante = mean(away_goals, na.rm = TRUE)
  )
cat("\nPromedios globales de goles:\n")
print(promedios_global)

# --- Promedio de goles por equipo local ---
promedios_equipos <- matches %>%
  group_by(home_team) %>%
  summarise(
    goles_a_favor_local = mean(home_goals, na.rm = TRUE),
    goles_en_contra_local = mean(away_goals, na.rm = TRUE),
    partidos_local = n()
  ) %>%
  arrange(desc(goles_a_favor_local))

cat("\nTop 10 equipos con mayor promedio de goles como local:\n")
print(head(promedios_equipos, 10))

# --- Guardar versión limpia ---
write.csv(matches, "../output/liga_argentina_clean.csv", row.names = FALSE)
cat("\nArchivo limpio guardado en: ../output/liga_argentina_clean.csv\n")

# --- Mensaje final ---
cat("\nScript 01 finalizado correctamente.\n")
