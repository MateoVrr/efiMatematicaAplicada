# ==========================================================
#  EFI Matemática Aplicada - Análisis Predictivo en R
#  Tema: Predicción de resultados del Fútbol Argentino
#  Script 05 - Graficos adicionales
# ==========================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

cat("Cargando datos limpios...\n")
data_path <- "~/MatAplic_2025/EFI-predR/output/liga_argentina_clean.csv"

if (!file.exists(data_path)) {
  stop("No se encontró el archivo de datos limpios. Ejecutá el script 01 primero.")
}

df <- read.csv(data_path)

# --- Intentar detectar año ---
if ("torneo" %in% names(df)) {
  # Si es fecha válida, convertir a POSIXct
  if (all(grepl("^\\d{4}-\\d{2}-\\d{2}", df$torneo))) {
    df <- df %>% mutate(anio = year(as.POSIXct(torneo)))
  } else {
    # Extraer el año de un texto (por ejemplo “Torneo 2023” o “Clausura 2018”)
    df <- df %>% mutate(anio = as.numeric(stringr::str_extract(torneo, "\\d{4}")))
  }
} else {
  df$anio <- NA
}

# --- Crear carpeta de salida ---
if (!dir.exists("~/MatAplic_2025/EFI-predR/output")) {
  dir.create("~/MatAplic_2025/EFI-predR/output", recursive = TRUE)
}

# --- Gráfico 1: Evolución de goles promedio por año ---
cat("Generando gráfico 1: Evolución de goles promedio por año...\n")

goles_anuales <- df %>%
  filter(!is.na(anio)) %>%
  group_by(anio) %>%
  summarise(promedio_goles = mean(home_goals + away_goals, na.rm = TRUE))

if (nrow(goles_anuales) > 0) {
  g1 <- ggplot(goles_anuales, aes(x = anio, y = promedio_goles)) +
    geom_line(color = "#2E86C1", size = 1.2) +
    geom_point(color = "#1B4F72", size = 2) +
    theme_minimal(base_size = 13) +
    labs(
      title = "Evolución del promedio de goles por año",
      x = "Año",
      y = "Promedio de goles por partido"
    )
  
  ggsave("~/MatAplic_2025/EFI-predR/output/05a_evolucion_goles_anuales.png", g1, width = 7, height = 5)
} else {
  cat("No se pudo generar el gráfico 1 (no se detectaron años válidos en 'torneo').\n")
}

# --- Gráfico 2: Efectividad local vs visitante ---
cat("Generando gráfico 2: Efectividad local vs visitante por año...\n")

if ("resultado" %in% names(df)) {
  ventaja_local <- df %>%
    filter(!is.na(anio)) %>%
    group_by(anio, resultado) %>%
    summarise(cantidad = n(), .groups = "drop") %>%
    group_by(anio) %>%
    mutate(pct = cantidad / sum(cantidad) * 100)
  
  g2 <- ggplot(ventaja_local, aes(x = anio, y = pct, color = resultado)) +
    geom_line(size = 1.4, alpha = 0.9) +
    geom_point(size = 2.5, alpha = 0.8) +
    theme_minimal(base_size = 14) +
    scale_color_manual(values = c("Empate" = "#F39C12", 
                                  "Local gana" = "#27AE60", 
                                  "Visita gana" = "#C0392B")) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +  
    labs(
      title = "Evolución de resultados (%) por año",
      x = "Año",
      y = "Porcentaje de resultados",
      color = "Resultado"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      axis.text.y = element_text(size = 11),
      panel.grid.minor = element_blank(),
      plot.margin = ggplot2::margin(10, 30, 10, 30)
    )
  
ggsave("~/MatAplic_2025/EFI-predR/output/05b_ventaja_local_vs_visitante.png", 
         g2, width = 9, height = 5.5)
} else {
  cat("No se encontró la columna 'resultado' para generar el gráfico 2.\n")
}

cat("\nScript 05 finalizado correctamente. Gráficos adicionales guardados en /output.\n")