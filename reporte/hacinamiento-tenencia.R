library(here)
source(here("datos","leer-datos-lp.R"))

library(tidyverse)

# Analizamos la frecuencia de la cantidad de personas por dormitorio

# Gráfico
bastones <- ggplot(datos, aes(x = as.factor(personas_dormitorio))) +
  geom_bar(fill = "salmon", width=0.1) +
  labs(
    title = "Distribución de la máxima cantidad de personas por dormitorio en los barrios relevados",
    x = "Cantidad de personas por dormitorio",
    y = "Frecuencia (absoluta)",
    caption = "Fuente: Relevamiento de Condiciones Habitacionales 2022, La Poderosa",
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_classic() +
  theme(
    plot.tag.position = "bottom",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, vjust = -2, margin = margin(b = 20)), 
    plot.caption = element_text(size=8, hjust=0),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
bastones

# Analicemos las medidas de tendencia central y dispersión:
mediana <- median(datos$personas_dormitorio, na.rm = TRUE)
q1 <- quantile(datos$personas_dormitorio, 0.25, na.rm = TRUE)
q3 <- quantile(datos$personas_dormitorio, 0.75, na.rm = TRUE)
RI <- q3 - q1

cat("Mediana:", mediana," RI:", RI)


## Marcar ciertas regiones en el gráfico
# q2 (50%)
bastones_q2 <- bastones +
  annotate("rect", xmin = 0.75, xmax = 2.25, ymin = -Inf, ymax = Inf,
                      alpha = 0.2, fill = "#FF6347") # Color "tomato"
bastones_q2

# q2 + q3
bastones_q2_q3 <- bastones_q2 +
  annotate("rect", xmin = 0.75, xmax = 3.25, ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "#FFD700") # Color "gold"
bastones_q2_q3

