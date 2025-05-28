library(here)
source(here("datos","leer-datos-lp.R"))

library(tidyverse)

# Analizamos la frecuencia de la cantidad de personas por dormitorio

# Gráfico
ggplot(datos, aes(x = as.factor(personas_dormitorio))) +
  geom_bar(fill = "salmon", width=0.1) +
  labs(
    title = "Distribución de la máxima cantidad de personas por dormitorio en los barrios relevados",
    x = "Cantidad de personas por dormitorio",
    y = "Frecuencia (absoluta)",
    caption = "Fuente: Relevamiento de Condiciones Habitacionles 2022, La Poderosa",
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(
    plot.caption = element_text(size=8, hjust=0),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5,vjust=-2), 
  )+
  theme_classic() +
  theme(
    plot.tag.position = "bottom",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, vjust = -2, margin = margin(b = 20)), 
    plot.caption = element_text(size=8, hjust=0),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Analizemos las tendencias centrales y dispersión:
mediana <- median(datos$personas_dormitorio, na.rm = TRUE)
q1 <- quantile(datos$personas_dormitorio, 0.25, na.rm = TRUE)
q3 <- quantile(datos$personas_dormitorio, 0.75, na.rm = TRUE)
RI <- q3 - q1

cat("Mediana:", mediana," RI:", RI)

