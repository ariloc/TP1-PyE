library(here)
source(here("datos","leer-datos-lp.R"))

library(tidyverse)

# Categorizar viviendas según condición de propiedad
datos_limpios <- datos %>%
  mutate(
    propio = case_when(propiedad %in% c("Propio con algún comprobante de tenencia","Propio sin títulos")~"Propia",
                       TRUE ~ "No propia"),
  )

# Seleccionar columnas necesarias para la gráfica
datos_tiempo_residencia <- datos_limpios %>%
  select(
    tiempo_residencia, propio
  )

# Análisis bivariado mediante boxplots comparativos
ggplot(datos_tiempo_residencia) +
  aes(x = propio, y = tiempo_residencia) +
  geom_boxplot(fill = "lavender") +
  labs(
    x = "Condición de propiedad",
    y = "Tiempo de residencia (en años)",
    title = "Distribución del tiempo de residencia en la vivienda según condición de propiedad en los barrios relevados",
    caption = "Fuente: Relevamiento de Condiciones Habitacionales 2022, La Poderosa"
  ) +
  scale_y_continuous(
    breaks = seq(0, 150, 5),
    minor_breaks = seq(0, 150, 2.5)
  ) +
  theme_classic() +
  theme(
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey80"),
    plot.tag.position = "bottom",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, vjust = -2, margin = margin(b = 20)), 
    plot.caption = element_text(size=8, hjust=0),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

