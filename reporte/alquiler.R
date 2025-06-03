library(here)
source(here("datos","leer-datos-lp.R"))

library(tidyverse)

# Datos del alquiler en los barrios populares, eliminando nulos (viviendas no alquiladas)
datos_alquiler <- datos %>%
  filter(!is.na(costo_alquiler))

# Calculamos el nro de intervalos (√n)
bins = ceiling(sqrt(dim(datos_alquiler)[1]))
max_alq <- max(datos_alquiler$costo_alquiler)
breaks <- seq(0, max_alq, length.out = bins-1) # sqrt(n)-2 es visualmente mejor

# Gráfico
ggplot(datos_alquiler, aes(x = costo_alquiler)) +
  geom_histogram(breaks = breaks, fill = "indianred2", color = "black") +
  labs(
    title = "Distribución del costo del alquiler en los barrios relevados",
    caption = "Fuente: Relevamiento de Condiciones Habitacionales 2022, La Poderosa",
    x = "Costo del alquiler en pesos",
    y = "Frecuencia de viviendas",
  ) +
  scale_x_continuous(breaks = breaks) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  theme_classic() + 
  theme(
    plot.tag.position = "bottom",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5,vjust=-2), 
    plot.caption = element_text(size=8, hjust=0),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Calculamos la media aritmética del coste del alquiler:
media_alquiler <- mean(datos$costo_alquiler, na.rm = TRUE)

# Calculamos el desvío estándar:
desvio_estandar_alquiler <- sd(datos$costo_alquiler,na.rm=TRUE)

# Calculamos el rango de valores del alquiler:
rango_alquiler <- range(datos$costo_alquiler,na.rm=TRUE)

