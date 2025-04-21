library(here)
source(here("datos","leer-datos-lp.R"))

library(tidyverse)

#Costo promedio del alquiler en los barrios populares:
datos_alquiler <- datos %>%
  filter(!is.na(costo_alquiler)) %>%
  count(costo_alquiler)

#Gráfico
ggplot(datos_alquiler, aes(x = as.factor(costo_alquiler), y = n)) +
  geom_bar(stat = "identity", fill = "indianred2", color = "black") +
  geom_text(aes(label = n), vjust = 2, color = "#8B0000", size = 3.5) + #etiquetas dentro de rectángulos.
  labs(
    title = "Frecuencia del costo del alquiler en barrios populares",
    x = "Costo del alquiler en pesos",
    y = "Cantidad de viviendas",
  ) +theme(
    plot.tag.position = "bottom",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5,vjust=-2)) + 
  ylim(0, max(datos_alquiler$n) + 1) +  # Ajuste dinámico del eje Y
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#Calculamos la media aritmética del coste del alquiler:
media_alquiler <- mean(datos$costo_alquiler, na.rm = TRUE)
print(media_alquiler)
#mediana_alquiler<-median(datos$costo_alquiler, na.rm = TRUE)

#Calculemos el mínimo y máximo:
min_alquiler <- min(datos$costo_alquiler, na.rm = TRUE)
print(min_alquiler)
max_alquiler <- max(datos$costo_alquiler, na.rm = TRUE)
print(max_alquiler)

