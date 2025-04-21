library(here)
source(here("datos","leer-datos-lp.R"))

library(tidyverse)

datos_limpios <- datos %>%
  mutate( presion_agua = factor(presion_agua,
                                levels = c("Muy débil", "Débil", "Buena"),
                                ordered = TRUE))

ggplot(datos_limpios) + 
  aes(x = presion_agua, y = ..count.. / sum(..count..)) + # Porcentajes
  aes(x = reorder(presion_agua, presion_agua, function(x) -length(x)), 
      y = ..count.. / sum(..count..)) +  # Porcentajes ordenados según frecuencia
  scale_y_continuous(labels = scales::percent) +    # Eje para porcentajes
  
  geom_bar(width = 0.75,   # Ancho de barras
           fill = '#7ed021',  # Color de relleno 
           col = "black",  # Color de línea
           alpha = 0.6) +  # Transparencia
  
  labs(x = "Tipo de presión de agua", y = "Porcentaje de hogares") + # Nombres de ejes
  
  ggtitle("Presión de agua en hogares relevados\nen barrios populares por La Poderosa - Año 2023") +
  
  coord_flip() + # Barras horizontales o verticales
  
  theme_classic() +# Temas preconfigurados de R https://r-charts.com/ggplot2/themes/
  theme(plot.title = element_text(size = 12))

# mediana
mediana_num <- median(as.numeric(datos_limpios$presion_agua))
nivel_mediana <- (levels(datos_limpios$presion_agua))[mediana_num]
print(paste0("El valor de presión de agua correspondiente a la mediana es: ", nivel_mediana))

