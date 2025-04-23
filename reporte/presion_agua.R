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
  
  labs(
    title = "Distribución de la calidad de presión de agua declarada en los barrios relevados",
    x = "Calidad de presión del agua", 
    y = "Porcentaje de viviendas",
    caption = "Fuente: Relevamiento de Condiciones Habitacionles 2022, La Poderosa",
  ) + # Nombres de ejes
  
  coord_flip() +
  
  theme_classic() +
  theme(
    plot.tag.position = "bottom",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5,vjust=-2), 
    plot.caption = element_text(size=8, hjust=0),
    axis.text.x = element_text(angle = 45, hjust = 1),
    
    legend.background = element_rect(color = "black", linewidth  = 0.5),
    legend.spacing = unit(0.5, "cm"), 
    legend.margin = margin(10, 10, 10, 10)
  )

# mediana
mediana_num <- median(as.numeric(datos_limpios$presion_agua))
nivel_mediana <- (levels(datos_limpios$presion_agua))[mediana_num]
print(paste0("El valor de presión de agua correspondiente a la mediana es: ", nivel_mediana))

