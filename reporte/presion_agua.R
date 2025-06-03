library(here)
source(here("datos","leer-datos-lp.R"))

library(tidyverse)

datos_limpios <- datos %>%
  mutate( presion_agua = factor(presion_agua,
                                levels = c("Muy débil", "Débil", "Buena"),
                                ordered = TRUE))

datos_presion <- datos_limpios %>% select(presion_agua)

datos_presion_res <- datos_presion %>% 
  group_by(presion_agua) %>% 
  summarize(cant = n())

datos_grafico <- datos_presion_res %>%
  mutate(
    porcentaje = round(cant / sum(cant), 3)
  )

grafico <- ggplot(datos_grafico) + 
  aes(
    x = reorder(presion_agua, -as.numeric(presion_agua)), # Categorías ordenadas según orden natural de la variable arriba a abajo (presión)
    y = porcentaje
  ) + 
  scale_y_continuous(labels = scales::percent) +    # Eje para porcentajes
  
  geom_bar(width = 0.75,   # Ancho de barras
           fill = '#7ed021',  # Color de relleno 
           col = "black",  # Color de línea
           alpha = 0.6,      # Transparencia
           stat = "identity") +  
  
  geom_text(
    aes(y = porcentaje / 2, label = paste0(porcentaje * 100, "%")),
    position = position_dodge(width = 0.9),
    size = 4.5
  ) +
  
  labs(
    title = "Distribución de la calidad de presión de agua declarada en los barrios relevados",
    x = "Calidad de presión del agua", 
    y = "Porcentaje de viviendas",
    caption = "Fuente: Relevamiento de Condiciones Habitacionales 2022, La Poderosa",
  ) + # Nombres de ejes
  
  coord_flip() +
  
  theme_classic() +
  theme(
    plot.tag.position = "bottom",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5,vjust=-2), 
    plot.caption = element_text(size=8, hjust=0),
    axis.text.x = element_text(angle = 45, hjust = 1),
  )
grafico

# Mediana
mediana_num <- median(as.numeric(datos_grafico$presion_agua))
nivel_mediana <- (levels(datos_grafico$presion_agua))[mediana_num]
print(paste0("El valor de presión de agua correspondiente a la mediana es: ", nivel_mediana))

## Marcar ciertas regiones del gráfico
# q2
grafico_q2 <- grafico +
  annotate("rect", xmin = 1.5, xmax = 3.5, ymin = -Inf, ymax = Inf,
         alpha = 0.2, fill = "#87CEEB")
grafico_q2

# Categoría muy débil
grafico_md <- grafico +
  annotate("rect", xmin = 2.5, xmax = 3.5, ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "#FFD700")
grafico_md

