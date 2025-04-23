library(here)
source(here("datos","leer-datos-lp.R"))

library(tidyverse)

# HISTOGRAMA: EDAD DEL JEFE/A DEL HOGAR
# Frecuencias absolutas
ggplot(datos) +
  aes(x = edad_jefe_hogar) +
  geom_histogram(fill = "aquamarine3", col = "black", 
                 breaks = seq(0, 100, 10)) +
  
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(
    x = "Edad en años", 
    y = "Cantidad de personas",
    title = "Distribución de la edad del jefe/a del hogar en los barrios relevados",
    caption = "Fuente: Relevamiento de Condiciones Habitacionles 2022, La Poderosa"
  ) + 
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

# promedio edad y desvio estandar
promedio_edad <- mean(datos$edad_jefe_hogar)
desvio_edad <- sd(datos$edad_jefe_hogar)
print(paste0("La edad promedio del jefe/a de los hogares ecuestados es de: ", promedio_edad))
print(paste0("El desvio estándar de la edad del jefe/a de los hogares encuestados es de: ", desvio_edad))
