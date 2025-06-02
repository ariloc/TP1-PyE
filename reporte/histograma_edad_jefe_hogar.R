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
    y = "Frecuencia de jefes/as",
    title = "Distribución de la edad del jefe/a del hogar en los barrios relevados",
    caption = "Fuente: Relevamiento de Condiciones Habitacionales 2022, La Poderosa"
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

# Promedio edad y Desvío estándar
promedio_edad <- mean(datos$edad_jefe_hogar)
desvio_edad <- sd(datos$edad_jefe_hogar)
print(paste0("La edad promedio del jefe/a de los hogares encuestados es de: ", promedio_edad))
print(paste0("El desvio estándar de la edad del jefe/a de los hogares encuestados es de: ", desvio_edad))

# Mediana e rango intercuartílico, dada la leve asimetría por derecha que presentan los datos
mediana_edad <- median(datos$edad_jefe_hogar)
ri_edad <- IQR(datos$edad_jefe_hogar)
print(paste0("La mitad de los jefes/as tiene hasta ", mediana_edad, " años"))
print(paste0("El 50% central de las edades de los jefes/as de hogar en las viviendas relevadas se encuentra entre ", mediana_edad-ri_edad, "-", mediana_edad+ri_edad, " años"))

