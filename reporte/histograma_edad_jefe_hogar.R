library(here)
source(here("datos","leer-datos-lp.R"))

library(tidyverse)

# HISTOGRAMA: EDAD DEL JEFE DEL HOGAR
# Frecuencias absolutas
ggplot(datos) +
  aes(x = edad_jefe_hogar) +
  geom_histogram(fill = "lightgray", col = "black", 
                 breaks = seq(0, 100, 10)) +
  
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(x = "Edad en años", y = "Cantidad de personas") + 
  ggtitle("Edad del jefe/a del hogar\nen barrios populares por La Poderosa - Año 2023")

# promedio edad y desvio estandar
promedio_edad <- mean(datos$edad_jefe_hogar)
desvio_edad <- sd(datos$edad_jefe_hogar)
print(paste0("La edad promedio del jefe/a de los hogares ecuestados es de: ", promedio_edad))
print(paste0("El desvio estándar de la edad del jefe/a de los hogares encuestados es de: ", desvio_edad))
