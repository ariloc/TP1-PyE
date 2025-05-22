library(here)
source(here("datos","leer-datos-lp.R"))

library(tidyverse)

#Datos del alquiler en los barrios populares , con la cantidad de cada uno:
datos_alquiler <- datos %>%
  filter(!is.na(costo_alquiler)) %>%
  count(costo_alquiler)

#Calculemos el mínimo y máximo:
min_alquiler <- min(datos$costo_alquiler, na.rm = TRUE)
max_alquiler <- max(datos$costo_alquiler, na.rm = TRUE)


#Calculamos el nro de intervalos (√n)
bins = ceiling(sqrt(dim(datos_alquiler)[1]))

#Gráfico
ggplot(datos_alquiler, aes(x = costo_alquiler)) +
  geom_histogram(bins = bins, fill = "indianred2", color = "black") +
  labs(
    title = "Distribución del costo del alquiler en los barrios populares relevados",
    caption = "Fuente: Relevamiento de Condiciones Habitacionles 2022, La Poderosa",
    x = "Costo del alquiler en pesos",
    y = "Cantidad de viviendas",
  ) +
  theme_classic() + 
  theme(
    plot.tag.position = "bottom",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5,vjust=-2), 
    plot.caption = element_text(size=8, hjust=0),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

attach(datos)

#Calculamos la media aritmética del coste del alquiler:
media_alquiler <- mean(datos$costo_alquiler, na.rm = TRUE)

#Calculamos el desvío estándar:
desvio_estandar_alquiler<-sd(datos$costo_alquiler,na.rm=TRUE)

detach(datos)

