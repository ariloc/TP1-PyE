library(here)
source(here("datos","leer-datos-lp.R"))

#Costo promedio del alquiler en los barrios populares:
datos_alquiler <- datos %>%
  filter(!is.na(costo_alquiler)) %>%
  count(costo_alquiler)

#Calculemos el mínimo y máximo:
min_alquiler <- min(datos$costo_alquiler, na.rm = TRUE)
print(min_alquiler)
max_alquiler <- max(datos$costo_alquiler, na.rm = TRUE)
print(max_alquiler)


#Calculamos el nro de intervalos (√n)
n <- nrow(datos_alquiler)
rango <-max_alquiler - min_alquiler
binwidth <- rango / sqrt(n)

#Gráfico
ggplot(datos_alquiler, aes(x = costo_alquiler)) +
  geom_histogram(binwidth = binwidth, fill = "indianred2", color = "black") +
  labs(
    title = "Frecuencia del costo del alquiler en barrios populares",
    x = "Costo del alquiler en pesos",
    y = "Cantidad de viviendas",
  ) +theme(
    plot.tag.position = "bottom",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5,vjust=-2), 
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  theme_minimal()

#Calculamos la media aritmética del coste del alquiler:
media_alquiler <- mean(datos$costo_alquiler, na.rm = TRUE)
print(media_alquiler)
#mediana_alquiler<-median(datos$costo_alquiler, na.rm = TRUE)

#Calculamos el desvío estándar:
desvio_estandar_alquiler<-sd(datos$costo_alquiler,na.rm=TRUE)
