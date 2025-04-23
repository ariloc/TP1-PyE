library(here)
source(here("datos","leer-datos-lp.R"))

library(tidyverse)

#Analizaremos los problemas de humedad en los diferentes ambientes de la vivienda de los barrios populares

datos_humedad <- datos %>%
  select(
    humedad_dormitorios, humedad_cocina,humedad_baño,humedad_living,humedad_otro
  )

#Como las respuetas no cuentan con "sí"/"no" , las refactorizamos:
datos_humedad_expandido <- datos_humedad %>%
  mutate(
    dormitorio = !is.na(humedad_dormitorios) & humedad_dormitorios != "",
    cocina = !is.na(humedad_cocina) & humedad_cocina != "",
    baño = !is.na(humedad_baño) & humedad_baño != "",
    living = !is.na(humedad_living) & humedad_living != "",
    otro = !is.na(humedad_otro) & humedad_otro != "",
  )

#TRUE-FALSE
datos_humedad_largo <- datos_humedad_expandido %>%
  pivot_longer(
    cols = c(dormitorio, cocina, baño, living, otro),
    names_to = "ambiente",
    values_to = "hay_humedad"
  )

#Los ordeno para no mezclar valores:
datos_humedad_largo<-datos_humedad_largo %>%
  mutate(hay_humedad=factor(hay_humedad,levels=c(TRUE,FALSE)))

# Gráfico
ggplot(datos_humedad_largo, aes(x = ambiente, fill = hay_humedad)) +
  geom_bar(position = "fill") +
  labs(
    title = "Distribución de problemas de humedad por ambiente declarados en los barrios relevados",
    x = "Ambiente de la vivienda",
    y = "Porcentaje de viviendas con humedad",
    fill="Presencia de humedad",
    caption = "Fuente: Relevamiento de Condiciones Habitacionles 2022, La Poderosa",
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c("TRUE" = "#63B8FF", "FALSE" = "#CD96CD"), # Cambia los colores
    labels = c("Sí", "No") # Cambia el texto de la leyenda
  )+
  geom_text(
    stat = "count",
    aes(label = scales::percent(..count../tapply(..count.., ..x.., sum)[..x..], accuracy = 1)), #porcentajes.
    position = position_fill(vjust = 0.5),
    size = 3
  )+
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


