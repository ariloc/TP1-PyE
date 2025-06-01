library(here)
source(here("datos","leer-datos-lp.R"))

library(tidyverse)

# Analizaremos los problemas de humedad en los diferentes ambientes de la vivienda de los barrios populares

datos_humedad <- datos %>%
  select(
    humedad_dormitorios, humedad_cocina,humedad_baño,humedad_living,humedad_otro
  )

# Como las respuestas no cuentan con "sí"/"no" , las refactorizamos:
datos_humedad_expandido <- datos_humedad %>%
  mutate(
    dormitorio = !is.na(humedad_dormitorios) & humedad_dormitorios != "",
    cocina = !is.na(humedad_cocina) & humedad_cocina != "",
    baño = !is.na(humedad_baño) & humedad_baño != "",
    living = !is.na(humedad_living) & humedad_living != "",
    otro = !is.na(humedad_otro) & humedad_otro != "",
  )

# TRUE-FALSE
datos_humedad_largo <- datos_humedad_expandido %>%
  pivot_longer(
    cols = c(dormitorio, cocina, baño, living, otro),
    names_to = "ambiente",
    values_to = "hay_humedad"
  )

# Los ordeno para no mezclar valores:
datos_humedad_largo <- datos_humedad_largo %>%
  mutate(hay_humedad=factor(hay_humedad,levels=c(TRUE,FALSE)))

# Calculamos porcentajes  - agrupamos datos.
datos_humedad_resumen <- datos_humedad_largo %>%
  group_by(ambiente, hay_humedad) %>%
  summarize(cant = n(), .groups = "drop_last") %>%
  mutate(
    porcentaje = round(cant / sum(cant), 3) # Redondeado a un dígito al multiplicar por 100
  )

# Nos quedamos con sólo con los TRUE
datos_humedad_resumen <- datos_humedad_resumen %>%
  filter(hay_humedad == TRUE)

# Cambiamos finalmente los nombres de los ambientes para la gráfica
datos_humedad_resumen <- datos_humedad_resumen %>%
  mutate(ambiente = 
           factor(ambiente, 
                  levels = c("cocina","dormitorio","baño","living","otro"),
                  labels = c("Cocina","Dormitorio","Baño","Living","Otro")
           )
  )
  

# Gráfico
ggplot(datos_humedad_resumen) +
  aes(x = reorder(ambiente, porcentaje), y = porcentaje) + # Ordenar por frecuencia de mayor a menor con reorder()
  geom_bar(position = "dodge", stat = "identity", colour = "black", fill = "dodgerblue2") +
  scale_y_continuous(labels = scales::percent) +  # Usar etiquetas de porcentaje en la escala
  labs(
    x = "Ambiente de la vivienda",
    y = "Porcentaje de viviendas con humedad",
    fill = "Ambientes con humedad",
    title = "Distribución de problemas de humedad por ambiente en las viviendas\nde los barrios relevados",
    caption = "Fuente: Relevamiento de Condiciones Habitacionales 2022, La Poderosa"
  ) +
  geom_text(
    aes(y = porcentaje / 2, label = paste0(porcentaje * 100, "%")),
    position = position_dodge(width = 0.9),
    size = 4.5
  ) +
  coord_flip() +
  theme_classic() +
  theme(
    plot.tag.position = "bottom",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, vjust = -2),
    plot.caption = element_text(size = 8, hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1),
  )

# Veamos la cantidad de cada uno en un tabla (+ porcentaje):
print(
  datos_humedad_resumen %>% 
  select(-hay_humedad) %>%
  arrange(desc(porcentaje)) %>%
  mutate(porcentaje = porcentaje*100)
)

