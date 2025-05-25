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

#Calculamos porcentajes  - agrupamos datos.
datos_humedad_resumen <- datos_humedad_largo %>%
  group_by(ambiente, hay_humedad) %>%
  summarize(cant = n(), .groups = "drop_last") %>%
  mutate(
    porcentaje = round(cant / sum(cant) * 100, 1)
  )

# Gráfico
ggplot(datos_humedad_resumen) +
  aes(x = ambiente, y = porcentaje, fill = hay_humedad) +
  geom_bar(position = "dodge", stat = "identity", colour = "black") +
  scale_fill_manual(
    values = c("TRUE" = "#63B8FF", "FALSE" = "#CD96CD"),
    labels = c("Sí", "No")
  ) +
  labs(
    x = "Ambiente de la vivienda",
    y = "Porcentaje de humedad",
    fill = "Presencia de humedad",
    title = "Distribución de problemas de humedad por ambiente\nsegún declaración en los barrios relevados",
    caption = "Fuente: Relevamiento de Condiciones Habitacionales 2022, La Poderosa"
  ) +
  geom_text(
    aes(y = porcentaje / 2, label = paste0(porcentaje, "%")),
    position = position_dodge(width = 0.9),
    size = 4.5
  ) +
  theme_classic() +
  theme(
    plot.tag.position = "bottom",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, vjust = -2),
    plot.caption = element_text(size = 8, hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.background = element_rect(color = "black", linewidth = 0.5),
    legend.spacing = unit(0.5, "cm"),
    legend.margin = margin(10, 10, 10, 10)
  )

# Veamos la cantidad de cada uno (+ porcentaje):
print(datos_humedad_resumen)


# Calculamos proporción de humedad promedio por ambiente
porcentaje_humedad_promedio <- datos_humedad_largo %>%
  mutate(hay_humedad = as.logical(hay_humedad)) %>%  # <- Conversión TRUE/FALSE
  group_by(ambiente) %>%
  summarize(
    promedio_humedad = mean(hay_humedad, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(promedio_humedad))

print(porcentaje_humedad_promedio)

# Calculamos la mediana  de humedad por ambiente
porcentaje_humedad_mediana <- datos_humedad_largo %>%
  mutate(hay_humedad = as.logical(hay_humedad)) %>%  # <- Conversión TRUE/FALSE
  group_by(ambiente) %>%
  summarize(
    mediana_humedad = median(hay_humedad, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(mediana_humedad))

print(porcentaje_humedad_mediana)
#Si es 1: >50% humedad.
#Si es 0: <50% humedad.

