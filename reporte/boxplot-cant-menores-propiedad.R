library(here)
source(here("datos","leer-datos-lp.R"))

library(tidyverse)

# Categorizar viviendas según condición de propiedad
datos_limpios <- datos %>%
  mutate(
    propio = case_when(propiedad %in% c("Propio con algún comprobante de tenencia","Propio sin títulos")~"Propia",
                       TRUE ~ "No propia"),
  )

# Seleccionar columnas necesarias para la gráfica
datos_menores_tenencia <- datos_limpios %>%
  select(
    menores_edad, propio
  )

# Análisis bivariado mediante boxplots comparativos
ggplot(datos_menores_tenencia) +
  aes(x = propio, y = menores_edad) +
  geom_boxplot(fill = "seagreen3") +
  labs(
    x = "Condición de propiedad", 
    y = "Cantidad de menores en el hogar",
    title = "Distribución de cantidad de menores en la vivienda según condición de propiedad\nen los barrios relevados",
    caption = "Fuente: Relevamiento de Condiciones Habitacionales 2022, La Poderosa"
  ) +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  theme_classic() +
  theme(
    plot.tag.position = "bottom",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5,vjust=-2), 
    plot.caption = element_text(size=8, hjust=0),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

