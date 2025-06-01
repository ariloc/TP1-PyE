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
datos_jefe_propiedad <- datos_limpios %>%
  select(
    edad_jefe_hogar, propio
  )

# Análisis bivariado mediante boxplots comparativos
ggplot(datos_jefe_propiedad) +
  aes(x = propio, y = edad_jefe_hogar) +
  geom_boxplot(fill = "indianred2") +
  labs(
    x = "Condición de propiedad", 
    y = "Edad del jefe del hogar",
    title = "Distribución de la edad del jefe del hogar según condición de propiedad en los barrios relevados",
    caption = "Fuente: Relevamiento de Condiciones Habitacionales 2022, La Poderosa"
  ) +
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  theme_classic() +
  theme(
    plot.tag.position = "bottom",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5,vjust=-2), 
    plot.caption = element_text(size=8, hjust=0),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

