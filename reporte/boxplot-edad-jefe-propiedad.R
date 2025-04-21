library(here)
source(here("datos","leer-datos-lp.R"))

library(tidyverse)

datos_limpios <- datos %>%
  mutate(
    propio = case_when(propiedad %in% c("Propio con algún comprobante de tenencia","Propio sin títulos")~"Propia",
                       TRUE ~ "No propia"),
  )

datos_jefe_propiedad <- datos_limpios %>%
  select(
    edad_jefe_hogar, propio
  )

ggplot(datos_jefe_propiedad) +
  aes(x = propio, y = edad_jefe_hogar, fill=propio) +
  geom_boxplot(show.legend = F) +
  labs(x = "Condición de propiedad", y = "Edad del jefe del hogar") +
  ggtitle("Distribución de la edad del jefe del hogar según condición de propiedad") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_classic()

