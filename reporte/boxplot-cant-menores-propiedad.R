library(here)
source(here("datos","leer-datos-lp.R"))

library(tidyverse)

datos_limpios <- datos %>%
  mutate(
    propio = case_when(propiedad %in% c("Propio con algún comprobante de tenencia","Propio sin títulos")~"Propia",
                       TRUE ~ "No propia"),
  )

datos_menores_tenencia <- datos_limpios %>%
  select(
    menores_edad, propio
  )

ggplot(datos_menores_tenencia) +
  aes(x = propio, y = menores_edad, fill=propio) +
  geom_boxplot(show.legend = F) +
  labs(
    x = "Condición de propiedad", 
    y = "Cantidad de menores en el hogar",
    title = "Distribución de cantidad de menores en la vivienda según condición de propiedad\nen los barrios relevados",
    caption = "Fuente: Relevamiento de Condiciones Habitacionales 2022, La Poderosa"
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_classic() +
  theme(
    plot.tag.position = "bottom",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5,vjust=-2), 
    plot.caption = element_text(size=8, hjust=0),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

