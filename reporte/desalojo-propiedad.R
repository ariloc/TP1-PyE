library(here)
source(here("datos","leer-datos-lp.R"))

library(tidyverse)

datos_limpios <- datos %>%
  mutate(
    propio = case_when(propiedad %in% c("Propio con algún comprobante de tenencia","Propio sin títulos")~"Propia",
                       TRUE ~ "No propia"),
  )

datos_desalojo_propiedad <- datos_limpios %>%
  select(
    propio, intento_desalojo
  ) %>%
  group_by(propio, intento_desalojo) %>%
  summarize(cant = n(), .groups = "drop_last") %>%
  mutate(
    porcentaje = round(cant / sum(cant) * 100,1)
  )

datos_desalojo_propiedad %>% 
  ggplot() + 
  aes(x = propio, y = porcentaje, fill = intento_desalojo) +
  geom_bar(position="dodge", stat="identity", colour="black") +
  scale_fill_manual(values = c("Sí" = "palegreen", "No" = "lightcoral")) +
  labs(x = "Condición de propiedad", 
       y = "Porcentaje de viviendas", 
       fill = "Tuvo al menos un intento de desalojo",
       title = "Distribución de viviendas con al menos un intento de desalojo\nsegún condición de propiedad en los barrios relevados",
       caption = "Fuente: Relevamiento de Condiciones Habitacionales 2022, La Poderosa"
  ) +
  geom_text(aes(y = datos_desalojo_propiedad$porcentaje / 2, label = paste0(datos_desalojo_propiedad$porcentaje, "%"),), position = position_dodge(width = 0.9), size = 4.5) + 
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

