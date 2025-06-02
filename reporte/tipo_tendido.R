library(here)
source(here("datos","leer-datos-lp.R"))

library(tidyverse)
library(ggrepel)

# Filtrar viviendas con conexión y seleccionar sólo tendido
tendido_con_conexion <- datos %>%
  filter(conexion_electricidad != "No posee conexión a la red eléctrica en la vivienda") %>%
  select(
    tendido_electrico
  )

# Obtener frecuencia de cada grupo de tendido
tendido_tipos <- tendido_con_conexion %>% group_by(tendido_electrico) %>% 
  summarize(cant = n())

datos_grafico <- tendido_tipos %>%
  mutate(
    porc = round( tendido_tipos$cant / sum(tendido_tipos$cant) * 100, 1),
    grupo = tendido_electrico
  ) %>%
  mutate(
    csum=rev(cumsum(rev(porc))), 
    pos_etiq=porc/2+lead(csum,1),
    pos_etiq=if_else(is.na(pos_etiq),porc/2,pos_etiq)
  ) #Posición de las etiquetas

# Gráfico
ggplot(datos_grafico, aes(x = 3 , y = porc, fill = fct_inorder(grupo))) +
  geom_col(width = 1, color = "black") + 
  coord_polar(theta = "y") +  
  scale_fill_manual(values = c("#FC8D59", "#91CF60") ) +  # Paleta de colores que refleje el contraste de las condiciones
  #scale_fill_manual(values = brewer.pal(3, "RdYlGn")[c(1, 3) ) +  # (sino con library(RColorBrewer))
  xlim(c(0.5,4)) +
  geom_label_repel(
    aes(y = pos_etiq, label = paste0(porc, "%")),
    size = 4.5,
    nudge_x = 0.95,
    show.legend = FALSE) + 
  guides(fill = guide_legend(title = "Tipo de tendido eléctrico dentro de la vivienda")) + 
  labs(  
    title = "Distribución de los hogares según calidad del tendido eléctrico\nen los barrios relevados",
    caption = "Fuente: Relevamiento de Condiciones Habitacionales 2022, La Poderosa"
  ) +
  theme_void()+
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, vjust=-2),
    plot.caption = element_text(size=8, hjust=0),
    plot.margin = margin(0,0,15,0),
    
    legend.background = element_rect(color = "black", linewidth  = 0.5),
    legend.spacing = unit(0.5, "cm"),
    legend.margin = margin(10, 10, 10, 10),
    # legend.position = "right",   # Posicionamiento de la leyenda (opcional)
    # legend.justification = c(0,.95),
  )
