library(here)
source(here("datos","leer-datos-lp.R"))

library(tidyverse)
library(ggrepel)

# Para introducir los datos, queremos visualizar la proporción de personas que residen en una vivienda propia y 
# a proporción que residen en una no propia
# NOTA: Agruparemos todo lo que sea distinto de "propio" en "no propio" (Es decir,Prestado-Alquilado-Ocupado-Otro)
# Además, agruparemos en "propio" la vivienda "Propio con título" y "Propio sin título".

# Re-clasificamos las variables (según criterio [VER NOTA])
datos_refactorizados<- datos %>%
  mutate(propiedad_reclasificada=case_when(propiedad %in% c("Propio con algún comprobante de tenencia","Propio sin títulos")~"Propia",
                                           TRUE ~ "No propia"))
# hacer el gráfico(datos)
grafico_torta <- datos_refactorizados %>% 
  count(propiedad_reclasificada) %>% mutate(valor=round(n/sum(n)*100,1), #Cuanta cuantos casos hay x grupo y calcula su porcentaje
                                            grupo=propiedad_reclasificada) %>% #lo renombramos
  mutate(csum=rev(cumsum(rev(valor))), #ubicamos etiquetas
         pos=valor/2+lead(csum,1),  #posición etiquetas
         pos=if_else(is.na(pos),valor/2,pos))  #Correguimos última etiqueta

# Gráfico
ggplot(grafico_torta, aes(x = 3 , y = valor, fill = fct_inorder(grupo))) +  #valor=proprción ,fct_inorder mantiene el orden original de los grupos.
  geom_col(width = 1, color = "brown") + #barras==columnas (porciones de la torta)
  coord_polar(theta = "y") +   #ahora es un grafico de torta.
  scale_fill_brewer(palette = "Pastel1") +  #color.
  #EFECTO DONUT:
  xlim(c(0.5,4)) +
  #ETIQUETAS POR FUERA
  geom_label_repel(
    aes(y = pos, label = paste0(valor, "%")),  #la fun geom_label_repel evit que las etiquetas se superpongan. Agregamos las etiquetas(porcentaje) fuera del gráfico estilo flecha
    size = 4.5,
    nudge_x = 0.95, #aleja las etiquetas
    show.legend = FALSE) + 
  #leyenda
  guides(fill = guide_legend(title = "Tipo de tenencia de la vivienda")) +
  #Titulo
  labs(
    title = "Distribución de los hogares según tipo de tenencia de la vivienda\nDatos de barrios populares de Argentina encuestados, 2023.",
    caption="NOTA: Los datos de vivienda propia con comprobante de propiedad y sin título se han agrupado en vivienda propia \n Los datos de vivienda alquilada, prestada, ocupada u otro se han agrupado en vivienda no propia",
  ) +
  #Estilo-gráfico
  theme_void()+
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5,vjust=-2),
    plot.caption=element_text(size=8,face="italic",hjust=0.5,vjust=5),
    
    legend.background = element_rect(color = "black", linewidth  = 0.5),  # Borde de fondo de la leyenda
    legend.spacing = unit(0.5, "cm"),  # Espacio entre los elementos de la leyenda
    legend.margin = margin(10, 10, 10, 10)  # Espacio entre el borde de la leyenda y los elementos
  )

# Realizaremos la moda de cada grupo, para averiguar cuál es el que lo lidera

moda_tenencia_no_propia<-datos_refactorizados %>%
  filter(propiedad_reclasificada == "No propia") %>%
  count(propiedad, sort = TRUE)%>%
  slice(1)

moda_tenencia_propia<-datos_refactorizados %>%
  filter(propiedad_reclasificada == "Propia") %>%
  count(propiedad, sort = TRUE)%>%
  slice(1)

print(moda_tenencia_no_propia)

