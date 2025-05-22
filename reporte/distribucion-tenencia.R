library(here)
source(here("datos","leer-datos-lp.R"))

library(tidyverse)
library(ggrepel)

# Para introducir los datos, queremos visualizar la proporción de personas que residen en una vivienda propia y 
# la proporción que residen en una no propia

# NOTA: Agruparemos todo lo que sea distinto de "propio" en "no propio" (Es decir,Prestado-Alquilado-Ocupado-Otro)
# Además, agruparemos en "propio" la vivienda "Propio con título" y "Propio sin título".

# Re-clasificamos las variables (según criterio [VER NOTA])
datos_refactorizados<- datos %>%
  mutate(propiedad_reclasificada=case_when(propiedad %in% c("Propio con algún comprobante de tenencia","Propio sin títulos")~"Propia",
                                           TRUE ~ "No propia"))

#Datos para el gráfico
datos_grafico_tenencia <- datos_refactorizados %>% 
  count(propiedad_reclasificada) %>% mutate(valor=round(n/sum(n)*100,1), #Cuenta cuantos casos hay por grupo y calcula su porcentaje
                                            grupo=propiedad_reclasificada) %>% #Lo renombramos por simplificación
  mutate(csum=rev(cumsum(rev(valor))), #Ubicamos etiquetas
         pos=valor/2+lead(csum,1),  #Posición de las etiquetas
         pos=if_else(is.na(pos),valor/2,pos))  #Correguimos etiqueta NA

# Gráfico
ggplot(datos_grafico_tenencia, aes(x = 3 , y = valor, fill = fct_inorder(grupo))) +  #Mantiene el orden original de los grupos.
  geom_col(width = 1, color = "brown") + # Ponemos las barras como columnas, para simular porciones del sector circular.
  coord_polar(theta = "y") +   # Lo convertimos en un grafico de torta.
  scale_fill_brewer(palette = "Pastel1") +  #color del gráfico
  xlim(c(0.5,4)) + #Hace el efecto "Donut"
  geom_label_repel( #Para poner las etiquetas por fuera.
    aes(y = pos, label = paste0(valor, "%")),  # Evitamos superposición de etiquetas , las agregamos como flechas.
    size = 4.5,
    nudge_x = 0.95, # Aleja las etiquetas
    show.legend = FALSE) + 
  guides(fill = guide_legend(title = "Tipo de tenencia de la vivienda")) + #Leyenda
  labs(  #Título y nota
    title = "Distribución de los hogares según tipo de tenencia de la vivienda\nen los barrios relevados",
    caption = "Fuente: Relevamiento de Condiciones Habitacionles 2022, La Poderosa"
  ) +
  #Estilo del gráfico
  theme_void()+
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, vjust=-2),
    plot.caption = element_text(size=8, hjust=0),
    plot.margin = margin(0,0,15,0),
    
    legend.background = element_rect(color = "black", linewidth  = 0.5),  # Borde de fondo de la leyenda
    legend.spacing = unit(0.5, "cm"),  # Espacio entre los elementos de la leyenda
    legend.margin = margin(10, 10, 10, 10)  # Espacio entre el borde de la leyenda y los elementos
  )

#------------------------------

# Realizaremos la moda de cada grupo, para averiguar cuál es el que lo lidera
moda_tenencia_no_propia<-datos_refactorizados %>%
  filter(propiedad_reclasificada == "No propia") %>%
  count(propiedad, sort = TRUE)%>%
  slice(1)

moda_tenencia_propia<-datos_refactorizados %>%
  filter(propiedad_reclasificada == "Propia") %>%
  count(propiedad, sort = TRUE)%>%
  slice(1)

#--------------------------------

# Filtramos por tipo de tenencia y contamos por subgrupo dentro de la misma.
datos_tenencia_no_propia <- datos_refactorizados %>%
  filter(propiedad_reclasificada == "No propia") %>%
  count(propiedad)

datos_tenencia_propia <- datos_refactorizados %>%
  filter(propiedad_reclasificada == "Propia") %>%
  count(propiedad)

# Calculamos el total de cada grupo
total_tenencia<-datos_refactorizados %>%
  count(propiedad_reclasificada)


# Porcentajes dentro de cada tipo de tenencia (Ejemplo: El x% de la tenencia no propia está constituida por...)
total_tenencia_no_propia_tabla_porcentajes<-datos_tenencia_no_propia %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1))

total_tenencia_propia_tabla_porcentajes<-datos_tenencia_propia %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1))


##---------------------

# Gráfico de barras: tenencia no propia
ggplot(datos_tenencia_no_propia, aes(x = fct_reorder(propiedad, n), y = n)) +
  geom_col(fill = "#FF9999", color = "black") +
  geom_text(aes(label = n), hjust = 0.5, size = 4,vjust=4) +  #Etiquetas de cantidad (n) 
  labs(
    title = "Distribución de tipos de tenencia entre viviendas no propias",
    x = "Tipo de tenencia",
    y = "Cantidad de viviendas",
    caption = "Fuente: Relevamiento de Condiciones Habitacionales 2022, La Poderosa"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.text = element_text(size = 10),
    plot.caption = element_text(size = 8, hjust = 0)
  ) +
  expand_limits(y = max(datos_tenencia_no_propia$n) * 1.1)  # Espacio para que el número no se corte

# Gráfico de barras: tenencia propia
ggplot(datos_tenencia_propia, aes(x = fct_reorder(propiedad, n), y = n)) +
  geom_col(fill = "#ad5e9d", color = "black") +
  geom_text(aes(label = n), hjust = 0.5, size = 4,vjust=5) +  
  labs(
    title = "Distribución de tipos de tenencia entre viviendas propias",
    x = "Tipo de tenencia",
    y = "Cantidad de viviendas",
    caption = "Fuente: Relevamiento de Condiciones Habitacionales 2022, La Poderosa"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.text = element_text(size = 10),
    plot.caption = element_text(size = 8, hjust = 0)
  ) +
  expand_limits(y = max(datos_tenencia_propia$n) * 1.1) 

