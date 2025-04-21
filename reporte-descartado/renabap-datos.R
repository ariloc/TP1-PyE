library(readxl)
library(ggrepel)
library(tidyverse)
library(here)

datos_renabap <- read.csv(here("datos", "renabap-datos-barrios-csv.csv"))
datos_encuestas <- readxl::read_excel(here("datos", "Barrios_Usuarios.xlsx"), col_names = FALSE, skip = 1)

attach(datos_encuestas)
colnames(datos_encuestas) <- c("region","provincia","barrio","encuestas")
detach(datos_encuestas)

encuestas_limpias <- datos_encuestas %>% slice(-n())    # Quitar última fila con totales
for(i in 2:(dim(encuestas_limpias)[1])) {
  if (is.na(encuestas_limpias$provincia[i])) {
    encuestas_limpias$provincia[i] <- encuestas_limpias$provincia[i-1]
    encuestas_limpias$barrio[i] <- encuestas_limpias$barrio[i-1]
    
  }
}      

barrios_encuestas <- encuestas_limpias %>%
  select(barrio, provincia) %>% unique
  # mutate(
  #   barrio = tolower(barrio),
  # )
  

# barrios_renabap <- datos_renabap %>%
#     select(nombre_barrio, localidad, provincia) %>%
#     mutate(
#       nombre_barrio = tolower(nombre_barrio),
#       localidad = tolower(localidad)
#     )

# barrios_encuestas_renabap <- barrios_encuestas %>%
#   filter((barrio %in% barrios_renabap$nombre_barrio) | 
#           (barrio %in% barrios_renabap$localidad) # Algunos barrios encuestados son nombres considerados localidades en el RENABAP
#   )
# barrios_encuestas_renabap <- distinct(barrios_encuestas_renabap)


### Al final se realiza una revisión manual en la página del RENABAP debido a inconsistencias en los nombres de los barrios
### https://www.argentina.gob.ar/habitat/integracion-socio-urbana/renabap/listado-renabap

print(barrios_encuestas, n = dim(barrios_encuestas)[1])
barrios_encuestas_renabap <- tibble(
  barrio = barrios_encuestas$barrio,
  provincia = barrios_encuestas$provincia,
  renabap = c(FALSE, # Constitución
              TRUE,  # "Virgen Desatanudos"
              FALSE, # Alberdi
              FALSE, # Gualeyán
              TRUE,  # "La Cariñosa"
              TRUE,  # "Chalet"
              TRUE,  # "Zavaleta"
              TRUE,  # "Villa 20"
              TRUE,  # "Padre Mugica (Ex Villa 31 y 31 Bis)"
              TRUE,  # "Los Álamos"
              TRUE,  # Como localidad "Altos De San Lorenzo", múltiples barrios
              TRUE,  # "Las Dalias"
              TRUE,  # "Villa Cordobita"
              TRUE,  # "Las Quintas"
              TRUE,  # "Río Paraná"
              TRUE,  # "Chacra 159"
              TRUE,  # "San Juan Bautista"
              FALSE, # Chacra 136
              TRUE,  # "Fiske Menuco"
              TRUE,  # "Madres A La Lucha"
              FALSE, # 28 de Septiembre
              TRUE,  # "Once de Enero"
              TRUE   # "Bosco II"
  )
)

donut_renabap <- barrios_encuestas_renabap %>%
  mutate( renabap = factor(renabap,
                           levels = c(TRUE, FALSE),
                           labels = c("Sí","No"))) %>%
  group_by(renabap) %>%
  summarize(cant = n()) %>%
  mutate(
    porcentaje = round(cant / sum(cant) * 100, 1),
    csum=rev(cumsum(rev(porcentaje))),
    pos=porcentaje/2+lead(csum,1),
    pos=if_else(is.na(pos),porcentaje/2,pos)
  )

ggplot(donut_renabap) +
  aes(x = 3 , y = porcentaje, fill = renabap) +
  xlim(c(0.5,4)) +
  geom_col(width = 1, color = "black") +
  coord_polar(theta = "y", clip = "off") +
  scale_fill_manual(values = c("Sí" = "palegreen", "No" = "lightcoral")) +  #color.

  geom_label_repel(
    aes(y = pos, label = paste0(cant, " (", porcentaje, "%", ")")), 
    size = 4.5,
    nudge_x = 1, 
    show.legend = FALSE) + 

  guides(fill = guide_legend(title = "Registrado en RENABAP")) +

  labs(
    title = "Distribución de barrios populares relevados según si se encuentran\nregistrados en el RENABAP",
  ) +

  theme_void()+
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5,vjust=-10),
    plot.margin = grid::unit(c(0, 0, 0, 0), "lines"),
    
    legend.background = element_rect(color = "black", linewidth  = 0.5),  
    legend.spacing = unit(0.25, "cm"),
    legend.margin = margin(10, 10, 10, 10),
  )

