library(readxl)
library(tidyverse)
library(tidygeocoder)
library(ggplot2)
library(ggforce)
library(rnaturalearth)
library(rnaturalearthhires)
library(cowplot)

setwd("/home/ariel/GitHub/TP1-PyE/datos")

datos <- readxl::read_excel("Barrios_Usuarios.xlsx", 
                                col_names = FALSE, 
                                skip = 1)

attach(datos)

colnames(datos) <- c("region","provincia","barrio","encuestas")

detach(datos)

encuestas_limpias <- datos %>% slice(-n())    # Quitar última fila con totales
for(i in 2:(dim(encuestas_limpias)[1])) {
  if (is.na(encuestas_limpias$provincia[i])) {
    encuestas_limpias$provincia[i] <- encuestas_limpias$provincia[i-1]
    encuestas_limpias$barrio[i] <- encuestas_limpias$barrio[i-1]

  }
}                                             # Completar filas con NA por celdas combinadas

encuestas_limpias$barrio[encuestas_limpias$barrio %in% "Las Quintas"] <- "Las Quintas, Santa Teresita" # Hay dos Las Quintas en Buenos Aires, especificando según mapa
encuestas_limpias <- encuestas_limpias %>% 
  group_by(provincia,barrio) %>% 
  summarise(encuestas = sum(encuestas), .groups='drop') %>%             # Sumar múltiples encuestas por barrio
  mutate(provincia = recode(provincia, "BsAs" = "Provincia de Buenos Aires")) %>%    # Renombrar Buenos Aires para que sea reconocido por el geocoding
  mutate(ubicacion = paste("Barrio", paste(barrio, provincia, "Argentina", sep=", ")))   # Formatear ubicaciones con formato "Barrio [barrio], [provincia]" para geocoding

encuestas_coords <- encuestas_limpias %>%                               # Geocoding -> obtener coords de los barrios
  geocode(
    ubicacion,
    method='google',                                                    # Necesita una API key. Se debe indicar en la variable de entorno GOOGLEGEOCODE_API_KEY (.Renviron a nivel proyecto)
    lat = latitude, 
    long = longitude
  )

argentina <- ne_states(country = "Argentina", returnclass = "sf")
malvinas <- ne_states(country = "Falkland Islands", returnclass = "sf")

mapa_regiones <- tibble(
  name = c("Buenos Aires", "Catamarca", "Chaco", "Chubut", "Córdoba", "Corrientes", "Entre Ríos",
           "Formosa", "Jujuy", "La Pampa", "La Rioja", "Mendoza", "Misiones", "Neuquén", "Río Negro", 
           "Salta", "San Juan", "San Luis", "Santa Cruz", "Santa Fe", "Santiago del Estero",
           "Tierra del Fuego", "Tucumán", "Ciudad de Buenos Aires", "Falkland Islands"),
  region_eco = c("PBA", "NOA", "NEA", "Patagonia", "Centro", "NEA", "Centro",
             "NEA", "NOA", "Patagonia", "NOA", "Cuyo", "NEA", "Patagonia", "Patagonia",
             "NOA", "Cuyo", "Cuyo", "Patagonia", "Centro", "NOA", 
             "Patagonia", "NOA", "CABA", "Patagonia")
) # Fuente: https://www.argentina.gob.ar/economia/politicaeconomica/regionalysectorial/economiasregionales/icer

argentina <- argentina %>%
  left_join(mapa_regiones, by = "name")
malvinas <- malvinas %>%
  left_join(mapa_regiones, by = "name")

# Mapa principal
p1 <- ggplot() +
  geom_sf(data = argentina, aes(fill = region_eco), color = "black") +
  geom_sf(data = malvinas, aes(fill = region_eco), color = "black") +
  geom_point(data = encuestas_coords, aes(x = longitude, y = latitude), shape=21, fill = "black", stroke=0.5, size=2, color="white") +
  scale_fill_brewer(palette = "Set3", name = "Región Económica") +
  theme_void()

# Zoom a CABA
p2 <- ggplot() +
  geom_sf(data = argentina, aes(fill = region_eco), color = "black") +
  geom_sf(data = malvinas, aes(fill = region_eco), color = "black") +
  geom_point(data = encuestas_coords, aes(x = longitude, y = latitude), shape=21, fill = "black", stroke=0.5, size=2, color="white") +
  scale_fill_brewer(palette = "Set3", name = "Región Económica") +
  coord_sf(xlim = c(-58.6, -58.3), ylim = c(-34.775, -34.5)) + 
  theme_void() +
  guides(fill = "none")


p1 + geom_point() +
  
  geom_segment(aes(x = -58.6, y = -34.775, xend = -53, yend = -34.25), color = "black") +   # Líneas al zoom
  geom_segment(aes(x = -58.6, y = -34.775, xend = -57, yend = -30.75), color = "black") +

  annotation_custom(        # Fragmento zoom a CABA
    grob = ggplotGrob(p2),
    xmin = -53, xmax = -57,
    ymin = -36, ymax = -29
  ) + 
  annotate("rect", xmin = -53, xmax = -57, ymin = -34.25, ymax = -30.75,    # Rectángulo con borde rayado al fragmento con zoom a CABA
           color = "black", fill = NA, linetype = "dashed")

