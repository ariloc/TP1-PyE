library(here)
source(here("datos","leer-datos-lp.R"))

library(tidyverse)

#######
# Analizamos rápidamente si la propiedad afecta los medios de calefacción habituales

datos_limpios_calefaccion <- datos %>%
  mutate(
    energia_calefaccion_gas_natural = ifelse( is.na(energia_calefaccion_gas_natural), 0, 1 ), 
    energia_calefaccion_garrafa = ifelse( is.na(energia_calefaccion_garrafa), 0, 1 ), 
    energia_calefaccion_electricidad = ifelse( is.na(energia_calefaccion_electricidad), 0, 1 ), 
    energia_calefaccion_leña_carbon = ifelse( is.na(energia_calefaccion_leña_carbon), 0, 1 ), 
    energia_calefaccion_sin = ifelse( is.na(energia_calefaccion_sin), 0, 1 ),
    energia_calefaccion_no_necesita = ifelse( is.na(energia_calefaccion_no_necesita), 0, 1 )
)

datos_calefaccion_propiedad <- datos_limpios_calefaccion %>%
  select(
    id, provincia, barrio, propiedad, energia_calefaccion_gas_natural, energia_calefaccion_garrafa, energia_calefaccion_electricidad, energia_calefaccion_leña_carbon, energia_calefaccion_sin, energia_calefaccion_no_necesita
  )


datos_calefaccion_propio <-datos_calefaccion_propiedad %>%
  filter(propiedad == "Propio sin títulos" | propiedad == "Propio con algún comprobante de tenencia")

datos_calefaccion_no_propio <-datos_calefaccion_propiedad %>%
  filter(propiedad != "Propio sin títulos" & propiedad != "Propio con algún comprobante de tenencia")

datos_calefaccion_propio %>%
  
  mutate(
    gas_natural = energia_calefaccion_gas_natural,
    garrafa = energia_calefaccion_garrafa,
    electricidad = energia_calefaccion_electricidad,
    leña_carbon = energia_calefaccion_leña_carbon,
    no_necesita = energia_calefaccion_no_necesita,
    sin = energia_calefaccion_sin
  ) %>%
  
  summarize(gas_natural = sum(gas_natural),
            garrafa = sum(garrafa),
            electricidad = sum(electricidad),
            leña_carbon = sum(leña_carbon),
            no_necesita = sum(no_necesita),
            sin = sum(sin)) %>%
  
  pivot_longer(cols = c(gas_natural, garrafa, electricidad, leña_carbon, no_necesita, sin),
               names_to = "calefaccion",
               values_to = "cant") %>%
  
  mutate(
    porc = paste0(round( cant / nrow(datos_calefaccion_propio) * 100, 2),"%")
  ) %>%
  
  arrange(desc(cant))

datos_calefaccion_no_propio %>%
  
  mutate(
    gas_natural = energia_calefaccion_gas_natural,
    garrafa = energia_calefaccion_garrafa,
    electricidad = energia_calefaccion_electricidad,
    leña_carbon = energia_calefaccion_leña_carbon,
    no_necesita = energia_calefaccion_no_necesita,
    sin = energia_calefaccion_sin
  ) %>%
  
  summarize(gas_natural = sum(gas_natural),
            garrafa = sum(garrafa),
            electricidad = sum(electricidad),
            leña_carbon = sum(leña_carbon),
            no_necesita = sum(no_necesita),
            sin = sum(sin)) %>%
  
  pivot_longer(cols = c(gas_natural, garrafa, electricidad, leña_carbon, no_necesita, sin),
               names_to = "calefaccion",
               values_to = "cant") %>%
  
  mutate(
    porc = paste0(round( cant / nrow(datos_calefaccion_no_propio) * 100, 2),"%")
  ) %>%
  
  arrange(desc(cant))

#########
# Analizamos cantidad de integrantes dependiendo de la condición de propiedad de la vivienda

datos_personas_propiedad <- datos %>%
  select(
    propiedad, integrantes
  )

datos_personas_propio <- datos_personas_propiedad %>%
  filter(propiedad == "Propio sin títulos" | propiedad == "Propio con algún comprobante de tenencia")

datos_personas_no_propio <- datos_personas_propiedad %>%
  filter(propiedad != "Propio sin títulos" & propiedad != "Propio con algún comprobante de tenencia")

attach(datos_personas_propio)
ggplot(datos_personas_propio) +
  aes(x = integrantes) + 
  geom_bar(width = 0.10) +
  scale_x_continuous() +
  labs(y = "Freq. de cant. integantes", 
       x = "Cant. integrantes")+
  theme_classic()
detach(datos_personas_propio)

attach(datos_personas_no_propio)
ggplot(datos_personas_no_propio) +
  aes(x = integrantes) + 
  geom_bar(width = 0.10) +
  scale_x_continuous() +
  labs(y = "Freq. de cant. integantes", 
       x = "Cant. integrantes")+
  theme_classic()
detach(datos_personas_no_propio)


#########
# Analizamos edad del jefe del hogar de la condición de propiedad de la vivienda

datos_limpios_jefe <- datos %>%
  mutate(
    edad_jefe_int = cut(edad_jefe_hogar,
                        breaks = c(0, 21, 30, 40, 50, 60, 100),
                        right = F),
  )

datos_edad_jefe_propiedad <- datos_limpios_jefe %>%
  select(
    propiedad, edad_jefe_int
  )

datos_edad_jefe_propio <- datos_edad_jefe_propiedad %>%
  filter(propiedad == "Propio sin títulos" | propiedad == "Propio con algún comprobante de tenencia")

datos_edad_jefe_no_propio <- datos_edad_jefe_propiedad %>%
  filter(propiedad != "Propio sin títulos" & propiedad != "Propio con algún comprobante de tenencia")

attach(datos_edad_jefe_propio)

datos_edad_jefe_propio %>%
  ggplot() + 
  aes(x = edad_jefe_int, y = ..count.. / sum(..count..)) + # Porcentajes
  geom_bar(width = 0.75,   # Ancho de barras
           fill = '#7ed021',  # Color de relleno 
           col = "black",  # Color de línea
           alpha = 0.6) +  # Transparencia
  labs(y = "Porcentaje en rango etario", x = "Intervalo edad jefe del hogar") + # Nombres de ejes
  ggtitle("Porcentajes de rangos etarios de jefe del hogar para viviendas propias") +
  coord_flip() + # Barras horizontales o verticales
  theme_classic() # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/

detach(datos_edad_jefe_propio)

attach(datos_edad_jefe_no_propio)

datos_edad_jefe_no_propio %>%
  ggplot() + 
  aes(x = edad_jefe_int, y = ..count.. / sum(..count..)) + # Porcentajes
  geom_bar(width = 0.75,   # Ancho de barras
           fill = '#7ed021',  # Color de relleno 
           col = "black",  # Color de línea
           alpha = 0.6) +  # Transparencia
  labs(y = "Porcentaje en rango etario", x = "Intervalo edad jefe del hogar") + # Nombres de ejes
  ggtitle("Porcentajes de rangos etarios de jefe del hogar para viviendas no propias") +
  coord_flip() + # Barras horizontales o verticales
  theme_classic() # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/

detach(datos_edad_jefe_no_propio)


#########
# Analizamos cantidad de encuestados según estado de propiedad de la vivienda

datos_propiedad <- datos %>%
  select(
    propiedad
  )

attach(datos_propiedad)
datos_propiedad %>% group_by(propiedad) %>%
  summarize(cant = n())
detach(datos_propiedad)

#########
# Analizamos porcentaje de personas con intentos de desalojo según si poseen certificado de RENABAP o no

datos_limpios <- datos
datos_limpios$renabap[datos_limpios$renabap %in% "No corresponde"] <- "Sí"
datos_limpios <- datos_limpios %>%
  mutate(
    renabap = as.factor(renabap)
  )

datos_desalojo_renabap <- datos_limpios %>%
  select(
    renabap, intento_desalojo
  )

attach(datos_desalojo_renabap)
datos_desalojo_renabap %>% 
  ggplot() + 
  aes(renabap, fill = intento_desalojo) +
  geom_bar(position="dodge", colour="black") +
  labs(x = "Tiene RENABAP", 
       y = "Porcentaje intento desalojo", 
       fill = "Intento desalojo") +
  geom_bar(position = "fill") # position = fill, dodge, stack 
detach(datos_desalojo_renabap)

