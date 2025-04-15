setwd("/home/ariel/GitHub/TP1-PyE/datos")

# googledrive::drive_download(as_id("1IRhvzOQkvuspQF3TAsBCI-68i8ya0_hy"), 
                          #  overwrite = T)

datos <- readxl::read_excel("Datos_LP.xlsx", 
                            col_names = FALSE, 
                            skip = 3)
library(tidyverse)
library(janitor)
library(ggplot2)

attach(datos)

colnames(datos) <- c("id", "provincia", "barrio",
                     "edad_jefe_hogar","tiempo_residencia","integrantes","grupos_familiares", "varones","mujeres","genero_disidente", "menores_edad", "cant_discapacitados",
                     "dormitorios", "personas_dormitorio",
                     "renabap", "intento_desalojo", "veces_desalojo", "tiempo_desde_desalojo", "propiedad", "contrato_alquiler", "costo_alquiler", "aumento_alquiler", "porcentaje_aumento_alquiler",
                     "origen_agua", "agua_embotellada", "presion_agua", "agua_en_altura", "almacenamiento_agua", "baño", "higiene_fuera_vivienda", "baño_compartido", "baño_descarga", "tipo_desagüe", "agua_cocina", "agua_caliente_cocina", "agua_baño", "agua_caliente_baño",
                     "energia_cocina_gas_natural", "energia_cocina_garrafa", "energia_cocina_electricidad", "energia_cocina_leña_carbon", "energia_cocina_sin", "energia_calefaccion_gas_natural", "energia_calefaccion_garrafa", "energia_calefaccion_electricidad", "energia_calefaccion_leña_carbon", "energia_calefaccion_sin", "energia_calefaccion_no_necesita", "ventilacion_calefaccion",
                     "conexion_electricidad", "tendido_electrico", "perdida_electrodomesticos_electricidad", "incendios_electricidad", "frecuencia_corte_electricidad_verano","frecuencia_corte_electricidad_invierno", 
                     "banda_ancha", "celular_internet", "abonos_moviles", "computadoras", "telefonos",
                     "contrapiso", "material_piso", "material_techo", "aislamiento_techo", "material_puerta_cemento", "material_puerta_madera", "material_puerta_ceramico", "material_puerta_sin", "material_paredes_exteriores", "terminacion_exterior", "tipo_terrminacion_exterior", "terminacion_pintura", "humedad_dormitorios", "humedad_cocina", "humedad_baño", "humedad_living", "humedad_sin", "humedad_otro", "derrumbe_dormitorios", "derrumbe_cocina", "derrumbe_baño", "derrumbe_living", "derrumbe_sin", "derrumbe_otro", "trabajo_vivienda", "tipo_trabajo_vivienda",
                     "calle_asfaltqada", "salida_calle", "vereda_calle", "alumbrado_publico", "calificacion_arbolado", "plagas", "plagas_cucarachas", "plagas_mosquitos", "plagas_ratas", 
                     "esparcimiento_polideportivo", "esparcimiento_natatorio", "esparcimiento_playon", "esparcimiento_futbol", "esparcimiento_ejercicio", "esparcimiento_skatepark", "esparcimiento_balneario", "esparcimiento_sin", "esparcimiento_otro", "frecuencia_esparcimiento", "verdes_placita", "verdes_plaza", "verdes_parque", "verdes_sin", "frecuencia_verdes", "frecuencia_colectivo", "frecuencia_colectivo_dispar", "acceso_bici_publica", "basurales", "cesto_cuadra", "eliminacion_residuos", "recoleccion_residuos_municipio", "riesgo_inundacion"
)

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

#######
# Analizamos rápidamente si la propiedad afecta los medios de calefacción habituales
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

# Analizamos cantidad máxima de integrantes por habitación dependiendo de la condición de propiedad de la vivienda
datos_personas_propiedad <- datos %>%
  select(
    propiedad, personas_dormitorio
  )

datos_personas_propio <- datos_personas_propiedad %>%
  filter(propiedad == "Propio sin títulos" | propiedad == "Propio con algún comprobante de tenencia")

datos_personas_no_propio <- datos_personas_propiedad %>%
  filter(propiedad != "Propio sin títulos" & propiedad != "Propio con algún comprobante de tenencia")

attach(datos_personas_propio)
ggplot(datos_personas_propio) +
  aes(x = personas_dormitorio) + 
  geom_bar(width = 0.10) +
  scale_x_continuous() +
  labs(y = "Freq. de cant. de personas por dormitorio", 
       x = "Cant. de personas por dormitorio")+
  theme_classic()
detach(datos_personas_propio)

attach(datos_personas_no_propio)
ggplot(datos_personas_no_propio) +
  aes(x = personas_dormitorio) + 
  geom_bar(width = 0.10) +
  scale_x_continuous() +
  labs(y = "Freq. de cant. de personas por dormitorio", 
       x = "Cant. de personas por dormitorio")+
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
  
  #aes(x = tiempo) + # Frecuencias absolutas
  #aes(x = reorder(tiempo, tiempo, function(x) -length(x))) + # Ordenar según frecuencia
  aes(x = edad_jefe_int, y = ..count.. / sum(..count..)) + # Porcentajes
  # aes(x = reorder(tiempo, tiempo, function(x) -length(x)), 
  #		y = ..count.. / sum(..count..)) +  # Porcentajes ordenados según frecuencia
  #scale_y_continuous(labels = scales::percent) +    # Eje para porcentajes
  
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
  
  #aes(x = tiempo) + # Frecuencias absolutas
  #aes(x = reorder(tiempo, tiempo, function(x) -length(x))) + # Ordenar según frecuencia
  aes(x = edad_jefe_int, y = ..count.. / sum(..count..)) + # Porcentajes
  # aes(x = reorder(tiempo, tiempo, function(x) -length(x)), 
  #		y = ..count.. / sum(..count..)) +  # Porcentajes ordenados según frecuencia
  #scale_y_continuous(labels = scales::perc# Analizamos edad del jefe del hogar de la condición de propiedad de la vivienda
ent) +    # Eje para porcentajes
  
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
# Analizamos cantidad de encuestado según estado de propiedad de la vivienda
# googledrive::drive_download(as_id("1IRhvzOQkvuspQF3TAsBCI-68i8ya0_hy"), 
#  overwrite = T)
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
    #propio = ifelse( propiedad == "Propio sin títulos" || propiedad == "Propio con algún comprobante de tenencia", 1, 0)
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

#######
# Boxplot de propiedad vs edad de jefe del hogar
