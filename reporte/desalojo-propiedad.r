setwd("/home/ariel/GitHub/TP1-PyE/datos")

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
                     "origen_agua", "agua_embotellada", "presion_agua", "agua_en_altura", "almacenamiento_agua", "baño", "higiene_
                     fuera_vivienda", "baño_compartido", "baño_descarga", "tipo_desagüe", "agua_cocina", "agua_caliente_cocina", "agua_baño", "agua_caliente_baño",
                     "energia_cocina_gas_natural", "energia_cocina_garrafa", "energia_cocina_electricidad", "energia_cocina_leña_carbon", "energia_cocina_sin", "energia_calefaccion_gas_natural", "energia_calefaccion_garrafa", "energia_calefaccion_electricidad", "energia_calefaccion_leña_carbon", "energia_calefaccion_sin", "energia_calefaccion_no_necesita", "ventilacion_calefaccion",
                     "conexion_electricidad", "tendido_electrico", "perdida_electrodomesticos_electricidad", "incendios_electricidad", "frecuencia_corte_electricidad_verano","frecuencia_corte_electricidad_invierno", 
                     "banda_ancha", "celular_internet", "abonos_moviles", "computadoras", "telefonos",
                     "contrapiso", "material_piso", "material_techo", "aislamiento_techo", "material_puerta_cemento", "material_puerta_madera", "material_puerta_ceramico", "material_puerta_sin", "material_paredes_exteriores", "terminacion_exterior", "tipo_terrminacion_exterior", "terminacion_pintura", "humedad_dormitorios", "humedad_cocina", "humedad_baño", "humedad_living", "humedad_sin", "humedad_otro", "derrumbe_dormitorios", "derrumbe_cocina", "derrumbe_baño", "derrumbe_living", "derrumbe_sin", "derrumbe_otro", "trabajo_vivienda", "tipo_trabajo_vivienda",
                     "calle_asfaltqada", "salida_calle", "vereda_calle", "alumbrado_publico", "calificacion_arbolado", "plagas", "plagas_cucarachas", "plagas_mosquitos", "plagas_ratas", 
                     "esparcimiento_polideportivo", "esparcimiento_natatorio", "esparcimiento_playon", "esparcimiento_futbol", "esparcimiento_ejercicio", "esparcimiento_skatepark", "esparcimiento_balneario", "esparcimiento_sin", "esparcimiento_otro", "frecuencia_esparcimiento", "verdes_placita", "verdes_plaza", "verdes_parque", "verdes_sin", "frecuencia_verdes", "frecuencia_colectivo", "frecuencia_colectivo_dispar", "acceso_bici_publica", "basurales", "cesto_cuadra", "eliminacion_residuos", "recoleccion_residuos_municipio", "riesgo_inundacion"
)

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
       y = "Porcentaje", 
       fill = "Tuvo al menos un intento de desalojo",
       title = "Distribución de viviendas con al menos un intento de desalojo,\nsegún condición de propiedad"
  ) +
  geom_text(aes(y = datos_desalojo_propiedad$porcentaje / 2, label = paste0(datos_desalojo_propiedad$porcentaje, "%"),), position = position_dodge(width = 0.9), size = 4.5) + 
  theme_classic() 
