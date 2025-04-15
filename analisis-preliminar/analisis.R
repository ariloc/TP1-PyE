#setwd("/home/ariel/GitHub/TP1-PyE/datos")
#setwd("C:/Users/grass/codigo/Facultad/Probabilidad_Y_Estadistica/TP/TP1-PyE/analisis-preliminar")

#------------------------------------------
#AVISO: En algún gráfico podría aparecer el error de tipo:
## Error en grid.Call(C_convert, x, as.integer(whatfrom), as.integer(whatto), : 
## Viewport has zero dimension(s)
#SOLUCIÓN: Recargue las librerías nuevamente.
#-------------------------------------------

# googledrive::drive_download(as_id("1IRhvzOQkvuspQF3TAsBCI-68i8ya0_hy"), 
                          #  overwrite = T)

datos <- readxl::read_excel("Datos_LP.xlsx", 
                            col_names = FALSE, 
                            skip = 3)

#Instalar librerías si aún no las tiene.
#install.packages("dplyr");install.packages("ggrepel");install.packages("forcats")

library(dplyr)
library(ggrepel)
library(forcats)
library(tidyverse)
library(janitor)
library(ggplot2)

attach(datos)

#Asignamos los nombres de las variables correspondientes a las columnas del excel.
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
  aes(x = renabap, fill = intento_desalojo) +
  labs(x = "Tiene RENABAP", 
       y = "Porcentaje intento desalojo", 
       fill = "Intento desalojo") +
  geom_bar(position = "fill") # position = fill, dodge, stack 
detach(datos_desalojo_renabap)


#///////////////////////////////// ANALISIS DE VARIABLE 1.

# Para introducir los datos, queremos visualizar la proporción de personas que residen en una vivienda propia y 
#a proporción que residen en una no propia
#NOTA: Agruparemos todo lo que sea distinto de "propio" en "no propio" (Es decir,Prestado-Alquilado-Ocupado-Otro)
#Además, agruparemos en "propio" la vivienda "Propio con título" y "Propio sin título".

#Re-clasificamos las variables (según criterio [VER NOTA])
datos_refactorizados<- datos %>%
mutate(propiedad_reclasificada=case_when(propiedad %in% c("Propio con algún comprobante de tenencia","Propio sin títulos")~"Propia",
                                         TRUE ~ "No propia"))
#hacer el gráfico(datos)
grafico_torta <- datos_refactorizados %>% 
  count(propiedad_reclasificada) %>% mutate(valor=round(n/sum(n)*100,1), #Cuanta cuantos casos hay x grupo y calcula su porcentaje
                                            grupo=propiedad_reclasificada) %>% #lo renombramos
  mutate(csum=rev(cumsum(rev(valor))), #ubicamos etiquetas
         pos=valor/2+lead(csum,1),  #posición etiquetas
         pos=if_else(is.na(pos),valor/2,pos))  #Correguimos última etiqueta

##Gráfico nro 2
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
    title = "Gráfico 2. Distribución de los hogares según tipo de tenencia de la vivienda\nDatos de barrios populares de Argentina encuestados, 2023.",
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

#Realizaremos la moda de cada grupo, para averiguar cuál es el que lo lidera

moda_tenencia_no_propia<-datos_refactorizados %>%
  filter(propiedad_reclasificada == "No propia") %>%
  count(propiedad, sort = TRUE)%>%
  slice(1)

moda_tenencia_propia<-datos_refactorizados %>%
  filter(propiedad_reclasificada == "Propia") %>%
  count(propiedad, sort = TRUE)%>%
  slice(1)

print(moda_tenencia_no_propia)


#///////////////////////////////// ANALISIS DE VARIABLE 2.
# Analizamos la frecuencia de la cantidad de personas por habitación , dividiendo la muestra
# en 2 categorías: "tenencia propia" y "tenencia no propia", siguiendo el critero de la NOTA anterior.

datos_no_propios <- datos_refactorizados %>% 
  filter(propiedad_reclasificada == "No propia")

#Gráfico nro 3
ggplot(datos_no_propios, aes(x = personas_dormitorio)) +
  geom_histogram(binwidth = 0.5, fill = "salmon", color = "black") +
  labs(title = "Personas por habitación (Tenencia no propia)",
       x = "Cantidad de personas por habitación",
       y = "Frecuencia",
       tag="Gráfico 3.") +
  theme_minimal()

datos_propios <- datos_refactorizados %>% 
  filter(propiedad_reclasificada == "Propia")

#Gráfico nro 4
ggplot(datos_propios, aes(x = personas_dormitorio)) +
  geom_histogram(binwidth = 0.5, fill = "#00E5EE", color = "black") +
  labs(title = "Personas por habitación (Tenencia propia)",
       x = "Cantidad de personas por habitación",
       y = "Frecuencia",
       tag="Gráfico 4.") +
  theme_minimal()

#Podemos visualizar cierta tendencia en los No propios, analicemos
# las tendencias centrales para verificar/profundizar:

mediana_no_propia_cant_hab=median(datos_no_propios$personas_dormitorio,na.rm=TRUE)
print(mediana_no_propia_cant_hab)

mediana_propia_cant_hab=median(datos_propios$personas_dormitorio,na.rm=TRUE)
print(mediana_propia_cant_hab)

#promedio_no_propia_cant_hab=mean(datos_no_propios$personas_dormitorio,na.rm=TRUE)
#print(promedio_no_propia_cant_hab)

#promedio_propia_cant_hab=mean(datos_propios$personas_dormitorio,na.rm=TRUE)
#print(promedio_propia_cant_hab)

#Claramente mayor gente por habitación vive en las viviendas no propias.

#Costo promedio del alquiler en los barrios populares:
datos_alquiler <- datos %>%
  filter(!is.na(costo_alquiler)) %>%
  count(costo_alquiler)

#Gráfico nro 5
ggplot(datos_alquiler, aes(x = as.factor(costo_alquiler), y = n)) +
  geom_bar(stat = "identity", fill = "indianred2", color = "black") +
  geom_text(aes(label = n), vjust = 2, color = "#8B0000", size = 3.5) + #etiquetas dentro de rectángulos.
  labs(
    title = "Frecuencia del costo del alquiler en barrios populares",
    x = "Costo del alquiler en pesos",
    y = "Cantidad de viviendas",
    tag="Gráfico 5",
  ) +theme(
    plot.tag.position = "bottom",
     plot.title = element_text(size = 12, face = "bold", hjust = 0.5,vjust=-2)) + 
  ylim(0, max(datos_alquiler$n) + 1) +  # Ajuste dinámico del eje Y
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#Calculamos la media aritmética del coste del alquiler:
media_alquiler <- mean(datos$costo_alquiler, na.rm = TRUE)
print(media_alquiler)
#mediana_alquiler<-median(datos$costo_alquiler, na.rm = TRUE)

#Calculemos el mínimo y máximo:
min_alquiler <- min(datos$costo_alquiler, na.rm = TRUE)
print(min_alquiler)
max_alquiler <- max(datos$costo_alquiler, na.rm = TRUE)
print(max_alquiler)

#///////////////////////////////// ANALISIS DE VARIABLE 3.

#Analizaremos los problemas de humedad en los diferentes ambientes de la vivienda de los barrios populares

datos_humedad <- datos %>%
  select(
    humedad_dormitorios, humedad_cocina,humedad_baño,humedad_living,humedad_otro
  )

#Como las respuetas no cuentan con "sí"/"no" , las refactorizamos:
datos_humedad_expandido <- datos_humedad %>%
  mutate(
    dormitorio = !is.na(humedad_dormitorios) & humedad_dormitorios != "",
    cocina = !is.na(humedad_cocina) & humedad_cocina != "",
    baño = !is.na(humedad_baño) & humedad_baño != "",
    living = !is.na(humedad_living) & humedad_living != "",
    otro = !is.na(humedad_otro) & humedad_otro != "",
  )

#TRUE-FALSE
datos_humedad_largo <- datos_humedad_expandido %>%
  pivot_longer(
    cols = c(dormitorio, cocina, baño, living, otro),
    names_to = "ambiente",
    values_to = "hay_humedad"
  )

#Los ordeno para no mezclar valores:
datos_humedad_largo<-datos_humedad_largo %>%
  mutate(hay_humedad=factor(hay_humedad,levels=c(TRUE,FALSE)))


# Gráfico nro 6.
ggplot(datos_humedad_largo, aes(x = ambiente, fill = hay_humedad)) +
  geom_bar(position = "fill") +
  labs(
    title = "Distribución de problemas de humedad por ambiente",
    x = "Ambiente de la vivienda",
    y = "Proporción de viviendas con humedad",
    fill="¿Hay humedad?",
    tag="Gráfico 6.",
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c("TRUE" = "#63B8FF", "FALSE" = "#CD96CD"), # Cambia los colores
    labels = c("Sí", "No") # Cambia el texto de la leyenda
  )+
  geom_text(
    stat = "count",
    aes(label = scales::percent(..count../tapply(..count.., ..x.., sum)[..x..], accuracy = 1)), #porcentajes.
    position = position_fill(vjust = 0.5),
    size = 3
  )+
  theme_minimal()


