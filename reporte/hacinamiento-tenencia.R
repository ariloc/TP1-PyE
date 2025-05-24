library(here)
source(here("datos","leer-datos-lp.R"))

library(tidyverse)

# Analizamos la frecuencia de la cantidad de personas por habitación, dividiendo la muestra
# en 2 categorías: "tenencia propia" y "tenencia no propia".

#Factorizamos los datos en "Propia" y "No propia", según criterio dado anteriormente [[ VER NOTA]].
datos_refactorizados<- datos %>%
  mutate(propiedad_reclasificada=case_when(propiedad %in% c("Propio con algún comprobante de tenencia","Propio sin títulos")~"Propia",
                                           TRUE ~ "No propia"))

#Factorizamos datos según tipo de tenencia:
datos_no_propios <- datos_refactorizados %>% 
  filter(propiedad_reclasificada == "No propia")

datos_propios <- datos_refactorizados %>% 
  filter(propiedad_reclasificada == "Propia")

# Gráfico de  viviendas no propias
ggplot(datos_no_propios, aes(x = as.factor(personas_dormitorio))) +
  geom_bar(fill = "salmon", color = "black") +
  labs(
    title = "Cantidad de personas por habitación, en viviendas sin tenencia propia",
    x = "Cantidad de personas por habitación",
    y = "Frecuencia",
    caption = "Fuente: Relevamiento de Condiciones Habitacionles 2022, La Poderosa",
  ) +
  theme(
    plot.caption = element_text(size=8, hjust=0),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5,vjust=-2), 
  )+
  theme_classic()

# Gráfico de viviendas propias
ggplot(datos_propios, aes(x = as.factor(personas_dormitorio))) +
  geom_bar(fill = "#00E5EE", color = "black") +
  labs(
    title = "Cantidad de personas por habitación, en viviendas con tenencia propia",
    x = "Cantidad de personas por habitación",
    y = "Frecuencia",
    caption = "Fuente: Relevamiento de Condiciones Habitacionles 2022, La Poderosa",
  ) + 
  theme_classic()+
theme(
  plot.caption = element_text(size=8, hjust=0),
  plot.title = element_text(size = 12, face = "bold", hjust = 0.5,vjust=-2), 
)



# Analizemos las tendencias centrales y dispersión:

mediana_no_propia <- median(datos_no_propios$personas_dormitorio, na.rm = TRUE)
q1_no_propia <- quantile(datos_no_propios$personas_dormitorio, 0.25, na.rm = TRUE)
q3_no_propia <- quantile(datos_no_propios$personas_dormitorio, 0.75, na.rm = TRUE)
RI_no_propia <- q3_no_propia - q1_no_propia

mediana_propia <- median(datos_propios$personas_dormitorio, na.rm = TRUE)
q1_propia <- quantile(datos_propios$personas_dormitorio, 0.25, na.rm = TRUE)
q3_propia <- quantile(datos_propios$personas_dormitorio, 0.75, na.rm = TRUE)
RI_propia <- q3_propia - q1_propia


cat("Vivienda No propia:"," Mediana:", mediana_no_propia," RI:", RI_no_propia)

cat("Vivienda Propia:"," Mediana:", mediana_propia," RI:", RI_propia)

