library(here)
source(here("datos","leer-datos-lp.R"))

library(tidyverse)

# Analizamos la frecuencia de la cantidad de personas por habitación, dividiendo la muestra
# en 2 categorías: "tenencia propia" y "tenencia no propia".

datos_refactorizados<- datos %>%
  mutate(propiedad_reclasificada=case_when(propiedad %in% c("Propio con algún comprobante de tenencia","Propio sin títulos")~"Propia",
                                           TRUE ~ "No propia"))
datos_no_propios <- datos_refactorizados %>% 
  filter(propiedad_reclasificada == "No propia")

# Gráfico
ggplot(datos_no_propios, aes(x = personas_dormitorio)) +
  geom_histogram(binwidth = 0.5, fill = "salmon", color = "black") +
  labs(title = "Personas por habitación (Tenencia no propia)",
       x = "Cantidad de personas por habitación",
       y = "Frecuencia",
       tag="Gráfico 3.") +
  theme_minimal()

datos_propios <- datos_refactorizados %>% 
  filter(propiedad_reclasificada == "Propia")

# Gráfico
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

