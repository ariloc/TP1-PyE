library(here)
source(here("datos","leer-datos-lp.R"))

library(ggplot2)

ggplot(datos) +
  aes(x = edad_jefe_hogar, y = tiempo_residencia) +
  geom_point() +
  labs(x = "Edad del jefe del hogar (años)", 
       y = "Tiempo de residencia (años)",
       caption = "Fuente: Relevamiento de Condiciones Habitacionles 2022, La Poderosa",
       title = "Relación entre la edad del jefe del hogar\n y la cantidad máxima de personas por habitación")+
  theme_classic()
