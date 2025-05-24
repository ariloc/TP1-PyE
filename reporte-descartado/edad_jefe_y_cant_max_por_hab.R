library(here)
source(here("datos","leer-datos-lp.R"))

library(ggplot2)

ggplot(datos) +
  aes(x = edad_jefe_hogar, y = personas_dormitorio) +
  geom_point() +
  labs(x = "Edad del jefe del hogar (años)", y = "Cantidad máxima de personas\n por habitación")+
  ggtitle("Relación entre la edad del jefe del hogar\n y la cantidad máxima de personas por habitación") +
  theme_classic()
