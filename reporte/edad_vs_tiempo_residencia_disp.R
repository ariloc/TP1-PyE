library(here)
source(here("datos","leer-datos-lp.R"))

library(ggplot2)

ggplot(datos) +
  aes(x = edad_jefe_hogar, y = tiempo_residencia) +
  geom_jitter(width = 0.3, height = 0.3, alpha = 0.75, color = "#252850", size = 2) +
  #geom_smooth(method = "loess", se = TRUE, color = "darkred", fill = "red", alpha = 0.1) +
  labs(x = "Edad del jefe del hogar (años)", 
       y = "Tiempo de residencia (años)",
       caption = "Fuente: Relevamiento de Condiciones Habitacionles 2022, La Poderosa",
       title = "Relación entre la edad del jefe del hogar y el tiempo de residencia")+
  coord_fixed(ratio = 1, xlim = c(0, 80), ylim = c(0, 80)) +
  theme_classic()

