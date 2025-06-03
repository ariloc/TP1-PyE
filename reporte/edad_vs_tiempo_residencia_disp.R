library(here)
source(here("datos","leer-datos-lp.R"))

library(ggplot2)

ggplot(datos) +
  aes(x = edad_jefe_hogar, y = tiempo_residencia) +
  geom_jitter(width = 0.3, height = 0.3, alpha = 0.75, color = "#252850", size = 2) +
  #geom_smooth(method = "loess", se = TRUE, color = "darkred", fill = "red", alpha = 0.1) + #LM: recta de correlación lineal, LOESS: tendencia no lineal.
  labs(x = "Edad del jefe/a del hogar (años)", 
       y = "Tiempo de residencia (años)",
       caption = "Fuente: Relevamiento de Condiciones Habitacionales 2022, La Poderosa",
       title = "Relación entre la edad del jefe/a del hogar y el tiempo de residencia\n en los barrios relevados")+
  coord_fixed(ratio = 1, xlim = c(0, 80), ylim = c(0, 80)) +
  theme_classic() +
  theme(
    plot.tag.position = "bottom",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, vjust=2), 
    plot.caption = element_text(size=8, hjust=0),
    axis.text.x = element_text(angle = 45, hjust = 1),
    
    legend.background = element_rect(color = "black", linewidth  = 0.5), 
    legend.spacing = unit(0.5, "cm"),
    legend.margin = margin(10, 10, 10, 10)
  )