library(here)
source(here("datos","leer-datos-lp.R"))

library(tidyverse)

# Categorizar viviendas según condición de propiedad
datos_limpios <- datos %>%
  mutate(
    propio = case_when(propiedad %in% c("Propio con algún comprobante de tenencia","Propio sin títulos")~"Propia",
                       TRUE ~ "No propia"),
  )

# Seleccionar columnas necesarias para la gráfica
datos_jefe_propiedad <- datos_limpios %>%
  select(
    edad_jefe_hogar, propio
  )

# Análisis bivariado mediante boxplots comparativos
grafico <- ggplot(datos_jefe_propiedad) +
  aes(x = propio, y = edad_jefe_hogar) +
  geom_boxplot(fill = "indianred2") +
  labs(
    x = "Condición de propiedad", 
    y = "Edad del jefe/a del hogar",
    title = "Distribución de la edad del jefe/a del hogar según condición de propiedad en los barrios relevados",
    caption = "Fuente: Relevamiento de Condiciones Habitacionales 2022, La Poderosa"
  ) +
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  theme_classic() +
  theme(
    plot.tag.position = "bottom",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5,vjust=-2), 
    plot.caption = element_text(size=8, hjust=0),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
grafico

# Resumen estadístico de la relación entre la edad del jefe/a del hogar y la condición de propiedad.
datos_jefe_resumen <- datos_jefe_propiedad %>%
  group_by(propio) %>%
  summarise(
    cantidad = n(),
    media = mean(edad_jefe_hogar, na.rm = TRUE),
    min = min(edad_jefe_hogar, na.rm = TRUE),
    Q1 = quantile(edad_jefe_hogar, 0.25, na.rm = TRUE),
    mediana = median(edad_jefe_hogar, na.rm = TRUE),
    Q3 = quantile(edad_jefe_hogar, 0.75, na.rm = TRUE),
    max = max(edad_jefe_hogar, na.rm = TRUE),
    RI = Q3-Q1,
    `Desvío estándar` = sd(edad_jefe_hogar, na.rm = TRUE)
  )

# Contar menores en no propias
datos_jefe_propiedad %>%
 filter(propio == "No propia" & edad_jefe_hogar < 18)

## Marcar ciertas regiones en la gráfica
# Mediana
dat <- ggplot_build(grafico)$data[[1]]
grafico_med <- grafico +
  geom_segment(data = dat, aes(x = xmin, xend = xmax, y = middle, yend = middle), colour = c("red","blue"), size = 1)
grafico_med

# Mediana + RI
grafico_med_ri <- grafico_med +
  annotate("rect", xmin = 0.55, xmax = 1.45, ymin = as.numeric(datos_jefe_resumen[1,"Q1"]) - 2, ymax = as.numeric(datos_jefe_resumen[1,"Q3"]) + 2,
           alpha = 0.2, fill = "#FFD700") +
  annotate("rect", xmin = 1.55, xmax = 2.45, ymin = as.numeric(datos_jefe_resumen[2,"Q1"]) - 2, ymax = as.numeric(datos_jefe_resumen[2,"Q3"]) + 2,
           alpha = 0.2, fill = "#FFD700")
grafico_med_ri

