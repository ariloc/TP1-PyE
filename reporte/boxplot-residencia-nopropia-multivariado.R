library(here)
source(here("datos","leer-datos-lp.R"))

library(tidyverse)

# Categorizar viviendas según condición de propiedad
datos_limpios <- datos %>%
  mutate(
    propio = case_when(propiedad %in% c("Propio con algún comprobante de tenencia","Propio sin títulos")~"Propia",
                       TRUE ~ "No propia"),
  )

# Filtrar las viviendas con condición de propiedad "no propia", y seleccionar las columnas necesarias para la gráfica
datos_tiempo_residencia <- datos_limpios %>%
  filter(
    propio == "No propia"
  ) %>%
  select(
    tiempo_residencia, propiedad
  )

# A pesar de tratarse de una variable categórica *nominal*, establecemos este orden de forma que la categoría "Otro" quede al final en el orden,
# a fin de que sea más sencillo comparar entre el resto de categorías más relevantes.
datos_tiempo_residencia <- datos_tiempo_residencia %>%
  mutate(
    propiedad = factor(propiedad, levels=c("Alquilado", "Ocupado/Tomado", "Prestado", "Otro"))
  )

# Análisis multivariado mediante boxplots comparativos
grafico <- ggplot(datos_tiempo_residencia) +
  aes(x = propiedad, y = tiempo_residencia) +
  geom_boxplot(fill = "darkseagreen3") +
  labs(
    x = "Situación dominial (no propia)",
    y = "Tiempo de residencia (en años)",
    title = "Distribución del tiempo de residencia según situación dominial en viviendas con tenencia no propia\npara los barrios relevados",
    caption = "Fuente: Relevamiento de Condiciones Habitacionales 2022, La Poderosa"
  ) +
  scale_y_continuous(
    breaks = seq(0, 60, 5),
    minor_breaks = seq(0, 60, 2.5)
  ) +
  theme_classic() +
  theme(
    # panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3), # opcional
    # panel.grid.major.y = element_line(color = "grey80"),                  # opcional
    plot.tag.position = "bottom",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, vjust = -2, margin = margin(b = 20)), 
    plot.caption = element_text(size=8, hjust=0),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
grafico

# Resumen estadístico del tiempo de residencia según la condición dominial en viviendas con tenencia no propia.
datos_tiempo_res_resumen <- datos_tiempo_residencia %>% 
  group_by(propiedad) %>% 
  summarise(
    cantidad = n(),
    media = mean(tiempo_residencia, na.rm = TRUE),
    min = min(tiempo_residencia, na.rm = TRUE),
    Q1 = quantile(tiempo_residencia, 0.25, na.rm = TRUE),
    mediana = median(tiempo_residencia, na.rm = TRUE),
    Q3 = quantile(tiempo_residencia, 0.75, na.rm = TRUE),
    max = max(tiempo_residencia, na.rm = TRUE),
    RI = Q3 - Q1,
    `Desvío estándar` = sd(tiempo_residencia, na.rm = TRUE)
  )

## Marcar ciertas regiones en el gráfico
# Alquilado q3
resumen_alquilado <- datos_tiempo_res_resumen[datos_tiempo_res_resumen$propiedad == "Alquilado",]
grafico_alq_q3 <- grafico +
  annotate("rect", xmin = 0.55, xmax = 1.45, ymin = as.numeric(resumen_alquilado[,"min"]), ymax = as.numeric(resumen_alquilado[,"Q3"]) + 0.5,
           alpha = 0.2, fill = "#FF6347")
grafico_alq_q3

# Alquilado q3 + mediana resto
dat <- ggplot_build(grafico)$data[[1]]
grafico_alq_q3_med <- grafico_alq_q3 +
  geom_segment(data = dat, aes(x = xmin, xend = xmax, y = middle, yend = middle), colour = c("transparent", "blue", "blue", "blue"), size = 1)
grafico_alq_q3_med
