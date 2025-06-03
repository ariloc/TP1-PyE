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
datos_tiempo_residencia <- datos_limpios %>%
  select(
    tiempo_residencia, propio
  )

datos_grafico <- datos_tiempo_residencia

# Eliminar valor atípico propia (opcional)
# datos_grafico <- datos_tiempo_residencia %>%
#   filter(tiempo_residencia < 100)

# Análisis bivariado mediante boxplots comparativos
grafico <- ggplot(datos_grafico) +
  aes(x = propio, y = tiempo_residencia) +
  geom_boxplot(fill = "lavender") +
  labs(
    x = "Condición de propiedad",
    y = "Tiempo de residencia (en años)",
    title = "Distribución del tiempo de residencia en la vivienda según condición de propiedad\nen los barrios relevados",
    # caption = paste0("NOTA: Se omite en la gráfica un valor atípico de 111 años en condición de tenencia propia para facilitar la visualización de los datos.\n",
    #           "Fuente: Relevamiento de Condiciones Habitacionales 2022, La Poderosa"),           # Agregar nota si se elimina valor atípico (opcional)
    caption = "Fuente: Relevamiento de Condiciones Habitacionales 2022, La Poderosa"
  ) +
  scale_y_continuous(
    breaks = seq(0, 150, 5),
    minor_breaks = seq(0, 150, 2.5)
  ) +
  theme_classic() +
  theme(
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey80"),
    plot.tag.position = "bottom",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, vjust = -2, margin = margin(b = 20)), 
    plot.caption = element_text(size=8, hjust=0),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
grafico

# Resumen estadístico del tiempo de residencia según la condición de propiedad,.
datos_tiempo_res_resumen <- datos_tiempo_residencia %>% 
  group_by(propio) %>% 
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
# q3
grafico_q3 <- grafico +
  annotate("rect", xmin = 0.55, xmax = 1.45, ymin = as.numeric(datos_tiempo_res_resumen[1,"min"]), ymax = as.numeric(datos_tiempo_res_resumen[1,"Q3"]) + 1,
           alpha = 0.2, fill = "#FF6347") +
  annotate("rect", xmin = 1.55, xmax = 2.45, ymin = as.numeric(datos_tiempo_res_resumen[2,"min"]), ymax = as.numeric(datos_tiempo_res_resumen[2,"Q3"]) + 1,
           alpha = 0.2, fill = "#87CEEB")
grafico_q3

