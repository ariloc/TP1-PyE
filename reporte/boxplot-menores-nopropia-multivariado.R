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
datos_menores_nopropio <- datos_limpios %>%
  filter(
    propio == "No propia"
  ) %>%
  select(
    menores_edad, propiedad
  ) 
  
# A pesar de tratarse de una variable categórica *nominal*, establecemos este orden de forma que la categoría "Otro" quede al final en el orden,
# a fin de que sea más sencillo comparar entre el resto de categorías más relevantes.
datos_menores_nopropio <- datos_menores_nopropio %>%
  mutate(
    propiedad = factor(propiedad, levels=c("Alquilado", "Ocupado/Tomado", "Prestado", "Otro"))
  )

# Análisis multivariado mediante boxplots comparativos
ggplot(datos_menores_nopropio) +
  aes(x = propiedad, y = menores_edad) +
  geom_boxplot(fill = "orchid3") +
  labs(
    x = "Situación dominial (no propia)",
    y = "Cantidad de menores en el hogar",
    title = "Distribución de cantidad de menores según situación dominial en viviendas con tenencia no propia\npara los barrios relevados",
    caption = "Fuente: Relevamiento de Condiciones Habitacionales 2022, La Poderosa"
  ) +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  theme_classic() +
  theme(
    plot.tag.position = "bottom",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5,vjust=-2), 
    plot.caption = element_text(size=8, hjust=0),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Resumen estadístico de la cantidad de menores en el hogar.
datos_menores_nopropio %>%
  group_by(propiedad) %>%
  summarise(
    cantidad= n(),
    media = mean(menores_edad, na.rm = TRUE),
    min = min(menores_edad, na.rm = TRUE),
    Q1 = quantile(menores_edad, 0.25, na.rm = TRUE),
    mediana = median(menores_edad, na.rm = TRUE),
    Q3 = quantile(menores_edad, 0.75, na.rm = TRUE),
    max = max(menores_edad, na.rm = TRUE),
    RI = Q3-Q1,
    `Desvío estándar` = sd(menores_edad, na.rm = TRUE),
    
  )
