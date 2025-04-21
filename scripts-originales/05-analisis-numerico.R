library(tidyselect)
library(janitor)

# Estructura del conjunto de datos
str(datos_limpios)

# Paqueta de medidas resumen 
summary(datos_limpios)
summary(datos_limpios[,c(2,3,4)])

# Funciones para obtener medidas
attach(datos_limpios)

# Posición: tendencia central
mean(altura) # Mediana
median(altura) # Media aritmética

# Posición: otras
min(altura) 
max(altura)
quantile(altura) # 5 medidas resumen
quantile(altura, 0.9) # Otros percentiles
sort(table(especie), decreasing = TRUE)[1] # Moda

# Dispersión
range(altura) # Valores mín y max
max(altura) - min(altura) # Rango
sd(altura) # Desvío estándar
var(altura) # Variancia
IQR(altura) # Rango intercuartílico
round(sd(altura)/mean(altura)*100,1) # Coeficiente de variación

# Otras medidas
var(altura,diametro) # Covariancia
cor(altura,diametro) # Correlación

# Medidas por grupos
datos_limpios %>% group_by(especie) %>%
  summarise(Promedio = median(altura),
            Desv.Est. = IQR(altura),
            Mínimo = min(altura),
            Máximo = max(altura))

# Distribuciones condicionales
tabyl(datos_limpios, tiempo, follaje) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_title(placement = "top", "Origen", "Tipo de follaje")
