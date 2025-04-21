# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("janitor")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(janitor)

# Fijo el dataset
attach(datos_limpios)

###########################
# Tablas usando tidyverse #
###########################

# Frecuencias
datos_limpios %>% group_by(tiempo) %>%
	summarize(cant = n())

# Medidas resumen por grupo
datos_limpios %>% group_by(especie) %>%
	summarize(altura_media = mean(altura),
						altura_ds = sd(altura))

# Variable de respuesta múltiple
datos_limpios %>%
	
	mutate(
		alguna = atracnosis + roya + manchas + ampollas,
		ninguna = ifelse(alguna == 0, 1, 0)
	) %>%
	
	summarize(atracnosis = sum(atracnosis),
						roya = sum(roya),
						manchas = sum(manchas),
						ampollas = sum(ampollas),
						ninguna = sum(ninguna)) %>%
		
	# Paso la tabla de formato horizontal a vertical
	pivot_longer(cols = c(atracnosis, roya, manchas, ampollas, ninguna),
							 names_to = "plaga",
							 values_to = "cant") %>%
	
	# Agrego columna con porcentajes
	mutate(
		porc = paste0(round( cant / nrow(datos_limpios) * 100, 2),"%")
	) %>%
	
	# Ordeno por frecuencia del fenómeno
	arrange(desc(cant))


#########################
# Tablas usando janitor #
#########################

# Tabla de distribución de frecuencias
tabla <- tabyl(especie)

# Adorns
tabla %>% 
	rename(   # Renombro columnas
		"Especie" = especie,
		"Cant. árboles" = n,
		"% árboles" = percent
	) %>% 
	adorn_totals() %>%  # Agrego fila de totales
	adorn_pct_formatting(digits = 1) # Cant. de decimales en %
	
	
# Tabla de contingencia
tabyl(datos, origen, follaje) %>%
	adorn_totals(where = c("row", "col")) %>%
	adorn_percentages(denominator = "row") %>% # Distribuciones condicionales
	adorn_pct_formatting(digits = 1) %>%
	adorn_title(placement = "top", "Origen", "Tipo de follaje")


# Variable cuantitativa
tabyl(altura_int) %>%
	mutate(
		"Cant. acumulada" = cumsum(n),
		"% acumulado" = cumsum(percent)
	) %>% 
	rename(
		"Altura" = altura_int,
		"Cant. árboles" = n, 
		"% árboles" = percent
	)
