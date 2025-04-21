# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("ggplot2")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(ggplot2)

# Fijo el dataset
attach(datos_limpios)

#####################
# Gráfico de barras #
#####################

datos %>%
	mutate( tiempo = factor(tiempo,
													levels = 1:5,
													labels = c("Menos de 2 años", "Entre 2 y 5 años",
																		 "Entre 5 y 10 años", "Entre 10 y 20 años",
																		 "20 años o más"))) %>%
	ggplot() + 
	
	#aes(x = tiempo) + # Frecuencias absolutas
	aes(x = reorder(tiempo, tiempo, function(x) -length(x))) + # Ordenar según frecuencia
	#aes(x = tiempo, y = ..count.. / sum(..count..)) + # Porcentajes
	# aes(x = reorder(tiempo, tiempo, function(x) -length(x)), 
	#		y = ..count.. / sum(..count..)) +  # Porcentajes ordenados según frecuencia
	#scale_y_continuous(labels = scales::percent) +    # Eje para porcentajes
	
	geom_bar(width = 0.75,   # Ancho de barras
					 fill = '#7ed021',  # Color de relleno 
					 col = "black",  # Color de línea
					 alpha = 0.6) +  # Transparencia
	
	labs(y = "Cantidad de árboles", x = "Tiempo desde la plantación") + # Nombres de ejes
	
	ggtitle("Antigüedad de plantación de los árboles") +
	
	coord_flip() + # Barras horizontales o verticales

	theme_classic() # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/
	
	

###########################################
# Gráfico de barras a partir de una tabla #
###########################################

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
	pivot_longer(cols = c(atracnosis, roya, manchas, ampollas, ninguna),
							 names_to = "plaga",
							 values_to = "cant") %>%

		ggplot(aes(x = plaga,
						 y = cant)) + 
	geom_bar(stat = "identity", # Argumento necesario si partimo de una tabla
					 width = 0.75) +
	labs(y = "Cantidad de árboles", x = "Presencia de plagas") +
	ggtitle("Antigüedad de plantación de los árboles") +
	coord_flip() +
	theme_classic() # Temas preconfigurados de R https://r-charts.com/ggplot2/themes/



#######################
# Gráfico de bastones #
#######################

ggplot(datos) +
	aes(x = brotes) + 
	geom_bar(width = 0.10) +
	scale_x_continuous() +
	labs(y = "Cantidad de árboles", 
			 x = "Número de brotes nuevos")+
	theme_classic()




###########
# Boxplot #
###########

datos_limpios %>% 
	
	# Puedo filtrar en el mismo paso que construyo el gráfico
	filter(especie == "Eucalipto") %>%
	
	ggplot() +
	aes(x = diametro, y = "") +
	geom_boxplot(width = 0.75, fill = "lightgray", outlier.size = 1) +
	theme(axis.ticks.y = element_blank()) +
	labs(y = "", x = "Diámetro (cm)") +
	scale_x_continuous(breaks = seq(0, 250, 50))




##############
# Histograma #
##############

# Frecuencias absolutas
ggplot(datos_limpios) +
	aes(x = diametro) +
	geom_histogram(fill = "lightgray", col = "black", 
								 breaks = seq(0, 250, 20)) +
	scale_x_continuous(breaks = seq(0, 250, 20)) +
	labs(x = "Diámetro (cm)", y = "Cantidad de árboles")

# Frecuencias relativas
ggplot(datos_limpios) +
	aes(x = diametro, y = ..count../sum(..count..)) +
	geom_histogram(fill = "lightgray", col = "black", 
								 breaks = seq(0, 250, 20)) +
	scale_x_continuous(breaks = seq(0, 250, 20)) +
	scale_y_continuous(labels = scales::percent) +
	labs(x = "Diámetro (cm)", y = "Cantidad de árboles")



##############
# Densidades #
##############

ggplot(datos_limpios) +
	aes(x = altura) +
	stat_density(bw = 3, # Nivel de suavizado
							         # El nivel por defecto puede conocerse con bw.nrd0(datos_limpios$altura)
							 fill = "lightgray", col = "black") +
	labs(x = "Altura (m)", y = "Densidad")
