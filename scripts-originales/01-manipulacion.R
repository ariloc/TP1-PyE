# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")

# Cargo los paquetes que voy a usar
library(tidyverse)

# Fijo el dataset
attach(datos)

######################
# Renombrar columnas #
######################
colnames(datos) <- c("id","altura","diametro","inclinacion","edad","tiempo",
										 "brotes","especie","follaje","origen","atracnosis",
										 "roya","manchas","ampollas")

###################
# Modificar datos #
###################
datos_limpios <- datos %>% # Los pipelines permiten encadenar acciones
	
	mutate(   # Para crear nuevas variables y editar las ya existentes
		
		# Veo valores min y max de la variable para elegir una
		# particion en intervalos apropiada
		# min(altura)
		# max(altura)
		# sqrt(nrow(datos))
		
		# Creo una variable nueva, con la partición en intervalos de altura
		altura_int = cut(altura,
										 breaks = seq(from=0, to=50, by = 5),
										 right = F),
		
		# Modifico las columnas de la variable de respuesta múltiple
		# para dejarlas como indicadoras con valores 1 (en caso de presentar
		# el atributo) y 0 (en caso de no presentarlo)
		atracnosis = ifelse( atracnosis == "atracnosis", 1, 0 ),
		roya = ifelse( roya == "roya", 1, 0 ),
		manchas = ifelse( manchas == "manchas", 1, 0 ),
		ampollas = ifelse( ampollas == "ampollas", 1, 0),
		# Notar que los NA no entran dentro de la categoría "no presentar 
		# el atributo", por lo que requieren un tratamiento particular:
		
		atracnosis = ifelse(is.na(atracnosis), 0, 1),
		roya = ifelse(is.na(roya), 0, 1),
		manchas = ifelse(is.na(manchas), 0, 1),
		ampollas = ifelse(is.na(ampollas), 0, 1),
		# Esto solo es correcto porque teníamos dos valores posibles en estas
		# columnas: presencia de atributo (nombre de la plaga) y ausencia (NA).
		# En los casos en los que se presenten ambas categorías además del NA
		# correspondería trabajarlos como tres valores distintos (presencia,
		# ausencia y faltante) y su tratamiento dependerá de lo que se desee hacer
		
		# Para condiciones ifelse múltiples puedo usar la función case_when
		inclinacion_cate = case_when(inclinacion == 0 ~ "Sin inclinación",
																 inclinacion < 15 ~ "Inclinación leve",
																 inclinacion < 30 ~ "Inclinación moderada",
																 TRUE ~ "Inclinación alta"),
		
		# Recodifico las etiquetas de una variable categórica
		especie = recode(especie, "ala" = "Álamo",
										 "casu" = "Casuarina",
										 "euca" = "Eucalipto",
										 "jaca" = "Jacarandá",
										 "palo"  = "Palo borracho"),
		
		# Especifico ordinalidad a las categorías de una variable
		tiempo = factor(tiempo,
										levels = 1:5,
										labels = c("Menos de 2 años", "Entre 2 y 5 años",
																				 "Entre 5 y 10 años", "Entre 10 y 20 años",
																				 "20 años o más"))

	)

##########################################
# Seleccionar un subconjunto de columnas #
##########################################

# Opcion 1
datos_chico1 <- datos_limpios %>%
	select(   # Seleccionar las columnas que quiero conservar
		id, altura, edad, follaje, inclinacion_cate
	)

# Opcion 2
datos_chico2 <- datos_limpios %>%
	select(   # Eliminar las columnas que no quiero conservar
		-altura, -edad, -follaje, -inclinacion_cate
	)

# Opcion 3
datos_orden <- datos_limpios %>%
	select(   # Reordeno columnas
		id, especie, tiempo, everything()
	)


###########################################
# Seleccionar un subconjunto de registros #
###########################################

# Opción 1: por criterio
datos_reducido1 <-datos_orden %>%
	filter((brotes > 4 & origen == "Nativo/Autóctono") | tiempo == "20 años o más")

# Opción 2: por indexación
datos_reducido2 <-datos_orden %>%
	slice(1:500)
