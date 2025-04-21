# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("ggplot2")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(ggplot2)

# Fijo el dataset
attach(datos_limpios)

##########
# Barras #
##########

datos_limpios %>% 
	ggplot() + 
	aes(x = especie, fill = follaje) +
	labs(x = "Especie", 
	     y = "Cantidad de árboles", 
			 fill = "Tipo de Follaje") +
	geom_bar(position = "fill") # position = fill, dodge, stack 


# Caso particuar: variable de opción múltiple
# Variable de respuesta múltiple
datos_limpios %>% group_by(origen) %>%
  
  mutate(
    alguna = atracnosis + roya + manchas + ampollas,
    ninguna = ifelse(alguna == 0, 1, 0)
  ) %>%
  
  summarize(Atracnosis = sum(atracnosis),
            Roya = sum(roya),
            Manchas = sum(manchas),
            Ampollas = sum(ampollas),
            Ninguna = sum(ninguna)) %>%
  
  # Paso la tabla de formato horizontal a vertical
  pivot_longer(cols = c(Atracnosis, Roya, Manchas, Ampollas, Ninguna),
               names_to = "plaga",
               values_to = "cant") %>% 
  
  # Grafico los datos
  ggplot() + 
  aes(x = plaga, y = cant,  fill = origen) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Plaga", 
       y = "Cantidad de árboles", 
       fill = "Origen") +
  geom_bar(stat = "identity",
           position = "fill") +
  ggtitle("Origen de los árboles según presencia de plagas") +
  coord_flip() +
  theme_minimal()


##########################
# Diagrama de dispersión #
##########################

ggplot(datos_limpios) +
  aes(x = diametro, y = altura) +
  geom_point() +
  labs(x = "Diámetro (cm)", y = "Altura (m)")+
  ggtitle("Relación entre el diámetro y la altura de los árboles") +
  theme_classic()

# En algunas situaciones se puede recurrir a algunas estrategias que 
# favorecen la visualización:
## Seleccionar al azar un subconjunto de datos para realizar el gráfico si
# se tienen muchísimos datos
## Colorear con transparencia.
## Agregar una perturbación aleatoria que desparrame puntos superpuestos,
# lo cual es particularmente útil cuando una de las variables es discreta
#o por redondeo se tienen muchos valores iguales. Esta técnica se conoce 
# como jittering. En el siguiente ejemplo se la aplica sobre la variable 
# 

ggplot(datos_limpios) +
  aes(x = edad, y = brotes) +
  geom_point(color = "red") + # Color de los puntos originales
  labs(x = "Diámetro (cm)", y = "Altura (m)")+
  geom_jitter(width = 0, # Jitter horizontal
              height = 0.5, # Jitter vertical
              alpha = 0.4, # Transparencia
              color = "red") + # Color de los puntos jitter
  ggtitle("Relación entre el diámetro y la altura de los árboles") +
  theme_bw()



########################
# Boxplot comparativos #
########################

ggplot(datos_limpios) +
  aes(x = especie, y = altura) +
  geom_boxplot(show.legend = F, fill = "lightblue") +
  labs(x = "Especie", y = "Altura (m)") +
  coord_flip() +
  ggtitle("Distribución de la altura de los árboles según especie") +
  theme_light()

# Caso particuar: variable categórica de opción múltiple
# Como la variable no es una partición del total de individuos entonces
# no podemos usar facets ni groups ni nada por el estilo.
# Hacemos los gráficos de a uno y usamos la librería gridExtra para 
# acomodarlos en una grilla.

library(gridExtra)

# Vemos hasta dónde extender los ejes así todos los gráficos tienen los 
# mismos límites y no se distorsiona la percepción
min(altura)
max(altura)

# Creamos los gráficos de a uno y los guardamos en objetos (g1, ..., g5)
g1 <- datos_limpios %>% filter(especie == "Álamo") %>%
  ggplot() +
  aes(x = especie, y = altura) +
  geom_boxplot(show.legend = F, fill = "orange") +
  scale_y_continuous(limits = c(0,50)) + # Fijo límites para el eje continuo
  theme_minimal() +
  labs(x = "", y = "Altura (m)") # Le dejo el nombre del eje solo al primero
g2 <- datos_limpios %>% filter(especie == "Casuarina") %>%
  ggplot() +
  aes(x = especie, y = altura) +
  geom_boxplot(show.legend = F, fill = "orange") +
  scale_y_continuous(limits = c(0,50), ) + 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 0)) + # Saco etiquetas de ticks de eje continuo
  labs(x = "", y = "") # Dejo ambos nombres de ejes vacíos para no cargar innecesariamente la figura
g3 <- datos_limpios %>% filter(especie == "Eucalipto") %>%
  ggplot() +
  aes(x = especie, y = altura) +
  geom_boxplot(show.legend = F, fill = "orange") +
  scale_y_continuous(limits = c(0,50)) + 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 0)) +
  labs(x = "", y = "")
g4 <- datos_limpios %>% filter(especie == "Jacarandá") %>%
  ggplot() +
  aes(x = especie, y = altura) +
  geom_boxplot(show.legend = F, fill = "orange") +
  scale_y_continuous(limits = c(0,50)) + 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 0)) +
  labs(x = "", y = "")
g5 <- datos_limpios %>% filter(especie == "Palo borracho") %>%
  ggplot() +
  aes(x = especie, y = altura) +
  geom_boxplot(show.legend = F, fill = "orange") +
  scale_y_continuous(limits = c(0,50)) + 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 0)) +
  labs(x = "", y = "")

# Con la función grid arrange armo la grilla para "imprimir"
grid.arrange(g1, g2, g3, g4, g5, # Qué objetos vamos a mostrar
             ncol=5, nrow =1) # Cant de columnas y filas de la grilla
             

#####################################################
# OTROS GRÁFICOS BIVARIADOS

####################
# Gráfico de línea #
####################

# Se utilizan para graficar la evolución de un indicador o variable a lo largo
# del tiempo. El tiempo se grafica siempre en el eje horizontal.

# A partir de la edad del árbol creamos la variable año_nacimiento.
datos_limpios %>% 
  mutate(año_nacimiento = 2011-edad) %>%
  group_by(año_nacimiento) %>%
  summarize(cant = n()) %>%
  ggplot() + 
  aes(x = año_nacimiento, y = cant) +
  geom_line(color = "#00AFBB",
            size = 1) +
  labs(x = "Año de nacimiento", y = "Cantidad de árboles") +
  ggtitle("Cantidad de árboles según el año de nacimiento") +
  theme_light()


# Y podría verse la evolución de varias series simultáneamente
datos_limpios %>% 
  mutate(año_nacimiento = 2011-edad) %>%
  group_by(año_nacimiento, origen) %>%
  summarize(cant = n()) %>%
  ggplot() + 
  aes(x = año_nacimiento, y = cant, group = origen, color = origen) +
  geom_line(size = 1) +
  labs(x = "Año de nacimiento", y = "Cantidad de árboles",
       color = "Origen") +
  ggtitle("Cantidad de árboles según el año de nacimiento y el origen") +
  theme_test()


#########################
# Gráfico de densidades #
#########################

ggplot(datos_limpios) +
  aes(x = altura, color = origen, fill = origen) +
  geom_density(alpha = 0.4) +
  labs(x = "Altura (m)", y = "Densidad",
       color = "Origen",
       fill = "Origen") 

# Una variante de los gráficos de densidades son los ridgeline plots 
# (por ridgeline mountains, crestas o riscos de montañas), en los cuales 
# las distintas densidades se apilan sobre el eje vertical en lugar de superponerse.
# Suelen ser particularmente útiles para mostrar tendencias en el tiempo o 
# ante la presencia de muchas distribuciones para comparar.
# Dado que el eje horizontal presenta la variable continua y el vertical la 
# variable de agrupación, no hay un eje apartado para la densidad. El propósito
# del gráfico no es mostrar explícitamente los valores de densidad estimados 
# sino permitir una fácil comparación de la forma y alturas de las densidades
# entre grupos.
library(ggridges)
ggplot(datos_limpios) +
  aes(x = edad, y = especie, fill = especie) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none") +
  labs(x = "Altura (m)",
       y = "Especie") 
