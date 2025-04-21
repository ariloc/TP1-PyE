# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("googledrive")
# install.packages("readxl")

# Descargo el archivo mediante su id de google drive
# El link de los archivos de drive tiene esta forma:
# https://docs.google.com/spreadsheets/d/16_zhdrZIW72I45SHIsVkGv-KYQw1oeup
# El id de esta hoja de cálculo es "16_zhdrZIW72I45SHIsVkGv-KYQw1oeup"
googledrive::drive_download(as_id("16_zhdrZIW72I45SHIsVkGv-KYQw1oeup"), 
														overwrite = T)

# Cargo el archivo como .xlsx
datos <- readxl::read_excel("arbol.xlsx", 
														col_names = FALSE, 
														skip = 3)

# Veo la estructura del dataset
str(datos)
