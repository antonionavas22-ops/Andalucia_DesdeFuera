#PASOS PREVIOS antes de empzar los análisis

#Verificar que las turas son las correctas
getwd()
#Se confirma que es la ruta de GitHub, donde se aloja todo el progreso
#Ahora se verifica que en esa ruta están los docuementos para el análisis cuantiativo
list.files()
#Se confirma que el libro y los excel a vincular están situados correctamente

#PRIMER PASO DEL ANALISIS CUANTITATIVO
#Se carga el relato de viaje

relato <- readLines("Calvert_1908.txt", encoding = "UTF-8")
# Se confirma la creación del vector, como resultado de los argumentos pertenecientes a la función readLines
# ahora el vector creado pasa al argumento de la nueva función par un correcto análisis
libro <- paste(relato, collapse = " ")
# otro paso importante es convertir todo en minúscula, tanto el escel ya convertido, como el relato
libro_previo <- tolower(libro)
#Ahora se intalan paquetes básicos para el análisis que, los cuales, incluyen otros más, evitando instalar cada paquete de manera independiente
install.packages("tidyverse")
#Ahora se carga el paquete o librería
library("tidyverse")
#Mismo procedimiento con otro paquete que incluye el resto
install.packages ("tidytext")
library("tidytext")
#Ahora se carga el paquete bfundamental para la vinculación con el excel manual
library("readxl")
#Ahora se crea un nuevo vector, reusltado de la variable asociado al documento
localidades<- read_excel("normalizacion_localidades.xlsx")
#Se comprueba que la tabla es correcta
View(localidades)
#Ahora se procede a que analize de los 71 municpios, cuales aaprecen en el libro
resultado <- localidades %>%
  mutate(menciones = str_count(libro_previo, fixed(english))) %>%
  filter(menciones > 0) %>%
  arrange(desc(menciones))
View(resultado)
#Para descargar el documento generado, se descarga un nuevo paquete
install.packages("writexl")
library("writexl")
# A continuación se ejecuta el final del PRIMER ANALISIS CUANTIATIVO.
write_xlsx(resultado, "resultado_localidades_Calvert.xlsx")
#A continuación, todo este proceso, junto al excel generado se exporta a GitHub