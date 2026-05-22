# CCOMPROBACIONES PREVIAS

#PASO UNO: Ver la ruta donde está el programa
getwd()
#a acontinuación, ver si están los documentos csv que se van a usar de referencia
list.files()
#Una vez asgurado todo, se comienza el preprocesmiento y adaptación del texto a las necesidades de R y de los archivos csv

#PASO DOS: cargar el relato principal que se va a vincular los csv
relato <- readLines("1910_buck_chapman_unexplored_spain.txt" , encoding = "UTF-8")

#PASO TRES: cambio de estructura del relato para trabajar con R
libro <- paste(relato, collapse = " ")

#PASO CUATRO: todo a minúscula para trabajar con R
#OJO, no se hace con tidytext, porque no nos interesa para la lista csv que queremos cruzar, pues son palabras compuestas
# Y el paquete tidytexto incluye el tokenizador y separaría las palabras compuestas
#por tanto, se usa la función independiente de tolower
libro_previo <- tolower(libro) 


#PASO CINCO: se instala el ecosistema tidyverse que incluye los paquetes y sus funciones para las operaciones
install.packages("tidyverse")
#Una vez instalado, se carga
library("tidyverse") 

#se ejecutan las funciones sin necesidad de instalar y cargar paquetes del ecositema tidyverse: readr, stringr, entre otros, pues ya está instalado e incluido en tidyverse
#Antes de las operaciones directas sobre los csv de referencia y su vínculo al texto, con el paquete stringr, se hace un último preprocesamiento.
#Con esta función, permite reconocer con mayor precisión, palabras del texto y vincularlas al documento csv creado. Esto es, se sustituye valors como comillas y guinones bajos, usados para muchas palabras castellanas. 
libro_para_contar <- str_replace_all(libro_previo, "[_\"]", " ")

#PASO SEIS: SE CARGAN LOS TRES CSV (LOCALIIDADES, PATRIMONIO MATERIAL E INMATERIAL)
#Se puede caragr también al inicio, ya que los pasos anteriores se aplican al texto, sina afectar al csv
#No obstante los tres csv, ya están en minúscula para poder coincidir en resultados y ser detectados en el texto.


localidades <- read_csv("localidades_normalizacion.csv")
View(localidades)
patrimonio_material <- read_csv("patrimonio_material_normalizacion.csv")
View(patrimonio_material)
patrimonio_inmaterial <- read_csv("patrimonio_inmaterial_normalizacion.csv")
View(patrimonio_inmaterial) 

#PASO SIETE: SE EJECUTAN LAS OPERACIONES CON EL CSV VINCULADO A LAS 3 GRANDES CATEGORÍAS
#se usa las mismas funciones de los paquetes de tidyverse, como dplyr, magrittr, readr o stringr


#  A.LOCALIDADES Y GENTILICIOS

resultado <- localidades %>%
  mutate(menciones = str_count(libro_para_contar, paste0("\\b", variantes, "\\b"))) %>%
  filter(menciones > 0) %>%
  arrange(desc(menciones))

# lo visualizamos en la tabla y aplicamos sumatorio para la ocurrencia de las palabras
View(resultado)
sum(resultado$menciones)

#Se especifica el número de menciones de los gentilicios para diferenciarlo de las localidades a las que se asocia

sum(resultado$menciones[grepl("gentilicio", resultado$principal)])

#mismo proceso con PATRIMONIO MATERIAL

#  B.PATRIMONIO MATERIAl


resultado_material <- patrimonio_material %>%
  mutate(menciones = str_count(libro_para_contar, paste0("\\b", variantes, "\\b"))) %>%
  filter(menciones > 0) %>%
  arrange(desc(menciones))

#visualizamos el resultado
View(resultado_material)

#A continucación aplicamos para los resultados el sumatorio correspondiente, dado que son muchas palabras y menciones de diferente tipología
#Para estas operaciones no son necesarios los paquetes, sino que es suficiente con la función básica de R "sum"

sum(resultado_material$menciones)

#Sumatorio de la subcategoría de P.MATERIAL (1/5) público y sus dos apartados específicos

sum(resultado_material$menciones[grepl("público", resultado_material$principal)])
sum(resultado_material$menciones[grepl("público,propio", resultado_material$principal)])
sum(resultado_material$menciones[grepl("público,común", resultado_material$principal)])

#Sumatorio  de la subcategoría de P.MATERIAL (2/5) monumento y sus dos apartados específicos

sum(resultado_material$menciones[grepl("monumento", resultado_material$principal)])
sum(resultado_material$menciones[grepl("monumento, propio", resultado_material$principal)])
sum(resultado_material$menciones[grepl("monumento, común", resultado_material$principal)])

#Sumatorio de la subcategoría de P.MATERIAL (3/5) consumo y sus tres apartados específcios

sum(resultado_material$menciones[grepl("consumo", resultado_material$principal)])
sum(resultado_material$menciones[grepl("consumo, alimentación", resultado_material$principal)])
sum(resultado_material$menciones[grepl("consumo, establecimiento", resultado_material$principal)])
sum(resultado_material$menciones[grepl("consumo, objeto", resultado_material$principal)])

#sumatorio de la subcategoría de P.MATERIAL (4/5) espacio natural y sus dos aparatdos específicos

sum(resultado_material$menciones[grepl("espacio natural", resultado_material$principal)])
sum(resultado_material$menciones[grepl("espacio natural, propio", resultado_material$principal)])
sum(resultado_material$menciones[grepl("espacio natural, común", resultado_material$principal)])

#sumatorio de la última subactegoría de P.MATERIAL (5/5) viaje y sus dos apartados específicos

sum(resultado_material$menciones[grepl("viaje", resultado_material$principal)])
sum(resultado_material$menciones[grepl("viaje, transporte", resultado_material$principal)])
sum(resultado_material$menciones[grepl("viaje, animal", resultado_material$principal)])


#Ahora se repite el mismo proceso cuantitativo con la última gran categoría, con subcategorías y aparatdos específicos de estos.
#Despúes de la categoría localidad y pptrimonio material, queda el...

#  C.PATRIMONIO INMATERIAL

resultado_inmaterial <- patrimonio_inmaterial %>%
  mutate(menciones = str_count(libro_para_contar, paste0("\\b", variantes, "\\b"))) %>%
  filter(menciones > 0) %>%
  arrange(desc(menciones))

#visualizamos el resultado y el sumatorio general
View(resultado_inmaterial)
sum(resultado_inmaterial$menciones)

#A continuación se hacen el sumario como en P.Material, por subactergorías y aparatdos esepcíficos.

#sumatorio de la subcategoría de P. INMATERIAL (1/5) característica y sus dos apartados específcios

sum(resultado_inmaterial$menciones[grepl("característica", resultado_inmaterial$principal)])
sum(resultado_inmaterial$menciones[grepl("característica, general", resultado_inmaterial$principal)])
sum(resultado_inmaterial$menciones[grepl("característica, personalidad", resultado_inmaterial$principal)])

#sumatorio de la subcategoría de P. INMATERIAL (2/5) género y sus dos apartados específcios

sum(resultado_inmaterial$menciones[grepl("género", resultado_inmaterial$principal)])
sum(resultado_inmaterial$menciones[grepl("género, femenino", resultado_inmaterial$principal)])
sum(resultado_inmaterial$menciones[grepl("género, masculino", resultado_inmaterial$principal)])

#sumatorio de la subcategoría de P. INMATERIAL (3/5) género y sus tres apartados específcios

sum(resultado_inmaterial$menciones[grepl("oficio", resultado_inmaterial$principal)])
sum(resultado_inmaterial$menciones[grepl("oficio, campo y ciudad", resultado_inmaterial$principal)])
sum(resultado_inmaterial$menciones[grepl("oficio, toreo", resultado_inmaterial$principal)])
sum(resultado_inmaterial$menciones[grepl("oficio, seguridad", resultado_inmaterial$principal)])

#sumatorio de la subcategoría de P. INMATERIAL (4/5) grupos y sus dos apartados específcios

sum(resultado_inmaterial$menciones[grepl("grupos", resultado_inmaterial$principal)])
sum(resultado_inmaterial$menciones[grepl("grupos, interno", resultado_inmaterial$principal)])
sum(resultado_inmaterial$menciones[grepl("grupos, externo", resultado_inmaterial$principal)])


#sumatorio de la subcategoría de P. INMATERIAL (5/5) fiestas  y sus tres apartados específcios

sum(resultado_inmaterial$menciones[grepl("fiestas", resultado_inmaterial$principal)])
sum(resultado_inmaterial$menciones[grepl("fiestas, toreo", resultado_inmaterial$principal)])
sum(resultado_inmaterial$menciones[grepl("fiestas, religión", resultado_inmaterial$principal)])
sum(resultado_inmaterial$menciones[grepl("fiestas, feria y carnaval", resultado_inmaterial$principal)])

#RECORDATORIO TRAS MÚLTIPLES CÁLCULOS
#Estabamos en el PASO SIETE, donde se hacían las operaciones cuantitativas

#Ahora PASO OCHO

#exportar los resultados de los tres CSV relativos a P.MATERIAL, P. INMATERIAL y LOCALIDADES-GENTILICIOS
#Al igual que las operaciones, se exporta de forma individual, solo que ahora no se tiene en cuenta las subcategorías y apartados específicos
#Estos van incluido entre paréntesis ya en el csv original, que es lo que ha permitido hacer un sumatorio tan exhaustivo, sin necesidad de crear numerosos docuemtnos independientes

#localidades
write_csv(resultado, "resultado_localidades_Buck_Chapman_Unexplored.csv") 
#patrimonio material
write_csv(resultado_material, "resultado_material_Buck_Chapman_Unexplored.csv")
#patrimonio inmaterial
write_csv(resultado_inmaterial, "resultado_inmaterial_Buck_Chapman_Unexplored.csv") 


# PASO NUEVE: creación de gráficas conjuntas en mismo espacio, pero manteninedo cada una su integridad

resultado <- resultado %>% mutate(tipo = "localidades") 
resultado_material <- resultado_material %>% mutate(tipo = "patrimonio material")
resultado_inmaterial <- resultado_inmaterial %>% mutate(tipo = "patrimonio inmaterial") 

#se une la nueva tabla para cada uno y así se asegura una gráfica visual eficaz.
todo <- bind_rows(resultado, resultado_material, resultado_inmaterial)
#representación visual de la gráfica
#Aparecerán no por cada palabra original, sino a las normalziadas semánticamente asociadas al csv y se reduce el ruido de datos
#Para mayor precisión de dichas palabras normalziadas, pues aún son numerosas entre las tres categorías, gracias al paso anterior están divididas según su categoría y el máximo de 20 por categoría
top20 <- todo %>%
  group_by(tipo) %>%
  slice_max(menciones, n = 20) %>%
  ungroup()
#creación definitiva con nuevas funciones y nuevo paquete diferente:ggplot
ggplot(top20, aes(x = reorder(principal, menciones), y = menciones, fill = tipo)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~tipo, scales = "free_y") +
  labs(title = "Frecuencia en Unexplored spain de Buck and Chapman (1910)",
       x = NULL, y = "Menciones") +
  theme_minimal() +
  theme(legend.position = "none")

#por último se exporta la imagen ajustada
ggsave("1910_patrimonio__Buck_Chapman_Unexplored.png", width = 12, height = 8) 

#PASO DIEZ Y ÚTLIMO: análisis de sentimientos.

#Como ya se ha hecho el preprocesmiento al inicio, únicamente hay que cargar el paquete que vamos a usar
#Syuzhet
#Este sí se instala y se carga, ya que no forma parte del tidyverse
install.packages("syuzhet")
library(syuzhet)
#Tras cargarse el paquete, se crea el comando para operar con sus algoritmos para crear una equivalencia de emociones sobre el texto
#Debido a que el sentimiento depende del contexto de la frase, apra este caso enc conreto, no se divide en tokens, sino en frases para asía captar la esencia real.
#En este caso concreto, en P.MATERIAL ha salido la palabra bueno como la de mayor mención. Pero se necesita al frase, proque puede darse el caso de que esté acompañado de un "NO" que cambia por completo la frase.
#Para ello, es esencial un diccionario especializado que sepa categorizar estas emociones.
#En consecuencia se opta por este del National Research Council (NRC)que categoriza en occho emociones específicas y dos generales
#Más aún en relatos de viaje con tanta carga literaria y sentido diverso.
frases <- get_sentences(libro_para_contar)
emociones <- get_nrc_sentiment(frases)
# A continuación se desglosa el resultado 
totales <- colSums(emociones)
totales
#Ahora se crea al gráfica, pero con un cambio
#Para esta investigación, se distinguen tres colores entre los diez:
#en color verde  son 4 las emociones positivas, incluyendo la general positiva
#en color rojo son 5 las emcoiones negativas, incluyendo la general negativa
#por último se deja en color aparte surprise, pues es la más interpretativa y ambigua, pudiendo ser negativa y posoitiva

colores <- c("red", "green", "red", "red", "green", "red", "orange", "green", "red", "green")
barplot(totales,
        las = 2,
        col = colores,
        main = "Análisis de sentimientos - Buck/Chapman 1910 Unexplored spain  ",
        ylab = "Número de palabras")
legend("topleft", 
       legend = c("Positivo", "Negativo", "Neutro/Sorpresa"),
       fill = c("green", "red", "orange"),
       cex = 0.7)

#Por último, exportamos los resultados cuantitativo de sentimeintos y la gráfica.
write.csv(totales, "1910_sentimientosBuck_Chapman_Unexplored.csv")
