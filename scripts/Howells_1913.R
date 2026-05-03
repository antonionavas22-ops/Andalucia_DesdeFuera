# CCOMPROBACIONES PREVIAS
#primer paso: Ver la ruta
getwd()
#segundo paso: ver si están los documentos csv
list.files()
#tercer paso: cargar el relato
relato <- readLines("1913_Howells_familiy_spanish.txt", encoding = "UTF-8")
#COMIENZO DE LAS OPERACIONES PARA AJUSTARSE A R
#cuarto paso: cambio de estructura para trabajar con R
libro <- paste(relato, collapse = " ")
#quinto paso: todo a minúscula para trabajar con R
#OJO, no se hace con tidytext, porque no nos interesa para la lista csv que queremos cruzar, pues son palabras compuestas
# Y el paquete tidytexto incluye el tokenizador y separaría las palabras compuestas
#por tanto, se usa la función independiente de tolower
#tokenizar se reserva para el análisis de sentimientos
libro_previo <- tolower(libro) 
#sexto paso: se instala el ecosistema tidyverse que incluye los paquetes y sus funciones para las operaciones
install.packages("tidyverse")
#Una vez instalado, se carga
library("tidyverse") 
#AHORA LAS OPERACIONES CON LOS TRES CSV (LOCALIIDADES, PATRIMONIO MATERIAL E INMATERIAL)
#septimo paso: se cargan sin necesidad de caragr el paquete readr, pues ya está instalado e incluido en tidyverse
localidades <- read_csv("localidades_normalizacion.csv")
View(localidades)
patrimonio_material <- read_csv("patrimonio_material_normalizacion.csv")
View(patrimonio_material)
patrimonio_inmaterial <- read_csv("patrimonio_inmaterial_normalizacion.csv")
View(patrimonio_inmaterial) 
#octavo paso: se hacen las operaciones, una por cada csv
#se usa las mismas funciones de los paquetes de tidyverse, como dplyr, magrittr, readr o stringr
#localidades
resultado <- localidades %>%
  mutate(menciones = str_count(libro_previo, paste0("\\b", variantes, "\\b"))) %>%
  filter(menciones > 0) %>%
  arrange(desc(menciones))
# lo visualizamos en la tabla
View(resultado)
#mismo proceso con patrimonio material
resultado_material <- patrimonio_material %>%
  mutate(menciones = str_count(libro_previo, paste0("\\b", variantes, "\\b"))) %>%
  filter(menciones > 0) %>%
  arrange(desc(menciones))
#visualizamos el resultado
View(resultado_material)
#mismo proceso con patrimonio inmaterial
resultado_inmaterial <- patrimonio_inmaterial %>%
  mutate(menciones = str_count(libro_previo, paste0("\\b", variantes, "\\b"))) %>%
  filter(menciones > 0) %>%
  arrange(desc(menciones))
#visualizamos el resultado
View(resultado_inmaterial)
#noveno paso: se exportan los resultados de cada uno fuera de R
#localidades
write_csv(resultado, "resultado_localidades_Howells.csv") 
#patrimonio material
write_csv(resultado_material, "resultado_material_Howells.csv")
#patrimonio inmaterial
write_csv(resultado_inmaterial, "resultado_inmaterial_Howells.csv") 
#décimo paso: creación de gráficas conjuntas en mismo espacio, pero manteninedo cada una su integridad
resultado <- resultado %>% mutate(tipo = "localidades") 
resultado_material <- resultado_material %>% mutate(tipo = "patrimonio material")
resultado_inmaterial <- resultado_inmaterial %>% mutate(tipo = "patrimonio inmaterial") 
#se une la nueva tabla apra cada uno
todo <- bind_rows(resultado, resultado_material, resultado_inmaterial)
#undécimo paso: representación visual de la gráfica
top25 <- todo %>%
  group_by(tipo) %>%
  slice_max(menciones, n = 25) %>%
  ungroup()
#duodécimo paso y últtimo que une todo lo anterior del preoceso de gráfica
#crecaicón definitiva con nuevas funciones y nuevo paquete diferente:ggplot
ggplot(top25, aes(x = reorder(principal, menciones), y = menciones, fill = tipo)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~tipo, scales = "free_y") +
  labs(title = "Menciones en Howells (1913)",
       x = NULL, y = "Menciones") +
  theme_minimal() +
  theme(legend.position = "none")
#dédimotercer paso o epílogo: exportación a imagen
ggsave("grafica_Howells.png", width = 12, height = 8) 