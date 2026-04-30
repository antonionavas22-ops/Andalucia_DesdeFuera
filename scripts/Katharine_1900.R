#Comprobaciones previas
#Ruta donde está alojado el pgrograma
getwd()
# función para verificar que el realto de viaje y los documentos excel están alojados correctamente
list.files()
# fin de comprobaciones
# incio de la carga del relato de viaje
relato <- readLines("1900_Bates_spanish_Highway_Bayway.txt", encoding = "UTF-8")
# Se adapta a R
libro <- paste(relato, collapse = " ") 
#PASO IMPORTANTE A CONTINUACIÓN. se debería de cargar los paqeuetes, pero por los problemas de normaliazión. este paso se sustuye por tolower.
#Como tidytext ha dado error, no se tokniza, pero se convierte todo en minuscula
#TOLOWER
libro_previo <- tolower(libro)
# SE cargan los paquetes del tidyverse para el análisis
install.packages("tidyverse”) 
library("tidyverse")
#Ahora se cargan las tablas apra cruzar los resultados en formato csv para evitar problemas
localidades <- read_csv("localidades_normalizacion.csv")
#Confirmamos que está todo Ok
glimpse(localidades)
View(localidades)
#Mismo procedeimeinto con el resto
patrimonio_material <- read_csv("patrimonio_material_normalizacion.csv")
View(patrimonio_material)
patrimonio_inmaterial <- read_csv("patrimonio_inmaterial_normalizacion.csv")
View(patrimonio_inmaterial)
#LO MAS IMPORTANTE, la operación añadiendo las funciones de los paQUETES DEL TU¡IDYVERSE INSTALADOS
#no es necesario caragrlos
resultado <- localidades %>%
  mutate(menciones = str_count(libro_previo, fixed(variantes))) %>%
  filter(menciones > 0) %>%
  arrange(desc(menciones))

View(resultado)
#Ahora se exporta el resultado para poderlo guardar en GutHub
write_csv(resultado, "resultado_localidades_Bates.csv")
# Ahora mismo proceso pero con los elementos del aptrimonio
#PATRIMONIO MATERIAL
resultado_material <- patrimonio_material %>%
mutate(menciones = str_count(libro_previo, fixed(variantes))) %>%
filter(menciones > 0) %>%
arrange(desc(menciones))
View(resultado_material)

# A continuación se xporta los resultados del amterial

write_csv(resultado_material, "resultado_material_Bates.csv") 

# Por último el inmaterial y su resultados

resultado_inmaterial <- patrimonio_inmaterial %>%
mutate(menciones = str_count(libro_previo, fixed(variantes))) %>%
filter(menciones > 0) %>%
arrange(desc(menciones))
View(resultado_inmaterial)

# al igual que los anteriores, se exporta´

write_csv(resultado_inmaterial, "resultado_inmaterial_Bates.csv") 

#Ahora procedemos a crear las GRÁFICAS
#Se crea una nueva tabla por tipo de patrimonio
resultado <- resultado %>% mutate(tipo = "localidades")
resultado_material <- resultado_material %>% mutate(tipo = "patrimonio material")                
resultado_inmaterial <- resultado_inmaterial %>% mutate(tipo = "patrimonio inmaterial")
# A continuación s eunen las tres, pero respetando la integridad de cada una de ellas
todo <- bind_rows(resultado, resultado_material, resultado_inmaterial)
# Y por último se representa visualmente
top20 <- todo %>%
  top20 <- todo %>%
  group_by(tipo) %>%
  slice_max(menciones, n = 20) %>%
  ungroup()

ggplot(top20, aes(x = reorder(principal, menciones), y = menciones, fill = tipo)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~tipo, scales = "free_y") +
  labs(title = "Menciones en Bates (1900)",
       x = NULL, y = "Menciones") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("grafica_Bates.png", width = 12, height = 8)
