getwd()
list.files()
relato <- readLines("Calvert_1908.txt", encoding = "UTF-8")
libro <- paste(relato, collapse = " ")
libro_previo <- tolower(libro)
install.packages("tidyverse")
library("tidyverse")
install.packages ("tidytext")
library("tidytext")
library("readxl")
localidades<- read_excel("normalizacion_localidades.xlsx")
View(localidades)
resultado <- localidades %>%
  mutate(menciones = str_count(libro_previo, fixed(english))) %>%
  filter(menciones > 0) %>%
  arrange(desc(menciones))
View(resultado)
