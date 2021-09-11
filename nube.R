## Titulo: Nubes de palabras
## Fecha: 11 de septiembre de 2021


# Bibliotecas -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pacman::p_load(tidyverse, tidytext, wordcloud)

#wordcloud2


# Datos -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

huerta <- read_csv("input/w_huerta.csv",
                   locale = locale(encoding = "LATIN1")) #Encoding para usuarias de Windows
#  ISO-8859-1 = LATIN1


# Conteo de palabras ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pal_nube <- huerta %>% 
  unnest_tokens(palabras, verso) %>% 
  count(palabras, sort = T) %>% 
  filter(!palabras %in% c(tm::stopwords("es"), "amor"))

pal_nube %>% 
  with(wordcloud(palabras, n, max.words = 100,
                 random.order = F,
                 colors = brewer.pal(8, "Reds")))
  

wordcloud(pal_nube$palabras, pal_nube$n, max.words = 100,
          random.order = F,
          colors = brewer.pal(8, "Reds"))


png("nube.png", width = 400, height = 400)

wordcloud(pal_nube$palabras, pal_nube$n, max.words = 100,
          random.order = F,
          colors = brewer.pal(8, "Reds"))
dev.off()