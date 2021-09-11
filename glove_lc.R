# Vectorización de textos
# 11 de septiembre 2021


# Bibliotecas -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pacman::p_load(tidyverse, 
               text2vec,
               tidytext)


# Datos -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

huerta_tokens <- read_csv("input/w_huerta.csv",
                   locale = locale(encoding = "LATIN1")) %>% 
  unnest_tokens(palabra, verso) %>% 
  select(id_libro, palabra) %>% 
  filter(!palabra %in% tm::stopwords("es"),
         nchar(palabra) > 3)


# Iterador ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

lista_palabras <- list(huerta_tokens$palabra)
lista_docs <- list(huerta_tokens$id_libro)

it <- itoken(lista_palabras)


# vocabulario -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

vocab <- create_vocabulary(it,
                           ngram = c(1,2)) %>% 
  prune_vocabulary(term_count_min = 5)

vocab


# Vectorizar --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

vector <- vocab_vectorizer(vocab)


# Matriz coocurrencia -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

tcm <- create_tcm(it, vector, skip_grams_window = 10)


# GloVe -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

glove <- GlobalVectors$new(
  rank = 50, 
  x_max = 10
)

wv_main <- glove$fit_transform(tcm, n_iter = 100, convergence_tol = 0.001)

wv_context <- glove$components

word_vector <- wv_main + t(wv_context)

# Algunas operaciones de vectores ----

amor <- word_vector["amor", , drop =F]

cos_sim <- sim2(x = word_vector,
                y = amor,
                method = "cosine")

head(sort(cos_sim[,1], decreasing = T), 10)



amor_sin_poema <- word_vector["amor", , drop =F] -
  word_vector["poema", , drop =F]

cos_sim <- sim2(x = word_vector,
                y = amor_sin_poema,
                method = "cosine")

head(sort(cos_sim[,1], decreasing = T), 10)

