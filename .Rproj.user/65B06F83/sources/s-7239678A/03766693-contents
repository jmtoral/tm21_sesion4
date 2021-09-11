pacman::p_load(tidyverse, tidytext, tm, quanteda,
               wesanderson, ggalt, hrbrthemes, text2vec)

huerta <- read_csv("input/w_huerta.csv",
                   locale = locale(encoding = "ISO-8859-1")) %>% 
  unnest_tokens(palabra, verso) %>% 
  select(libro, word=palabra) %>% 
  filter(!word %in% tm::stopwords("es"),
         nchar(word) > 3)

## Lista

words_ls <- list(huerta$word)

it <- itoken(words_ls) #Lista de palabras



## Vocabulario

vocab <- create_vocabulary(it, ngram =c(1,2)) %>% 
  prune_vocabulary(term_count_min = 10, 
                   #doc_proportion_max = 0.5
                   )

# Matriz de co-ocurrencia (TCM = Term co-ocurrence matriz)


#pruned_vocab = prune_vocabulary(vocab, 
 #                               term_count_min = 10, 
  #                              doc_proportion_max = 0.5,
   #                             doc_proportion_min = 0.001)

print(vocab)

vector <- vocab_vectorizer(vocab)

tcm <- create_tcm(it, vector, skip_grams_window = 10)

#### GLobal Vector

## GloVe Word

glove <- GlobalVectors$new(
  rank = 50,
  x_max = 10
)

wv_main = glove$fit_transform(tcm , n_iter = 10, convergence_tol = 0.01)

wv_context <- glove$components

word_vector <- wv_main + t(wv_context)

### Algunas operaciones de vector

amor <- word_vector["amor", , drop=F]

cos_sim <- sim2(x = word_vector, 
                y= amor,
                method = "cosine")

head(sort(cos_sim[,1], decreasing = T), 30)



########### Operaciones con vectores

odio <- word_vector["poema", , drop=F] -
  word_vector["amor", , drop = F] 

cos_sim <- sim2(x = word_vector, 
                y= odio,
                method = "cosine")

head(sort(cos_sim[,1], decreasing = T), 5)


poder <- word_vector["power", , drop=F]-
  word_vector["death", , drop = F] 

cos_sim <- sim2(x = word_vector, 
                y= poder,
                method = "cosine")

head(sort(cos_sim[,1], decreasing = T), 10)
