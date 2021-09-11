pacman::p_load(quanteda, tidyverse,  seededlda, readxl)

#noticias_falsas <- read_csv("input/onlyfakes1000.csv") 

noticias_falsas <- read_excel("input/train.xlsx") %>% 
  filter(Category=="Fake")
  
corp_falsas <- corpus(noticias_falsas, text_field = "Text")

docid <- noticias_falsas$Headline

docnames(corp_falsas) <- docid

summary(corp_inaug, 5)


###

toks_news <- tokens(corp_falsas, remove_punct = TRUE, remove_numbers = TRUE, remove_symbol = TRUE)

toks_news <- tokens_remove(toks_news, pattern = c(stopwords("es"), "*-time", "updated-*", "gmt", "bst",
                                                  "si", "van", "ser", "tras", "solo", "cada"))

dfmat_news <- dfm(toks_news) %>% 
  dfm_trim(min_termfreq = 0.8, termfreq_type = "quantile",
           max_docfreq = 0.1, docfreq_type = "prop")

tmod_lda <- textmodel_lda(dfmat_news, k = 10)


terms(tmod_lda, 10)


dfmat_news$topic <- topics(tmod_lda)
table(dfmat_news$topic)

x <- as_tibble(tmod_lda$theta, rownames="doc_id") %>% 
  arrange(-topic2)

#noticias_falsas[808,2] %>% as.character()



### Seeded LDA

my_dict <- dictionary(list(politica = c("amlo", "anaya", "meade", "voto", "votar", "eleccion", "salinas"),
                           internet = c( "facebook", "hi5", "internet", "datos", "red", "social"),
                           eua = c("trump", "donald", "blanca", "wahington")))

## fit the model

slda <- textmodel_seededlda(x = dfmat_news, dictionary = my_dict, 
                            residual = T) 

x <- as_tibble(slda$theta, rownames = "doc_id") 
