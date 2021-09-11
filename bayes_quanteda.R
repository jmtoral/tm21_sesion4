# Clasificador de Bayes
# 1 de septiembre de 2021


# Biblitoecas -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pacman::p_load(quanteda, caret, tidyverse, readxl,
               quanteda.textmodels)


# Datos -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

noticias <- readxl::read_excel("input/train.xlsx") %>% 
  select(Text, Category)


# Corpus ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

corp_not <- corpus(noticias,  text_field = "Text")

summary(corp_not)


# Muestras ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

set.seed(123)

id_train <- sample(1:676, 510)
head(id_train)

corp_not$id_num <- 1:ndoc(corp_not)

# tokenizador

token_not <- tokens(corp_not, remove_punct = T, remove_number = T,
                    remove_symbols = T, remove_separators = T) %>%
  tokens_tolower() %>% 
  tokens_remove(pattern = c(stopwords("es"))) %>% 
  tokens_wordstem()

dfm <- dfm(token_not)

# Dividr entrenamiento y prueba

df_train <- dfm_subset(dfm, id_num %in% id_train)
df_test <- dfm_subset(dfm, !id_num %in% id_train)


# Modelo

modelo_nb <- textmodel_nb(df_train, df_train$Category)

summary(modelo_nb)


# Evaluar -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

match <- dfm_match(df_test, features = featnames(df_train))

clasificacion_real <- match$Category

clasificacion_predicha <- predict(modelo_nb, newdata = match)

comparacion <- table(clasificacion_real, clasificacion_predicha)

confusionMatrix(comparacion, mode = "everything")
