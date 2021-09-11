# LDA y Seeded LDA
# 11 de septiembre de 2021


# Bibliotecas -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

pacman::p_load(tidyverse,
               readxl,
               quanteda, #text mining 
               seededlda)


# Datos -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

noticias <- read_excel("input/train.xlsx")

falsa <- noticias %>% 
  filter(Category == "Fake") %>% 
  select(Headline, Text)


# Crear un corpus ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

corp_falsa <- corpus(falsa, text_field = "Text")

summary(corp_falsa)

docnames(corp_falsa) <- falsa$Headline


summary(corp_falsa)



# Preprocesamiento --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

token_falsa <- tokens(corp_falsa, remove_punct = T, remove_numbers = T, remove_symbols = T) %>% 
  tokens_remove(pattern = c(stopwords("es"), "si", "van", "ser", "tras", "solo", "cada"))



# Matriz Documento Frecuencia ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

df_falsa <- dfm(token_falsa) %>% 
  dfm_trim(min_termfreq = 0.8, termfreq_type = "quantile",
           max_docfreq = 0.1, docfreq_type = "prop")


# LDA ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Distribución Latente de Dirichlet

modelo_lda <- textmodel_lda(df_falsa, k=5)
terms(modelo_lda, 30)

df_falsa$topic <- topics(modelo_lda)

clasificacion <- as_tibble(modelo_lda$theta, rownames = "doc_id")


# Seeded LDA / LDA con semilla


dic <- dictionary(
  list(
    politica = c("presidente", "poder", "amlo", "reelección", "fraude", "votos", "pri", "coorupción",
                 "renuncia", "revocación", "anaya", "meade", "morena", "salinas", "peña"),
    eua = c("trump", "donald", "frontera","mcdonald's", "migración", "rusia", "racismo",
            "china", "policía"),
    deportes = c("américa", "selección", "futbol", "chivas", "mundial", "cancha", "árbitro",
                 "azteca", "messi"),
    morena = c("amlo", "AMLO", "morena", "lópez", "obrador", "gobierno", "tranformación", "cuarta",
               "tatiana"),
    pan = c("pan", "anaya", "josefina", "margarita", "zavala", "ricardo")
  )
)

slda <- textmodel_seededlda(x = df_falsa,
                            dictionary = dic,
                            residual = T)

res <- as_tibble(slda$theta, rownames= "doc_id")






