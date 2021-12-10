library(tidyverse)
library(tidytext)
library(stringi)
library(stm)

#stm_rock <- read_rds('./models/stm_test_20topics_20210923.rds')

df <- read_csv('./data/proc/data_topics.csv')

df <- df %>%
        mutate(fecha = case_when(
                fecha <= 1967 ~ 1968,
                fecha > 2018 ~ 2018,
                TRUE ~ fecha
        ))


df <- df %>% 
        group_by(banda, cancion, letra_ed) %>% 
        filter(row_number() == 1) %>%
        ungroup() %>%
        mutate(tit_letra = paste(cancion, letra_ed),
               tit_letra = stri_trans_general(tit_letra,"Latin-ASCII"))


stop_words <- read_csv('./data/auxiliar/stop_words_complete.csv')

stop_words <- stop_words %>% 
        filter(lexicon=='custom') %>% 
        mutate(word = stri_trans_general(word,"Latin-ASCII")) %>%
        select(word) %>% 
        pull()

proc <- textProcessor(df$tit_letra, 
                      metadata = df %>% select(url_cancion, fecha, decada, banda, cancion, letra, tit_letra),
                      language = 'spa',
                      stem = FALSE,
                      lowercase = FALSE,
                      removestopwords = FALSE,
                      removenumbers = FALSE,
                      customstopwords = stop_words)


out <- prepDocuments(proc$documents, 
                     proc$vocab,
                     proc$meta,
                     lower.thresh = 15)


docs <- out$documents
vocab <- out$vocab
meta <- out$meta

stm_rocknacional <- stm(documents = out$documents, vocab = out$vocab,
                        K = 20, 
                        prevalence = ~ fecha + banda + fecha*banda, 
                        max.em.its = 75,
                        data = out$meta, 
                        init.type = "Spectral")
