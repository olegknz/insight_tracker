library(text2vec)
library(stringr)
library(tidytext)
library(tidyverse)

text <- c("Ayk had left VK Advertising Technologies and was the guest at the meeting.",
           "Ayk told about his background and how he got to VK",
           "He also talked about the history of Yula, which he had helped to create and grow.")
source <- c("in1", "in2", "in3")

insights = data.frame(text, source)

univ_tokens = insights %>% 
  unnest_tokens(output = words, input = text)

load("word_vectors.RDS")
source("emb.R")

dict = row.names(word_vectors)

univ_tokens = univ_tokens %>% 
  filter(words %in% dict) %>% 
  unique()

univ_embeddings = univ_tokens %>% 
  rowwise() %>%
  mutate(emb = str_c(get_embeddings(words), collapse = " ") ) 

univ_embeddings = univ_embeddings %>% 
  separate(emb, into = as.character(c(1:50)), sep = " ")

doc_emb = univ_embeddings %>%
  select(-words) %>% 
  group_by(source) %>% 
  mutate_at(vars(-group_cols()), as.numeric) %>% 
  summarise_all(mean) %>%
  column_to_rownames("source")

lsa::cosine(t(as.matrix(doc_emb)))
