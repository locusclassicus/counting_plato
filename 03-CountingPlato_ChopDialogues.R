library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidytext)
load("./data/corpus_36.Rdata")

corpus_pages <- corpus_36 %>% 
  group_by(title, page) %>% 
  mutate(text = str_c(text, collapse = " ")) %>% 
  mutate(text = str_squish(text)) %>% 
  ungroup() %>% 
  distinct(title, page, text)

#find dialogues with > 30 pages
large_dialogues <- corpus_pages %>% 
  group_by(title) %>% 
  summarise(total_pages = n()) %>% 
  filter(total_pages > 30)
 
# chop large
large_chopped <- corpus_pages %>%
  filter(title %in% large) %>% 
  group_by(title) %>% 
  left_join(large_dialogues) %>% 
  mutate(chunk = (row_number() + 14) %/% 15) %>% 
  select(-total_pages) %>% 
  group_by(title, chunk) %>% 
  add_count() %>% 
  # joining smaller chunks to the previous ones
  mutate(chunk = case_when(n < 9 ~ chunk - 1,
                           .default = chunk)) %>% 
  ungroup() %>% 
  select(-n)

# this should facilitate the interpretation
large_chopped_labels <- large_chopped %>% 
  group_by(title, chunk) %>% 
  mutate(min_page = min(page)) %>% 
  mutate(max_page = max(page)) %>% 
  ungroup()

  
large_chopped_united <- large_chopped_labels %>% 
  select(-page) %>% 
  unite(title, c(title, chunk, min_page, max_page), sep = "_") %>% 
  group_by(title) %>% 
  mutate(text = str_c(text, collapse = " ")) %>% 
  mutate(text = str_squish(text)) %>% 
  ungroup() %>% 
  distinct(title, text)

smaller_corpus <- corpus_pages %>%
  filter(!title %in% large) %>% 
  group_by(title) %>% 
  mutate(text = str_c(text, collapse = " ")) %>% 
  mutate(text = str_squish(text)) %>% 
  ungroup() %>% 
  distinct(title, text)

corpus_36_chopped <- large_chopped_united %>% 
  bind_rows(smaller_corpus)


# merge tibbles
load("./data/spuria.Rdata")

corpus_all <- corpus_36_chopped %>% 
  bind_rows(spuria) %>% 
  mutate(text = str_replace_all(text, "·", " ")) %>% 
  mutate(text = str_replace_all(text, "\\.", " ")) %>% 
  # some labels have escaped 
  mutate(text = str_remove_all(text, "ΣΩ"))

# unnest words
corpus_words <- corpus_all %>% 
  unnest_tokens(word, text)

# remove texts < 900 words
smaller <- corpus_words %>%
  group_by(title) %>% 
  summarise(total = n()) %>% 
  filter(total <= 900) %>% 
  pull(title)

corpus_words <- corpus_words %>% 
  filter(!title %in% smaller)

# save(corpus_words, file =  "./data/corpus_words.Rdata")

