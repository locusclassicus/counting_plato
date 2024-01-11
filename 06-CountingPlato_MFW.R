load("./data/corpus_plato_xen.Rdata")
library(tidyr)
library(tidytext)

# word frequencies by title
corpus_counts <- corpus_plato_xen %>% 
  group_by(title) %>% 
  count(word) %>% 
  ungroup()

# add total counts by title
corpus_counts_total <- corpus_counts %>%
  inner_join(corpus_stats)

# tf, idf, tf_idf by title
corpus_tf <- corpus_counts_total %>% 
  bind_tf_idf(word, title, n)

# most frequent words (all)
freq_vec <- corpus_plato_xen %>% 
  count(word) %>% 
  arrange(-n) %>%
  filter(word != "σώκρατες") %>% 
  pull(word)
  
mfw100 <- freq_vec[1:100]
mfw200 <- freq_vec[1:200]
mfw300 <- freq_vec[1:300]
mfw400 <- freq_vec[1:400]

# prepare dtm100 
dtm100 <- corpus_tf %>% 
  filter(word %in% mfw100) %>% 
  select(title, word, tf) %>% 
  pivot_wider(names_from = word, values_from = tf, values_fill = 0) %>% 
  as.data.frame() 

rownames(dtm100) <- dtm100$title

dtm100 <- dtm100 %>% 
  select(-title)

# prepare dtm200 
dtm200 <- corpus_tf %>% 
  filter(word %in% mfw200) %>% 
  select(title, word, tf) %>% 
  pivot_wider(names_from = word, values_from = tf, values_fill = 0) %>% 
  as.data.frame() 

rownames(dtm200) <- dtm200$title

dtm200 <- dtm200 %>% 
  select(-title)

# prepare dtm300 
dtm300 <- corpus_tf %>% 
  filter(word %in% mfw300) %>% 
  select(title, word, tf) %>% 
  pivot_wider(names_from = word, values_from = tf, values_fill = 0) %>% 
  as.data.frame() 

rownames(dtm300) <- dtm300$title

dtm300 <- dtm300 %>% 
  select(-title)

# prepare dtm400 
dtm400 <- corpus_tf %>% 
  filter(word %in% mfw400) %>% 
  select(title, word, tf) %>% 
  pivot_wider(names_from = word, values_from = tf, values_fill = 0) %>% 
  as.data.frame() 

rownames(dtm400) <- dtm400$title

dtm400 <- dtm400 %>% 
  select(-title)

# save(dtm100, file = "./data/dtm100.Rdata")
# save(dtm200, file = "./data/dtm200.Rdata")
# save(dtm300, file = "./data/dtm300.Rdata")
# save(dtm400, file = "./data/dtm400.Rdata")
