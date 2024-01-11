library(dplyr)
library(ggplot2)

#first join Plato and Xenophon in one corpus
load("./data/corpus_words.Rdata")
load("./data/hellenica.Rdata")
load("./data/hiero.Rdata")
load("./data/symposium.Rdata")
load("./data/memorabilia.Rdata")

corpus_plato_xen <- corpus_words %>%
  #bind_rows(hellenica_words) %>%
  bind_rows(memorabilia_words) %>%
  bind_rows(hiero_words) %>%
  bind_rows(symposium_words)

#save(corpus_plato_xen, file = "./data/corpus_plato_xen.Rdata")
load("./data/corpus_plato_xen.Rdata")
# rm(hellenica_words)
# rm(hiero_words)
# rm(memorabilia_words)
# rm(symposium_words)
# rm(corpus_words)

# general stats 
corpus_stats <- corpus_plato_xen %>% 
  group_by(title) %>% 
  summarise(total = n()) %>% 
  arrange(-total)

#save(corpus_stats, file = "./data/corpus_stats.Rdata")

# plot
corpus_stats %>% 
  ggplot(aes(total)) +
  geom_histogram()

# summary
summary(corpus_stats$total)

