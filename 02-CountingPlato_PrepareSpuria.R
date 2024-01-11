# spuria were copied manually from Diogenes (Halcyon in Lucian's works)
files <- list.files("spuria")
file_paths <- paste0("spuria/", files)

spuria <- map(file_paths, readLines)
names(spuria) <- str_replace_all(files, "\\.txt")

spuria <- spuria %>% 
  stack() %>% 
  as_tibble() %>% 
  rename(text = values, title = ind) %>% 
  relocate(text, .after = title) %>% 
  mutate(text = str_replace_all(text, "'", "ʼ"))

#save(spuria, file =  "./data/spuria.Rdata")

spuria_words <- spuria %>% 
  mutate(text = str_replace_all(text, "·", " ")) %>% 
  mutate(text = str_replace_all(text, "\\.", " ")) %>% 
  unnest_tokens(word, text)

spuria_stats <- spuria_words %>% 
  group_by(title) %>% 
  summarise(total = n())

summary(spuria_stats$total)

