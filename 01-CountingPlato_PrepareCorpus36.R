library(stringr)
library(tidyr)

# get corpus
file_names <- list.files("PerseusXML")
source("get_text.R")
corpus <- map_df(file_names, get_text)

# clean titles
corpus_clean <- corpus %>% 
  mutate(title = str_remove(title, "\\.xml"))

# clean texts and change page type
corpus_clean2 <- corpus_clean %>% 
  mutate(text = str_remove(text, "[ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ]{2,4}\\.")) %>% 
  mutate(page = as.numeric(page))

# divide letters
letters <- corpus_clean2 %>% 
  filter(title == "Letters") %>% 
  mutate(letter_nr = case_when(str_detect(text, "^Πλάτων") ~ 1,
                               .default = 0)) %>% 
  mutate(letter = cumsum(letter_nr)) %>% 
  select(-letter_nr) %>% 
  unite(title, c("title", "letter"))
  
# divide laws
book1 <- 624:650; book2 <- 652:674; book3 <- 676:702;
book4 <- 704:724; book5 <- 726:747; book6 <- 751:785;
book7 <- 788:824; book8 <- 828:850; book9 <- 853:882;
book10 <- 884:910; book11 <- 913:938; book12 <- 941:969

laws <- corpus_clean2 %>% 
  filter(title == "Laws") %>% 
  mutate(book_nr = case_when(
    page %in% book1 ~ 1,
    page %in% book2 ~ 2,
    page %in% book3 ~ 3,
    page %in% book4 ~ 4,
    page %in% book5 ~ 5,
    page %in% book6 ~ 6,
    page %in% book7 ~ 7,
    page %in% book8 ~ 8,
    page %in% book9 ~ 9,
    page %in% book10 ~ 10,
    page %in% book11 ~ 11,
    page %in% book12 ~ 12
    )) %>% 
  unite(title, c("title", "book_nr"))

# divide republic
book1 <- 327:354; book2 <- 357:383; book3 <- 386:417;
book4 <- 419:445; book5 <- 449:480; book6 <- 484:511;
book7 <- 514:541; book8 <- 543:569; book9 <- 571:592;
book10 <- 595:621

republic <- corpus_clean2 %>% 
  filter(title == "Republic") %>% 
  mutate(book_nr = case_when(
    page %in% book1 ~ 1,
    page %in% book2 ~ 2,
    page %in% book3 ~ 3,
    page %in% book4 ~ 4,
    page %in% book5 ~ 5,
    page %in% book6 ~ 6,
    page %in% book7 ~ 7,
    page %in% book8 ~ 8,
    page %in% book9 ~ 9,
    page %in% book10 ~ 10
  )) %>% 
  unite(title, c("title", "book_nr"))

# join letters, laws, republic, and the rest
corpus_36 <- corpus_clean2 %>% 
  filter(!title %in% c("Letters", "Republic", "Laws")) %>% 
  bind_rows(republic) %>% 
  bind_rows(letters) %>% 
  bind_rows(laws) 

# clean text from things like "Hom. Il. 2.303" and "\n"
corpus_36 <- corpus_36 %>% 
  mutate(text = str_replace_all(text, "\n", " ")) %>% 
  mutate(text = str_remove_all(text, "[A-Za-z0-9]"))

# save(corpus_36, file =  "./data/corpus_36.Rdata")

