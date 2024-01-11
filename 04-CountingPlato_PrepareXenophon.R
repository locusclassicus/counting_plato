library(XML)
library(purrr)
library(stringr)
library(tidyr)
library(tidytext)

file_names <- list.files("./Xenophon")

# two helper functions
get_chapter <- function(paragraph) {
  xmlGetAttr(xmlParent(xmlParent(paragraph)), "n")
}

get_book <- function(paragraph) {
  xmlGetAttr(xmlParent(xmlParent(xmlParent(paragraph))), "n")
}

# memorabilia
file_path <- paste0("./Xenophon/", file_names[3])
doc <- xmlTreeParse(file_path, useInternalNodes = T, isURL = FALSE)

paragraphs <- getNodeSet(doc, 
                         "//tei:div//tei:p",
                         namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))
books <- map_chr(paragraphs, get_book)
text <- map_chr(paragraphs, xmlValue)

memorabilia_tbl <- tibble(title = file_names[3],
                          book = books,
                          #chapter = chapters,
                          text = text)

# clean memorabilia
memorabilia <- memorabilia_tbl %>% 
  mutate(title = str_remove(title, "\\.xml$")) %>%
  unite(title, c("title", "book")) %>% 
  group_by(title) %>% 
  mutate(text = str_c(text, collapse = " ")) %>% 
  distinct(title, text)

# tidy memorabilia
memorabilia_words <- memorabilia %>%
  unnest_tokens(word, text)

#save(memorabilia_words, file = "./data/memorabilia.Rdata")

# hiero
file_path <- paste0("./Xenophon/", file_names[2])
doc <- xmlTreeParse(file_path, useInternalNodes = T, isURL = FALSE)

paragraphs <- getNodeSet(doc, 
                         "//tei:div//tei:p",
                         namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))
chapters <- map_chr(paragraphs, get_chapter)
text <- map_chr(paragraphs, xmlValue)

hiero_tbl <- tibble(title = file_names[2],
                          chapter = chapters,
                          text = text)

# clean hiero
hiero <- hiero_tbl %>% 
  mutate(title = str_remove(title, "\\.xml$")) %>%
  #unite(title, c("title", "chapter")) %>% 
  group_by(title) %>% 
  mutate(text = str_c(text, collapse = " ")) %>% 
  distinct(title, text)

# tidy hiero
hiero_words <- hiero %>%
  unnest_tokens(word, text)

#save(hiero_words, file = "./data/hiero.Rdata")

# symposium
file_path <- paste0("./Xenophon/", file_names[4])
doc <- xmlTreeParse(file_path, useInternalNodes = T, isURL = FALSE)

paragraphs <- getNodeSet(doc, 
                         "//tei:div//tei:p",
                         namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))
chapters <- map_chr(paragraphs, get_chapter)
text <- map_chr(paragraphs, xmlValue)

symposium_tbl <- tibble(title = file_names[4],
                    chapter = chapters,
                    text = text)

# clean symposium
symposium <- symposium_tbl %>% 
  mutate(title = str_remove(title, "\\.xml$")) %>%
  #unite(title, c("title", "chapter")) %>% 
  group_by(title) %>% 
  mutate(text = str_c(text, collapse = " ")) %>% 
  distinct(title, text)

# tidy symposium
symposium_words <- symposium %>%
  unnest_tokens(word, text)

#save(symposium_words, file = "./data/symposium.Rdata")

# hellenica
file_path <- paste0("./Xenophon/", file_names[1])
doc <- xmlTreeParse(file_path, useInternalNodes = T, isURL = FALSE)

paragraphs <- getNodeSet(doc, 
                         "//tei:div//tei:p",
                         namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))
books <- map_chr(paragraphs, get_book)
text <- map_chr(paragraphs, xmlValue)

hellenica_tbl <- tibble(title = file_names[1],
                          book = books,
                          text = text)

# clean hellenica
hellenica <- hellenica_tbl %>% 
  mutate(title = str_remove(title, "\\.xml$")) %>%
  unite(title, c("title", "book")) %>% 
  group_by(title) %>% 
  mutate(text = str_c(text, collapse = " ")) %>% 
  distinct(title, text)

# tidy hellenica
hellenica_words <- hellenica %>%
  unnest_tokens(word, text)

#save(hellenica_words, file = "./data/hellenica.Rdata")

