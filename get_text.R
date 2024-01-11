## function gets page and text from direct dialogues

library(purrr)
library(XML)
library(dplyr)

# get page function 
get_page <- function(paragraph) {
  xmlGetAttr(xmlParent(paragraph), "n")
}

# main function
get_text <- function(file_name) {
  file_path <-  paste0("PerseusXML/", file_name)
  doc <- xmlTreeParse(file_path, useInternalNodes = T, isURL = FALSE)
  paragraphs <- getNodeSet(doc, 
                           "//tei:div//tei:p",
                           namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))
  pages <- map_chr(paragraphs, get_page)
  text <- map_chr(paragraphs, xmlValue)
  dialogue_tbl <- tibble(title = file_name, 
                         page = pages, 
                         text = text)
  return(dialogue_tbl)
  }
