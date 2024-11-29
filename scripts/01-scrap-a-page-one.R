## load packages ----------------------------------------------------------------
#
library(tidyverse)
library(rvest)
#
## set url ----------------------------------------------------------------------
#
first_url <- "https://collections.ed.ac.uk/art/search/*:*/Collection:%22edinburgh+college+of+art%7C%7C%7CEdinburgh+College+of+Art%22?offset=0"
#
## read first page --------------------------------------------------------------
#
page <- read_html(first_url)
#
## scrape titles "record-title"----------------------------------------------------------------
#
# your code goes here
first_title_class <- ".record-title"
titles <- page %>% html_nodes(first_title_class) %>% html_text() %>% str_replace_all("\\s+", " ")
#
## scrape links -----------------------------------------------------------------
#https://collections.ed.ac.uk/art/record/21236?highlight=*:*
#href="./record/21236?highlight=*:*"
#https://collections.ed.ac.uk/art/
#
# your code goes here
#
full_link <- "https://collections.ed.ac.uk/art"
first_link_class <- ".record-title a"
links <- page %>% html_nodes(first_link_class) %>% html_attr("href") %>%
  str_replace("^\\.", "") %>% str_c(full_link, .)
## scrape artists ---------------------------------------------------------------
#
process_iteminfo_node <- function(node) {
  artist_nodes <- html_nodes(node, ".artist")
  
  if (length(artist_nodes) > 0) {
    artist_names <- html_text(artist_nodes) %>% 
      str_replace_all("\\s+", " ")
    return(str_c(artist_names, sep = ", "))
  } else {
    return("")
  }
}

artists <- page %>%
  html_nodes(first_artist_class) %>%
  purrr::map_chr(~ process_iteminfo_node(.))


# your code goes here
#
## put together in a data frame -------------------------------------------------
#
first_ten <- data.frame(Title = titles, Artist = artists, Link = links)


## scrape second ten paintings --------------------------------------------------
#
## set url ----------------------------------------------------------------------
#
second_url <- "https://collections.ed.ac.uk/art/search/*:*/Collection:%22edinburgh+college+of+art%7C%7C%7CEdinburgh+College+of+Art%22?offset=10"
#
## read first page --------------------------------------------------------------
#
second_page <- read_html(second_url)
#
## scrape titles "record-title"----------------------------------------------------------------
#
# your code goes here
second_title_class <- ".record-title"
titles <- second_page %>% html_nodes(second_title_class) %>% html_text() %>% str_replace_all("\\s+", " ")
#
## scrape links -----------------------------------------------------------------
#https://collections.ed.ac.uk/art/record/21236?highlight=*:*
#href="./record/21236?highlight=*:*"
#https://collections.ed.ac.uk/art/
#
# your code goes here
#
full_link <- "https://collections.ed.ac.uk/art"
second_link_class <- ".record-title a"
links <- second_page %>% html_nodes(second_link_class) %>% html_attr("href") %>%
  str_replace("^\\.", "") %>% str_c(full_link, .)
## scrape artists ---------------------------------------------------------------
#
process_iteminfo_node <- function(node) {
  artist_nodes <- html_nodes(node, ".artist")
  
  if (length(artist_nodes) > 0) {
    artist_names <- html_text(artist_nodes) %>% 
      str_replace_all("\\s+", " ")
    return(str_c(artist_names, sep = ", "))
  } else {
    return("")
  }
}

artists <- second_page %>%
  html_nodes(first_artist_class) %>%
  purrr::map_chr(~ process_iteminfo_node(.))


# your code goes here
#
## put together in a data frame -------------------------------------------------
#
second_ten <- data.frame(Title = titles, Artist = artists, Link = links)
