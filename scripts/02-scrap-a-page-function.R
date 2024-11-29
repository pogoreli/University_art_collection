library(tidyverse)
library(rvest)

scrap_page <- function(url) {
  page <- read_html(url)
  title_class <- ".record-title"
  
  titles <- page %>% html_nodes(title_class) %>% html_text() %>% str_replace_all("\\s+", " ")

  process_iteminfo_node <- function(node) {
    artist_nodes <- html_nodes(node, ".artist")
    
    if (length(artist_nodes) > 0) {
      # Extract artist names, clean up whitespace for each individually
      artist_names <- html_text(artist_nodes) %>%
        str_replace_all("\\s+", " ") %>%
        trimws() # Ensure to trim leading and trailing whitespaces
      
      # Now concatenate the cleaned names
      concatenated_artist_names <- str_c(artist_names, collapse = ", ")
      
      return(concatenated_artist_names)
    } else {
      return("")
    }
  }
  
  artists <- page %>%
    html_nodes(first_artist_class) %>%
    purrr::map_chr(~ process_iteminfo_node(.))
  
  full_link <- "https://collections.ed.ac.uk/art"
  link_class <- ".record-title a"
  links <- page %>% html_nodes(link_class) %>% html_attr("href") %>%
    str_replace("^\\.", "") %>% str_c(full_link, .)

  
  df <- data.frame(Title = titles, Artist = artists, Link = links)
  return(df)
}

data <- scrap_page("https://collections.ed.ac.uk/art/search/*:*/Collection:%22edinburgh+college+of+art%7C%7C%7CEdinburgh+College+of+Art%22?offset=380")
