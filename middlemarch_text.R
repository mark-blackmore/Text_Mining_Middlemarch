#' ---
#' title: Text Mining - Middlemarch v. Wuthering Heights  
#' author: "Mark Blackmore"  
#' date: "`r format(Sys.Date())`"  
#' output: 
#'   github_document:
#'     toc: true
#'     
#' ---

suppressWarnings(
  suppressPackageStartupMessages({
    library(gutenbergr)
    library(tidyverse)
    library(tidytext)
    library(tidyr)
    library(stringr)
  })
)

#' ### Download books from Project Gutenberg: Middlemarch & Wuthering Heights 
id <- gutenberg_works(title %in% c("Middlemarch", "Wuthering Heights"))
middlemarch <- gutenberg_download(id[1,1])
wuthering_heights <- gutenberg_download(id[2,1])

#' ### Tidy book text and remove stopwords  
tidy_middlemarch <- middlemarch %>% 
  unnest_tokens(word, text)
tidy_middlemarch

tidy_wuthering <- wuthering_heights %>% 
  unnest_tokens(word, text)
tidy_wuthering

# Remove stopwords  
tidy_middlemarch <- tidy_middlemarch %>%
  anti_join(stop_words)

tidy_wuthering <- tidy_wuthering %>%
  anti_join(stop_words)

#' ### Most common words

# Middlemarch
tidy_middlemarch %>%
  count(word, sort = TRUE) %>%
  filter(n > 250) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Wuthering Heights
tidy_wuthering %>%
  count(word, sort = TRUE) %>%
  filter(n > 110) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#' ### Calculating & visualizing net sentiment

tidy_middlemarch <- tidy_middlemarch %>%
  mutate(linenumber = row_number())

tidy_middlemarch %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 70, sentiment) %>%
  # Spread sentiment and n across multiple columns
  spread(sentiment, n, fill = 0) %>%
  # Use mutate to find net sentiment
  mutate(sentiment = positive - negative) %>%
  # Put index on x-axis, sentiment on y-axis
  ggplot(aes(index, sentiment)) +
  # Make a bar chart with geom_col()
  geom_col()
 
tidy_wuthering <- tidy_wuthering %>%
  mutate(linenumber = row_number())

tidy_wuthering %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 70, sentiment) %>%
  # Spread sentiment and n across multiple columns
  spread(sentiment, n, fill = 0) %>%
  # Use mutate to find net sentiment
  mutate(sentiment = positive - negative) %>%
  # Put index on x-axis, sentiment on y-axis
  ggplot(aes(index, sentiment)) +
  # Make a bar chart with geom_col()
  geom_col()