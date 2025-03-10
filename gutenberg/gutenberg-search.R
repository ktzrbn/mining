# Install required packages
install.packages(c("gutenbergr", "tidytext", "dplyr", "stringr", 
                   "tidyr", "ggplot2", "topicmodels", "tm"))

# Load libraries
library(gutenbergr)
library(tidytext)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(topicmodels)
library(tm)

# Get the Gutenberg metadata
gb_metadata <- gutenberg_works()

# Define search terms related to British Empire
search_terms <- c("india", "colony", "colonial", "empire", "africa", "asia", 
                  "imperial", "natives", "british", "england", "victoria", 
                  "trade", "east india")

# First approach: Find works with publication dates in the 19th century
dated_works <- gb_metadata %>%
  filter(
    language == "en",
    !is.na(gutenberg_author_id),
    !str_detect(title, "Bible|Dictionary|Encyclopedia|Manual|Cookbook")
  ) %>%
  # Some Gutenberg works have publication year info
  filter(
    !is.na(gutenberg_bookshelf),
    str_detect(gutenberg_bookshelf, "1800|19th")
  )

keyword_works <- gb_metadata %>%
  filter(
    language == "en",
    str_detect(tolower(title), paste(search_terms, collapse = "|"))
  )

# Combine all approaches
empire_works <- bind_rows(dated_works, keyword_works) 

print(paste("Found", nrow(empire_works), "potentially relevant works"))

head(empire_works %>% select(gutenberg_id, title, author), 20)

if(nrow(empire_works) > 1000) {
  # Calculate a relevance score based on how many search terms appear in the title
  empire_works <- empire_works %>%
    mutate(
      relevance_score = sapply(title, function(t) {
        sum(sapply(search_terms, function(term) {
          if(str_detect(tolower(t), term)) 1 else 0
        }))
      })
    ) %>%
    arrange(desc(relevance_score)) %>%
    head(1000)
}

# Download the corpus (this will take time)
# Uncomment when ready to download
empire_texts <- gutenberg_download(empire_works$gutenberg_id, 
                                   meta_fields = c("title", "author"))
