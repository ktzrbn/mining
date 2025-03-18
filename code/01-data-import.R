# Load required libraries
library(gutenbergr)
library(dplyr)
library(stringr)

# Get the Gutenberg metadata
gb_metadata <- gutenberg_works()

# Define search terms related to British Empire
search_terms <- c("india", "colony", "colonial", "empire", "africa", "asia", 
                  "imperial", "natives", "british", "england", "victoria", 
                  "trade", "east india", "conquest")

# First approach: Find works with publication dates in the 19th century where possible
# Many Gutenberg works have no publication dates
dated_works <- gb_metadata %>%
  filter(
    language == "en",
    !is.na(gutenberg_author_id),
    !str_detect(title, "Bible|Dictionary|Encyclopedia|Manual|Cookbook")
  ) %>%
  # But some do, so let's make sure we get those 
  filter(
    !is.na(gutenberg_bookshelf),
    str_detect(gutenberg_bookshelf, "1800|19th")
  )

# Second approach: Find popular authors from the 19th century
empire_authors <- c("Rudyard Kipling", "Joseph Conrad", "Charles Dickens", 
                    "H. Rider Haggard", "Robert Louis Stevenson", "Anthony Trollope", 
                    "E.M. Forster", "John Stuart Mill", "Thomas Macaulay", 
                    "Thomas Babington Macaulay", "James Mill", "George Curzon",
                    "Frederick Lugard", "Richard Burton", "David Livingstone",
                    "Henry Morton Stanley", "Mary Kingsley", "Flora Annie Steel")

author_works <- gb_metadata %>%
  filter(
    language == "en",
    str_detect(author, paste(empire_authors, collapse = "|"))
  )

# Third approach: Keyword search in titles
keyword_works <- gb_metadata %>%
  filter(
    language == "en",
    str_detect(tolower(title), paste(search_terms, collapse = "|"))
  )

# Combine all of the above approaches into one dataset
empire_works <- bind_rows(dated_works, author_works, keyword_works) %>% 
  distinct() %>%
  # Use author birth/death dates to try and estimate 19th century works
  left_join(gutenberg_authors, by = "gutenberg_author_id") %>%
  filter(
    # Authors who lived during the 19th century (possibly born earlier)
    (is.na(birthdate) | 
       birthdate <= 1880) & # Born before or during most of the 19th century
      (is.na(deathdate) | 
         deathdate >= 1800)   # Died after the 19th century began
  )

# View how many books we found
print(paste("Found", nrow(empire_works), "potentially relevant works"))

# Preview the first few works
head(empire_works %>% select(gutenberg_id, title, author.x), 20)

# If we have more than 1000 works, we can limit to the most relevant
# I'm going to cap this at 1000 works, but feel freee to use a lower number if you prefer
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
# Uncomment the two lines below when ready to download
empire_texts <- gutenberg_download(empire_works$gutenberg_id, 
                                  meta_fields = c("title", "author"))

# Take a quick look at the dataset to get a sense of how it's organized  
View(empire_texts) 

# You might want to save the metadata for future reference. 
# write.csv(empire_works %>% select(gutenberg_id, title, author.x), 
          #"empire_corpus_metadata.csv", row.names = FALSE)

