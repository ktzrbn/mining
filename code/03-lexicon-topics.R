# Load required packages
library(dplyr)
library(tidytext)
library(ggplot2)
library(stringr)
library(textdata)
library(tidyr)

# Some basic text mining features

# Filter for a specific author in the corpus and tokenize text into words. Here
# I've used Kipling 
word_freq <- empire_texts %>%
  filter(author == "Kipling, Rudyard") %>%
  unnest_tokens(word, text) %>%
  # Remove stop words and non-alphabetic characters
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "^[a-z]+$")) %>%
  # Count word frequencies
  count(word, sort = TRUE) %>%
  top_n(10, n)

# Create bar graph
ggplot(word_freq, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Words by Rudyard Kipling",
       x = "Words", y = "Frequency") +
  theme_minimal()

# Second analysis 
# Filter for a specific author and tokenize into bigrams
bigram_freq <- empire_texts %>%
  filter(author == "Haggard, H. Rider (Henry Rider)") %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  # Remove rows with NA bigrams
  filter(!is.na(bigram)) %>%
  # Separate bigrams into two words to filter stop words
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  # Recombine into bigrams and clean
  unite(bigram, word1, word2, sep = " ") %>%
  # Count bigram frequencies
  count(bigram, sort = TRUE) %>%
  top_n(10, n)

# Create bar plot
ggplot(bigram_freq, aes(x = reorder(bigram, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(title = "Top 10 Bigrams by H. Rider Haggard",
       x = "Bigrams", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

# Third analysis
# This analysis selects 4 texts at random and shows their top five distinctive words
# Sample 4 titles for analysis (adjust as needed)
# Changing the set.seed number will change the random titles generated
set.seed(279)
sample_titles <- empire_texts %>%
  distinct(title) %>%
  slice_sample(n = 4) %>%
  pull(title)

# Filter, tokenize, and calculate TF-IDF
tfidf_data <- empire_texts %>%
  filter(title %in% sample_titles) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  count(title, word, name = "n") %>%
  bind_tf_idf(word, title, n) %>%
  # Get top 5 words per title by TF-IDF
  group_by(title) %>%
  slice_max(order_by = tf_idf, n = 5, with_ties = FALSE) %>%
  ungroup()

# Create faceted bar plot
ggplot(tfidf_data, aes(x = reorder_within(word, tf_idf, title), y = tf_idf)) +
  geom_bar(stat = "identity", fill = "purple") +
  facet_wrap(~title, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top 5 Distinctive Words by Title (TF-IDF)",
       x = "Words", y = "TF-IDF Score") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

