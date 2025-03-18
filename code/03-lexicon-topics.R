# Load required packages
library(dplyr)
library(tidytext)
library(ggplot2)
library(stringr)
library(textdata)

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

# Create bar plot
ggplot(word_freq, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Words by Rudyard Kipling",
       x = "Words", y = "Frequency") +
  theme_minimal()

# Sentiment analysis over titles 
# Sample the dataset to 5 unique titles for efficiency (adjust as needed)
set.seed(123)  # For reproducibility
sample_titles <- gutenberg_data %>%
  distinct(title) %>%
  slice_sample(n = 5) %>%
  pull(title)

# Filter dataset to sampled titles, tokenize, and join with AFINN sentiment lexicon
sentiment_data <- empire_texts %>%
  slice(1:100) %>%  # Take first 100 rows
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(title) %>%
  summarise(avg_sentiment = mean(value, na.rm = TRUE)) %>%
  ungroup()

# Create bar plot
ggplot(sentiment_data, aes(x = reorder(title, avg_sentiment), y = avg_sentiment, fill = avg_sentiment > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Average Sentiment Score by Title",
       x = "Title", y = "Average Sentiment (AFINN)") +
  scale_fill_manual(values = c("red", "green"), guide = FALSE) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

# Load required packages
library(dplyr)
library(tidytext)
library(ggplot2)

# third analysis 
# Sample 4 titles for analysis (adjust as needed)
set.seed(123)
sample_titles <- gutenberg_data %>%
  distinct(title) %>%
  slice_sample(n = 4) %>%
  pull(title)

# Filter, tokenize, and calculate TF-IDF
tfidf_data <- gutenberg_data %>%
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

