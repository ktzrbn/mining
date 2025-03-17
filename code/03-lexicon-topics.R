install.packages("plotly")

library(dplyr)
library(stringr)
library(stringi)
library(ggplot2)
library(ggridges)
library(tidyr)
library(plotly)

# Define the sentiment dictionary
imperial_sentiment <- list(
  positive = c("civilize", "progress", "improve", "develop", "enlighten", "order", "duty"),
  negative = c("savage", "barbaric", "primitive", "wild", "uncivilized", "backwards"),
  dominance = c("rule", "govern", "control", "command", "master", "subject", "obey"),
  resistance = c("resist", "rebel", "uprising", "mutiny", "independence", "freedom")
)

# Apply dictionary to texts
imperial_attitude <- empire_texts %>%
  filter(gutenberg_id != 1470) %>%
  group_by(gutenberg_id, title) %>%
  summarize(text = paste(text, collapse = " ")) %>%
  mutate(
    text = iconv(stri_enc_toutf8(text, is_unknown_8bit = TRUE), to = "UTF-8", sub = ""),
    text_length = nchar(text, type = "chars", allowNA = TRUE),
    text_length = ifelse(is.na(text_length) | text_length == 0, 1, text_length)
  )

# Add attitude columns
for (attitude in names(imperial_sentiment)) {
  terms <- imperial_sentiment[[attitude]]
  pattern <- paste(terms, collapse = "|")
  counts <- tryCatch(
    str_count(tolower(imperial_attitude$text), pattern),
    error = function(e) {
      print(paste("Error in str_count for", attitude, ":", e$message))
      rep(0, length(imperial_attitude$text))
    }
  )
  imperial_attitude[[attitude]] <- counts / imperial_attitude$text_length * 10000
}

# Reshape into long format
imperial_attitude_long <- imperial_attitude %>%
  pivot_longer(
    cols = names(imperial_sentiment),
    names_to = "attitude",
    values_to = "score"
  ) %>%
  select(gutenberg_id, title, attitude, score)

# Debugging: Inspect the data
print("Summary of scores:")
print(summary(imperial_attitude_long$score))
print("Rows with NA, NaN, or Inf scores:")
print(imperial_attitude_long[!is.finite(imperial_attitude_long$score), ])
print("Unique scores by attitude:")
print(imperial_attitude_long %>% group_by(attitude) %>% summarise(unique_scores = list(unique(score))))

# Filter out problematic scores and create the plot
imperial_attitude_long_clean <- imperial_attitude_long %>%
  filter(is.finite(score))  # Remove NA, NaN, Inf

# Create the ridgeline plot
ggplot(imperial_attitude_long_clean, aes(x = score, y = attitude, fill = attitude)) +
  geom_density_ridges(alpha = 0.6, scale = 1.5) +
  facet_wrap(~title, scales = "free_y", ncol = 3) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Distribution of Imperial Attitudes Across Texts",
    subtitle = "Normalized frequency of sentiment terms per 10,000 characters",
    x = "Score (per 10,000 characters)",
    y = "Attitude"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Interactive scatter plot 

library(plotly)

# Create the scatter plot
p <- imperial_attitude_long %>%
  ggplot(aes(x = attitude, y = score, color = title, size = text_length)) +
  geom_jitter(width = 0.2, alpha = 0.7) +
  scale_size(range = c(2, 10)) +  # Size points by text length
  labs(
    title = "Imperial Attitudes by Text",
    x = "Attitude",
    y = "Score (per 10,000 characters)",
    color = "Title",
    size = "Text Length"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Make it interactive
plotly_plot <- ggplotly(p, tooltip = c("title", "score", "text_length"))
plotly_plot
