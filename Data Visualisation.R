library(dplyr)
library(tidyr)
library(ggplot2)

# Load data
akas <- read.delim("data/title.akas.tsv", sep = "\t", header = TRUE, stringsAsFactors = FALSE, na.strings = "\\N")
basics <- read.delim("data/title.basics.tsv", sep = "\t", header = TRUE, stringsAsFactors = FALSE, na.strings = "\\N")
ratings <- read.delim("data/title.ratings.tsv", sep = "\t", header = TRUE, stringsAsFactors = FALSE, na.strings = "\\N")

# Merge datasets
merged_data <- akas %>%
  inner_join(basics, by = c("titleId" = "tconst")) %>%
  inner_join(ratings, by = c("titleId" = "tconst"))

# Remove duplicates (keep ordering = 1)
merged_data <- merged_data %>%
  filter(ordering == 1)

# Filter data to include only movies from 2015–2024
filtered_data <- merged_data %>%
  filter(startYear >= 2015 & startYear <= 2024, 
         titleType == "movie", 
         ordering == 1) %>%         # Ensure unique entries
  separate_rows(genres, sep = ",") %>%  # Split combined genres
  filter(genres %in% c("Action", "Adventure", "Animation", "Comedy", "Drama", 
                       "Fantasy", "Horror", "Romance", "Sci-Fi", "Thriller")) %>% 
  filter(!is.na(runtimeMinutes), 
         !is.na(averageRating), 
         !is.na(numVotes))         # Remove rows with missing values

Confidence Score Calculation

# Calculate Confidence Score
filtered_data <- filtered_data %>%
  mutate(confidence_score = (averageRating * numVotes) / max(numVotes, na.rm = TRUE))

# Normalise Confidence Score for comparison
filtered_data <- filtered_data %>%
  mutate(confidence_score_normalised = confidence_score / max(confidence_score, na.rm = TRUE))

Data Aggregation for Visualisations

# Data for Faceted Bar Chart
faceted_data <- filtered_data %>%
  group_by(genres) %>%
  summarise(
    avg_confidence = mean(confidence_score, na.rm = TRUE),
    avg_rating = mean(averageRating, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(avg_confidence, avg_rating), 
               names_to = "metric", values_to = "value")

# Data for Heatmap
heatmap_data <- filtered_data %>%
  group_by(genres) %>%
  summarise(
    norm_confidence = mean(confidence_score_normalised, na.rm = TRUE),
    norm_rating = mean(averageRating / 10, na.rm = TRUE),  # Normalise Ratings to (0–1)
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(norm_confidence, norm_rating), 
               names_to = "metric", values_to = "value")

# Data for Line Graph
trends_data <- filtered_data %>%
  group_by(startYear, genres) %>%
  summarise(avg_confidence = mean(confidence_score, na.rm = TRUE), 
            .groups = "drop")

# Bubble Chart Data (uses filtered_data directly)
Visualisation Code

#Faceted Bar Chart
ggplot(faceted_data, aes(x = reorder(genres, -value), y = value, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~metric, scales = "free_y") +
  scale_fill_manual(values = c("avg_confidence" = "#377eb8", "avg_rating" = "#ff7f00")) +
  labs(
    title = "Confidence Score and Average Rating by Genre",
    x = "Genre",
    y = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Heatmap
ggplot(heatmap_data, aes(x = genres, y = metric, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b", name = "Value (0–1)") +
  labs(
    title = "Heatmap of Normalised Confidence Scores and Average Ratings by Genre",
    x = "Genre",
    y = "Metric",
    fill = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Line Graph
ggplot(trends_data, aes(x = startYear, y = avg_confidence, color = genres, group = genres)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Trends in Confidence Scores by Genre (2015–2024)",
    x = "Year",
    y = "Confidence Score",
    color = "Genre"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45)
  )
#Bubble Chart
ggplot(filtered_data, aes(x = confidence_score, y = averageRating, size = numVotes, color = genres)) +
  geom_point(alpha = 0.6) +
  scale_size(range = c(1, 10), name = "Number of Votes") +
  labs(
    title = "Confidence Score vs Average Rating with Number of Votes",
    x = "Confidence Score",
    y = "Average Rating",
    color = "Genre"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

