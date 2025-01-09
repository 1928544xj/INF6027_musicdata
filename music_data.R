# Load libraries
library(tidyverse)
library(caret)
library(ggplot2)
library(cluster)
library(factoextra)
library(corrplot)
library(tidyr)
library(lubridate)
library(topicmodels)
library(tm)
library(wordcloud)
library(wordcloud2)

# Load datasets
# Metadata files
metadata <- list(
  albums = read_csv("C:/Data-coursework/INF4000- Data visualization- 20-12/music data/musicoset_metadata/musicoset_metadata/albums.csv"),
  artists = read_csv("C:/Data-coursework/INF4000- Data visualization- 20-12/music data/musicoset_metadata/musicoset_metadata/artists.csv"),
  songs = read_csv("C:/Data-coursework/INF4000- Data visualization- 20-12/music data/musicoset_metadata/musicoset_metadata/songs.csv"),
  track = read_csv("C:/Data-coursework/INF4000- Data visualization- 20-12/music data/musicoset_metadata/musicoset_metadata/tracks.csv"),
  release = read_csv("C:/Data-coursework/INF4000- Data visualization- 20-12/music data/musicoset_metadata/musicoset_metadata/releases.csv")
  )

# Clean column names in metadata$songs
metadata$songs <- metadata$songs %>%
  rename_with(~ make.names(.))  # Standardize column names

metadata$artists <- metadata$artists %>%
  rename_with(~ make.names(.))  # Standardize column names

metadata$artists <- metadata$artists %>%
  separate(
    col = "artist_id.name.followers.popularity.artist_type.main_genre.genres.image_url",
    into = c("artist_id", "name", "followers", "popularity", "artist_type", "main_genre", "genres", "image_url"),
    sep = "\t"
  )

metadata$track <- metadata$track %>%
  rename_with(~ make.names(.))  # Standardize column names

metadata$track <- metadata$track %>%
  separate(
    col = "song_id.album_id.track_number.release_date.release_date_precision",
    into = c("song_id","album_id","track_number","release_date","release_date_precision"),
    sep = "\t"
  )

metadata$release <- metadata$release %>%
  rename_with(~ make.names(.))  # Standardize column names
metadata$release <- metadata$release %>%
  separate(
    col = "artist_id.album_id.release_date.release_date_precision",
    into = c("artist_id","album_id", "release_date", "release_date_precision"),
    sep = "\t"
  )

colnames(metadata$artists)
colnames(metadata$track)
colnames(metadata$release)
head(metadata$artists)

# Song features
songs_features <- read_delim("C:/Data-coursework/INF4000- Data visualization- 20-12/music data/musicoset_songfeatures/musicoset_songfeatures/acoustic_features.csv", 
                             delim = "\t", 
                             col_names = c("song_id", "duration_ms", "key", "mode", "time_signature", "acousticness", "danceability", "energy", "instrumentalness", "liveness", "loudness", "speechiness", "valence", "tempo"))


# Load and process lyrics dataset
# Clean lyrics dataset
lyrics <- read_delim("C:/Data-coursework/INF4000- Data visualization- 20-12/music data/musicoset_songfeatures/musicoset_songfeatures/lyrics.csv", delim = "\t", col_names = FALSE) %>%
  rename(raw_first_column = X1) %>%  # Rename the first column for clarity
  separate(raw_first_column, into = c("song_id", "first_line"), sep = ",", extra = "merge", fill = "right") %>% 
  mutate(
    lyrics = apply(select(., starts_with("X")), 1, paste, collapse = " "),  # Combine unnamed lyric columns
    lyrics = paste(first_line, lyrics, sep = " ")  # Add first line to lyrics
  ) %>%
  mutate(
    lyrics = str_replace_all(lyrics, "\\n", " "),   # Replace newlines with spaces
    lyrics = str_trim(lyrics)                      # Trim leading/trailing spaces
  ) %>%
  select(song_id, lyrics)  # Keep only song_id and the aggregated lyrics


lyrics <- lyrics %>%
  # Clean the existing lyrics column
  mutate(
    lyrics = str_replace_all(lyrics, "NA", ""),  # Remove redundant 'NA' strings
    lyrics = str_replace_all(lyrics, '\\n', ' '),  # Replace newlines with spaces
    lyrics = str_replace_all(lyrics, '\"', ''),       # Remove escaped quotes
    lyrics = str_trim(lyrics)                            # Trim leading/trailing whitespace
  ) %>%
    # Remove the first row if it contains column headers
  filter(song_id != "song_id")
lyrics = apply(select(., starts_with("X")), 1, paste, collapse = " ")
  select(song_id, lyrics) %>%  # Keep only song_id and the aggregated lyrics
  mutate(
    lyrics = str_replace_all(lyrics, "\\n", " "),   # Replace newlines with spaces
    lyrics = str_replace_all(lyrics, '\\"', ''),     # Remove escaped quotes
    lyrics = str_trim(lyrics)                          # Trim leading/trailing whitespace
  )

# Popularity
popularity <- read_delim("C:/Data-coursework/INF4000- Data visualization- 20-12/music data/musicoset_popularity/musicoset_popularity/song_pop.csv", 
                         delim = "\t", 
                         col_names = c("id", "year_end_score", "is_pop", "year"))
#Merge

# Verify column names
cat("songs_features columns: ", colnames(songs_features), "\n")
cat("popularity columns: ", colnames(popularity), "\n")
cat("metadata$songs columns: ", colnames(metadata$songs), "\n")
cat("lyrics columns: ", colnames(lyrics), "\n")

# Rename `id` to `song_id` in `popularity`
popularity <- popularity %>%
  rename(song_id = id)

metadata$songs <- metadata$songs %>%
  rename_with(~ make.names(.)) %>%
  separate(colnames(metadata$songs)[1], 
           into = c("song_id", "song_name", "billboard", "artists", "popularity", "explicit", "song_type"), sep = "\t")

lyrics <- lyrics %>% rename(song_id = song_id)  

songs_data <- songs_features %>%
  left_join(popularity, by = "song_id")  # Check if this step works
cat("After joining with popularity: ", nrow(songs_data), " rows\n")

songs_data <- songs_data %>%
  left_join(metadata$songs, by = "song_id")  # Check if this step works
cat("After joining with metadata$songs: ", nrow(songs_data), " rows\n")

songs_data <- songs_data %>%
  left_join(lyrics, by = "song_id")  # Check if this step works
cat("After joining with lyrics: ", nrow(songs_data), " rows\n")

songs_data <- songs_data[-1, ]  # Remove the first row

str(metadata$artists)
str(metadata$release)
str(metadata$track)

artist_releases <- metadata$artists %>%
  select(artist_id, main_genre) %>%
  inner_join(metadata$release, by = "artist_id")

artist_tracks <- artist_releases %>%
  inner_join(metadata$track, by = "album_id")

songs_with_genre <- artist_tracks %>%
  inner_join(metadata$songs, by = "song_id") %>%
  select(song_id, main_genre) %>%
  distinct()

# Descriptive Statistics
summary(songs_data)

# Identify numeric and categorical columns
numeric_columns <- c("duration_ms", "key", "mode", "time_signature", "acousticness", 
                     "danceability", "energy", "instrumentalness", "liveness", 
                     "loudness", "speechiness", "valence", "tempo", "year_end_score", "year")

categorical_columns <- setdiff(colnames(songs_data), numeric_columns)

# Convert columns 
songs_data <- songs_data %>%
  mutate(across(all_of(numeric_columns), ~ as.numeric(as.character(.)), .names = "{.col}")) %>%  # Convert numeric columns
  mutate(across(all_of(categorical_columns), ~ as.factor(.), .names = "{.col}"))  # Convert categorical columns

# Step 3: Verify data types
str(songs_data)
colnames(songs_data) <- make.names(colnames(songs_data))

numeric_columns <- songs_data %>%
  select(where(is.numeric))

numeric_summary <- numeric_columns %>%
  summarise(
    across(
      everything(),
      list(
        Min = ~min(., na.rm = TRUE),
        Q1 = ~quantile(., 0.25, na.rm = TRUE),
        Median = ~median(., na.rm = TRUE),
        Mean = ~mean(., na.rm = TRUE),
        Q3 = ~quantile(., 0.75, na.rm = TRUE),
        Max = ~max(., na.rm = TRUE)
      )
    )
  )
# Adjust column names
colnames(numeric_summary) <- colnames(numeric_summary) %>%
  str_replace_all("duration_ms", "duration.ms") %>%
  str_replace_all("time_signature", "time.signature") %>%
  str_replace_all("year_end_score", "year.end.score") %>%
  make.names()


numeric_summary_cleaned <- numeric_summary %>%
  select(where(is.numeric)) %>% # Include only numeric columns
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Statistic"),
    names_sep = "_",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = "Statistic",
    values_from = "Value"
  )


print("Reshaped Numeric Summary:")
print(numeric_summary_cleaned)

write.csv(numeric_summary, "numeric_summary_cleaned.csv", row.names = FALSE)


# Descriptive analysis 
categorical_summary <- songs_data %>%
  select(all_of(categorical_columns)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Category"
  ) %>%
  group_by(Variable, Category) %>%
  summarise(
    Count = n(),
    Percentage = (n() / nrow(songs_data)) * 100,
    .groups = "drop"
  ) %>%
  arrange(Variable, desc(Count))

print("Categorical Summary:")
print(categorical_summary)


# Visualizations

# 1. Histogram for Numeric Variables
# List of numeric columns
numeric_columns <- c("duration_ms", "key", "mode", "time_signature", "acousticness", 
                     "danceability", "energy", "instrumentalness", "liveness", 
                     "loudness", "speechiness", "valence", "tempo", "year_end_score", "year")

# Function to plot and display histogram
plot_histogram <- function(data, column) {
  p <- ggplot(data, aes_string(x = column)) +
    geom_histogram(binwidth = 30, fill = "blue", alpha = 0.7, color = "black") +
    labs(
      title = paste("Distribution of", column),
      x = column,
      y = "Frequency"
    ) +
    theme_minimal()
  print(p)  # Display the plot
}

# Plot each histogram interactively
for (col in numeric_columns) {
  message(paste("Plotting histogram for:", col))  # Inform about the current plot
  plot_histogram(songs_data, col)
  Sys.sleep(1)  # Pause to allow the user to view the plot before proceeding
}


# 2. Bar Chart for Categorical Variables
# Filter top N categories 
top_n <- 10
categorical_summary_top <- categorical_summary %>%
  group_by(Variable) %>%
  slice_max(order_by = Count, n = top_n) %>%
  ungroup()

# Plot bar charts for each variable
for (var in unique(categorical_summary_top$Variable)) {
  # Filter data 
  data <- categorical_summary_top %>% filter(Variable == var)
  
  # Generate the plot
  plot <- ggplot(data, aes(x = reorder(Category, -Count), y = Count)) +
    geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
    labs(
      title = paste("Top", "Categories for", var),
      x = var,
      y = "Count"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(plot)
}

# Filter out hit songs
unique(songs_data$is_pop)
songs_data <- songs_data %>%
  mutate(is_pop = as.logical(as.character(is_pop)))

hit_songs <- songs_data %>% 
  filter(is_pop == TRUE)

hit_songs <- hit_songs %>%
  mutate(popularity = as.numeric(as.character(popularity)))

popularity_summary <- hit_songs %>%
  group_by(song_type) %>%
  summarise(
    Mean_Popularity = mean(popularity, na.rm = TRUE),
    Median_Popularity = median(popularity, na.rm = TRUE),
    Min_Popularity = min(popularity, na.rm = TRUE),
    Max_Popularity = max(popularity, na.rm = TRUE),
    Count = n()
  )

print(popularity_summary)

# t-test for popularity between solo and collaboration songs
t_test_result <- t.test(
  hit_songs$popularity[hit_songs$song_type == "Solo"],
  hit_songs$popularity[hit_songs$song_type == "Collaboration"],
  alternative = "two.sided",
  var.equal = FALSE  # Assume unequal variances
)

print("T-test Results:")
print(t_test_result)

# Visualize the distribution of popularity 
ggplot(hit_songs, aes(x = song_type, y = popularity, fill = song_type)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Popularity Comparison for Hit Songs by Song Type",
    x = "Song Type",
    y = "Popularity"
  ) +
  theme_minimal()


# 3. Correlation Heatmap
numeric_data <- songs_data %>%
  select(all_of(numeric_columns)) %>%
  na.omit()

cor_matrix <- cor(numeric_data, use = "complete.obs")
corrplot(cor_matrix, method = "circle", tl.cex = 0.8)

# Correlation Matrix for Acoustic Features
acoustic_features <- songs_data %>% 
  select(danceability, energy, valence, tempo, acousticness, instrumentalness, liveness, loudness, speechiness)
corr_matrix <- cor(acoustic_features, use = "complete.obs")
corrplot(corr_matrix, method = "circle")


# Genre Analysis
genre_analysis_df <- artist_tracks %>%
  select(song_id, main_genre)

# Merge with songs_data 
genre_analysis_df <- genre_analysis_df %>%
  inner_join(songs_data, by = "song_id")


print(head(genre_analysis_df))

genre_distribution <- genre_analysis_df %>%
  count(main_genre) %>%
  mutate(percentage = (n / sum(n)) * 100)

ggplot(genre_distribution, aes(x = reorder(main_genre, -n), y = n)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Genre Distribution", x = "Genre", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Limit to top genres by count
top_genres <- genre_distribution %>%
  top_n(10, n) %>%  # Select top 10 genres
  arrange(desc(n))

# Plot the top genres
ggplot(top_genres, aes(x = reorder(main_genre, -n), y = n)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Top 10 Genre Distribution", x = "Genre", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Identify the top genres
top_5_genres <- genre_distribution %>%
  filter(main_genre != "-") %>%  # Exclude the "-" genre
  top_n(5, n) %>%
  pull(main_genre)

# Filter songs_data for only the top 5 genres
feature_analysis_data <- artist_tracks %>%
  inner_join(songs_data, by = "song_id") %>%
  filter(main_genre %in% top_5_genres) %>%
  group_by(main_genre) %>%
  summarise(
    avg_valence = mean(valence, na.rm = TRUE),
    avg_energy = mean(energy, na.rm = TRUE),
    avg_danceability = mean(danceability, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = avg_valence:avg_danceability,
    names_to = "Feature",
    values_to = "Average"
  )

#Pot the feature analysis for the top 5 genres
ggplot(feature_analysis_data, aes(x = main_genre, y = Average, fill = Feature)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Feature Analysis by Top 5 Genres", x = "Genre", y = "Average Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Hit vs Non-Hit Analysis
features_to_analyze <- c("danceability", "energy", "valence", "loudness", "speechiness", "tempo", "instrumentalness")

# Check if NA exists in is_pop
is_pop_levels <- unique(songs_data$is_pop)
is_pop_labels <- if (any(is.na(is_pop_levels))) {
  c("Non-Hit", "Hit", "NA")
} else {
  c("Non-Hit", "Hit")
}

# Generate the plot
# Generate the plot excluding NA
songs_data %>%
  filter(!is.na(is_pop)) %>%  
  group_by(is_pop) %>%
  summarise(across(all_of(features_to_analyze), ~ mean(.x, na.rm = TRUE), .names = "avg_{.col}")) %>%
  pivot_longer(cols = starts_with("avg_"), names_to = "feature", values_to = "value") %>%
  mutate(
    feature = gsub("avg_", "", feature),  
    is_pop = factor(is_pop, labels = c("Non-Hit", "Hit"))  # labels
  ) %>%
  ggplot(aes(x = is_pop, y = value, fill = is_pop)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  facet_wrap(~ feature, scales = "free_y", ncol = 3) +  
  labs(
    title = "Feature Comparison by Hit Status",
    x = "Hit Status",
    y = "Feature Average",
    fill = "Hit Status"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),  # Bold facet titles
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "bottom"
  )
# T-tests 
songs_data_filtered <- songs_data %>% filter(!is.na(is_pop))

t_test_results <- map_dfr(features_to_analyze, ~ {
  t_test <- t.test(songs_data_filtered[[.x]] ~ songs_data_filtered$is_pop)
  tibble(Feature = .x, p_value = t_test$p.value)
})
print(t_test_results)


# Logistic Regression
model1 <- glm(is_pop ~ danceability + energy + valence + tempo + acousticness + instrumentalness + liveness + loudness + speechiness,
             data = songs_data_clean, family = "binomial")
summary(model1)
library(rsample)
set.seed(123)
data_split <- initial_split(songs_data, prop = 0.7)
train_data <- training(data_split)
test_data <- testing(data_split)

model <- glm(is_pop ~ danceability + energy + valence + tempo + acousticness + instrumentalness + liveness + loudness + speechiness,
             data = train_data, family = "binomial")
summary(model)


# Predictions on test data
predicted_probabilities <- predict(model, test_data, type = "response")

# Convert probabilities to binary classifications (e.g., threshold = 0.5)
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)

# Calculate Residuals (difference between actual and predicted)
residuals <- as.numeric(test_data$is_pop) - predicted_probabilities

# Calculate RMSE
rmse <- sqrt(mean(residuals^2))
cat("Root Mean Square Error (RMSE):", rmse, "\n")

# Calculate Residual Standard Error (RSE)
# Note: RSE is typically for regression; here, residuals are adapted for classification probabilities.
rse <- sqrt(sum(residuals^2) / (nrow(test_data) - length(coef(model)) - 1))
cat("Residual Standard Error (RSE):", rse, "\n")

# Calculate Accuracy
accuracy <- mean(predicted_classes == test_data$is_pop)
cat("Accuracy:", accuracy, "\n")

# Create confusion matrix
confusion_matrix <- table(Predicted = predicted_classes, Actual = test_data$is_pop)
print("Confusion Matrix:")
print(confusion_matrix)

# Calculate additional metrics: Precision, Recall, and F1-Score
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
f1_score <- 2 * ((precision * recall) / (precision + recall))
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-Score:", f1_score, "\n")


# Add Cluster Labels to Data
songs_data <- songs_data %>% 
  mutate(cluster = clusters$cluster)

# Visualization: Clusters
library(ggplot2)

ggplot(songs_data, aes(x = danceability, y = energy, color = as.factor(cluster))) +
  geom_point(alpha = 0.6) +
  labs(title = "Clustering of Songs", x = "Danceability", y = "Energy", color = "Cluster") +
  theme_minimal()


# Preprocess Lyrics

# Define terms to remove from lyrics
remove_terms <- c(
  "verse 1", "verse 2", "verse 3", "verse", "chorus", 
  "bridge", "refrain", "intro", "outro", "hook", 
  "\\d+", # Remove standalone numbers
  "\\(.*?\\)" 
)

# Sentiment analysis

sentiment_lexicon <- get_sentiments("bing")

# Clean and tokenize the lyrics data

lyrics_clean <- lyrics %>%
  mutate(
    lyrics = str_to_lower(lyrics), # Convert to lowercase
    lyrics = str_remove_all(lyrics, paste(remove_terms, collapse = "|")), # Remove predefined terms
    lyrics = str_remove_all(lyrics, "[[:punct:]]"), # Remove punctuation
    lyrics = str_remove_all(lyrics, "\\b\\w{1}\\b"), # Remove single characters
    lyrics = str_squish(lyrics) # Remove extra whitespace
  ) %>%
  unnest_tokens(word, lyrics) %>% # Tokenize words
  anti_join(stop_words, by = "word") %>% # Remove stopwords
  inner_join(sentiment_lexicon, by = "word") 

# Create the sentiment column in lyrics_clean
# Ensure this step worked correctly
head(lyrics_clean)

# Separate positive and negative words
positive_words <- lyrics_clean %>%
  filter(sentiment == "positive") %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 20) # Top 20 positive words

negative_words <- lyrics_clean %>%
  filter(sentiment == "negative") %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 20) # Top 20 negative words

# Combine positive and negative words
combined_words <- bind_rows(
  positive_words %>% mutate(sentiment = "positive"),
  negative_words %>% mutate(sentiment = "negative")
)

print(combined_words)

combined_words <- combined_words %>%
  mutate(color = ifelse(sentiment == "positive", "green", "red"))

# Generate the word cloud
wordcloud2(
  data = combined_words %>% select(word, n), 
  size = 1,  
  color = combined_words$color,  
  backgroundColor = "white",
  shape = "circle"  # Use a circular layout
)

#Generate again with different color to match with other chart

combined_words <- combined_words %>%
  mutate(color = ifelse(sentiment == "positive", "blue", "red"))

# Generate the word cloud
wordcloud2(
  data = combined_words %>% select(word, n), 
  size = 1,  
  color = combined_words$color,  
  backgroundColor = "white",
  shape = "circle"  # Use a circular layout
)

### Topic modelling
# Prepare the data
positive_lyrics <- lyrics_clean %>% filter(sentiment == "positive")
negative_lyrics <- lyrics_clean %>% filter(sentiment == "negative")

# Function to create DTM
dtm_positive <- positive_lyrics %>%
  count(song_id, word, sort = TRUE) %>%
  cast_dtm(document = song_id, term = word, value = n)

# Negative lyrics DTM
dtm_negative <- negative_lyrics %>%
  count(song_id, word, sort = TRUE) %>%
  cast_dtm(document = song_id, term = word, value = n)
num_topics <- 10

# Positive topics
lda_positive <- LDA(dtm_positive, k = num_topics, control = list(seed = 123))
topics_positive <- tidy(lda_positive, matrix = "beta")

# Negative topics
lda_negative <- LDA(dtm_negative, k = num_topics, control = list(seed = 123))
topics_negative <- tidy(lda_negative, matrix = "beta")

top_words_positive <- topics_positive %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>%
  ungroup()

top_words_negative <- topics_negative %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>%
  ungroup()

# Positive topics visualization
ggplot(top_words_positive, aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs(title = "Top Words for Positive Topics", x = "Word", y = "Beta")

# Negative topics visualization
ggplot(top_words_negative, aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs(title = "Top Words for Negative Topics", x = "Word", y = "Beta")

