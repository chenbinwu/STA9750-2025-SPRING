library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(jsonlite)
library(ggplot2)
library(lubridate)

# Helper Functions -------------------------------------------------------

# Clean artist string
clean_artist_string <- function(x) {
  x |>
    str_replace_all("\\['", "") |>
    str_replace_all("'\\]", "") |>
    str_replace_all("[ ]?'", "") |>
    str_replace_all("[ ]*,[ ]*", ",")
}

# Remove Spotify prefix from URIs
strip_spotify_prefix <- function(x) {
  str_replace(x, ".*:", "")
}

# Load and clean songs data
load_songs <- function() {
  dir_path <- "data/mp03"
  file_path <- file.path(dir_path, "data.csv")
  url <- "https://raw.githubusercontent.com/gabminamedez/spotify-data/refs/heads/master/data.csv"
  
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  if (!file.exists(file_path)) {
    download.file(url, destfile = file_path, method = "libcurl")
  }
  
  songs_df <- read.csv(file_path, stringsAsFactors = FALSE)
  return(songs_df)
}

# Load playlist JSON files
load_playlists <- function(directory = "data/mp03/playlists") {
  playlist_files <- list.files(directory, pattern = "mpd.slice.*\\.json$", full.names = TRUE)
  playlists <- lapply(playlist_files, function(file) {
    fromJSON(file, flatten = TRUE)
  })
  return(playlists)
}

# Process playlists into tidy track-level data
process_playlists <- function(playlists_data) {
  track_list <- list()
  
  for (i in seq_along(playlists_data)) {
    playlists_df <- playlists_data[[i]]$playlists
    
    for (j in seq_len(nrow(playlists_df))) {
      playlist <- playlists_df[j, ]
      playlist_name <- playlist$name
      playlist_id <- playlist$pid
      playlist_followers <- playlist$num_followers
      tracks_df <- playlist$tracks[[1]]
      
      for (k in seq_len(nrow(tracks_df))) {
        track <- tracks_df[k, ]
        
        track_data <- data.frame(
          playlist_name = playlist_name,
          playlist_id = playlist_id,
          playlist_position = track$pos + 1,
          playlist_followers = playlist_followers,
          track_name = track$track_name,
          track_id = strip_spotify_prefix(track$track_uri),
          album_name = track$album_name,
          album_id = strip_spotify_prefix(track$album_uri),
          duration = track$duration_ms,
          artist_name = track$artist_name,
          artist_id = strip_spotify_prefix(track$artist_uri),
          stringsAsFactors = FALSE
        )
        
        track_list <- append(track_list, list(track_data))
      }
    }
  }
  
  all_tracks <- bind_rows(track_list)
  return(all_tracks)
}

# Load Data --------------------------------------------------------------

SONGS <- load_songs()
playlists_data <- load_playlists()
tidy_tracks <- process_playlists(playlists_data)

# Exploration Tasks ------------------------------------------------------

distinct_tracks <- tidy_tracks |> distinct(track_id) |> nrow()
distinct_artists <- tidy_tracks |> distinct(artist_id) |> nrow()
cat("Distinct tracks:", distinct_tracks, "\n")
cat("Distinct artists:", distinct_artists, "\n")

top_tracks <- tidy_tracks |>
  group_by(track_name, artist_name) |>
  summarise(count = n(), .groups = "drop") |>
  arrange(desc(count)) |>
  slice_head(n = 50)

top_tracks

missing_in_songs <- tidy_tracks |>
  filter(!track_id %in% SONGS$id) |>
  group_by(track_name, artist_name) |>
  summarise(count = n(), .groups = "drop") |>
  arrange(desc(count)) |>
  slice_head(n = 1)

missing_in_songs

most_danceable <- SONGS |>
  arrange(desc(danceability)) |>
  slice_head(n = 1)

danceable_appearances <- tidy_tracks |>
  filter(track_id == most_danceable$id) |>
  summarise(count = n())

most_danceable_track <- data.frame(
  track_name = most_danceable$name,
  artist_name = most_danceable$artists,
  danceability = most_danceable$danceability,
  appearances = danceable_appearances$count
)

most_danceable_track

longest_avg_playlist <- tidy_tracks |>
  group_by(playlist_name, playlist_id) |>
  summarise(avg_duration = mean(duration, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(avg_duration)) |>
  slice_head(n = 1)

longest_avg_playlist

most_popular_playlist <- tidy_tracks |>
  distinct(playlist_id, playlist_name, playlist_followers) |>
  arrange(desc(playlist_followers)) |>
  slice_head(n = 1)

most_popular_playlist
# Join for popularity analysis -------------------------------------------

joined_df <- inner_join(tidy_tracks, SONGS, by = c("track_id" = "id"))

track_popularity <- joined_df |>
  group_by(track_id, track_name) |>
  summarize(
    playlist_count = n(),
    avg_popularity = mean(popularity, na.rm = TRUE),
    .groups = 'drop'
  )

# Visualize popularity correlation ---------------------------------------
# Ensure release_year is present in the SONGS data
SONGS <- SONGS |>
  mutate(
    release_year = suppressWarnings(as.numeric(str_sub(release_date, 1, 4)))
  ) |>
  filter(!is.na(release_year), release_year > 1900, release_year <= year(Sys.Date()))

# Now join the tidy_tracks with SONGS to get the release_year into the dataset
joined_df <- inner_join(tidy_tracks, SONGS, by = c("track_id" = "id"))

# Filter for popular songs (popularity >= 80)
popular_songs <- joined_df |>
  filter(popularity >= 80)

# Check if 'release_year' is present
head(popular_songs)
# 1. Popularity vs. Playlist Appearances - Correlation and Scatter Plot
ggplot(track_popularity, aes(x = playlist_count, y = avg_popularity)) +
  geom_point(alpha = 0.5, color = "#1DB954") +
  geom_smooth(method = "lm", se = FALSE, color = "gray30") +
  labs(
    title = "Popularity vs. Playlist Appearances",
    x = "Number of Playlist Appearances",
    y = "Average Popularity"
  ) +
  theme_minimal()

# Compute correlation coefficient
correlation <- cor(track_popularity$playlist_count, track_popularity$avg_popularity, use = "complete.obs")
cat("Correlation between playlist count and popularity:", correlation, "\n")

# 2. Popular Songs Release Year Distribution
popular_songs <- joined_df |>
  filter(popularity >= 80)

ggplot(popular_songs, aes(x = release_year)) +
  geom_histogram(binwidth = 1, fill = "#1DB954", color = "white") +
  labs(
    title = "Release Year of Popular Songs",
    x = "Release Year",
    y = "Number of Popular Songs"
  ) +
  theme_minimal()

# 3. Year when Danceability Peaked
ggplot(popular_songs, aes(x = release_year, y = danceability)) +
  geom_boxplot(fill = "#1DB954") +
  labs(
    title = "Danceability by Release Year (Popular Songs)",
    x = "Release Year",
    y = "Danceability"
  ) +
  theme_minimal()

# 4. Which Decade is Most Represented on User Playlists?
popular_songs <- popular_songs |>
  mutate(decade = floor(release_year / 10) * 10)

ggplot(popular_songs, aes(x = factor(decade))) +
  geom_bar(fill = "#1DB954") +
  labs(
    title = "Decade Representation in User Playlists",
    x = "Decade",
    y = "Number of Popular Songs"
  ) +
  theme_minimal()

# 5. Key Frequency Plot (Polar Coordinates)
ggplot(popular_songs, aes(x = factor(key))) +
  geom_bar(fill = "#1DB954") +
  coord_polar() +
  labs(
    title = "Key Frequency Among Popular Songs",
    x = "Key",
    y = "Count"
  ) +
  theme_minimal()

# 6. Track Length vs. Popularity
ggplot(popular_songs, aes(x = duration_ms / 1000, y = popularity)) +  # Convert duration from ms to seconds
  geom_point(alpha = 0.5, color = "#1DB954") +
  geom_smooth(method = "lm", se = FALSE, color = "gray30") +
  labs(
    title = "Track Length vs. Popularity",
    x = "Track Length (Seconds)",
    y = "Popularity"
  ) +
  theme_minimal()

# 7. Additional Exploratory Questions

# Energy vs. Popularity
ggplot(popular_songs, aes(x = energy, y = popularity)) +
  geom_point(alpha = 0.5, color = "#1DB954") +
  geom_smooth(method = "lm", se = FALSE, color = "gray30") +
  labs(
    title = "Energy vs. Popularity",
    x = "Energy",
    y = "Popularity"
  ) +
  theme_minimal()

# Loudness vs. Popularity
ggplot(popular_songs, aes(x = loudness, y = popularity)) +
  geom_point(alpha = 0.5, color = "#1DB954") +
  geom_smooth(method = "lm", se = FALSE, color = "gray30") +
  labs(
    title = "Loudness vs. Popularity",
    x = "Loudness",
    y = "Popularity"
  ) +
  theme_minimal()


# Filter anchor songs from dataset
anchor_songs <- SONGS |>
  filter(name %in% c("She Will Be Loved - Radio Mix", "See You Again (feat. Charles Puth)"))

anchor_ids <- anchor_songs$id

co_occurring_songs <- tidy_tracks |>
  filter(track_id %in% anchor_ids) |>
  select(playlist_id) |>
  inner_join(tidy_tracks, by = "playlist_id") |>
  filter(!track_id %in% anchor_ids) |>
  count(track_id, track_name, artist_name, sort = TRUE)

co_occurring_songs

tempo_tolerance <- 10

same_key_tempo <- inner_join(anchor_songs, SONGS, by = character()) |>
  filter(
    key.x == key.y,
    abs(tempo.x - tempo.y) <= tempo_tolerance,
    id.y != id.x
  ) |>
  select(track_id = id.y, track_name = name.y, artist_name = artists.y) |>
  distinct()

same_key_tempo

same_artist_songs <- SONGS |>
  filter(artists %in% anchor_songs$artists, !id %in% anchor_ids) |>
  select(track_id = id, track_name = name, artist_name = artists)
same_artist_songs

# Select and normalize audio features
normalize <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

SONGS_norm <- SONGS |>
  filter(!is.na(release_date)) |>
  mutate(release_year = as.numeric(str_sub(release_date, 1, 4))) |>
  mutate(across(c(danceability, energy, acousticness, instrumentalness, valence), normalize))

anchor_year <- anchor_songs$release_year[1]

similar_features <- SONGS_norm |>
  filter(release_year == anchor_year, !id %in% anchor_ids) |>
  mutate(
    feature_dist = sqrt(
      (danceability - anchor_songs$danceability[1])^2 +
        (energy - anchor_songs$energy[1])^2 +
        (acousticness - anchor_songs$acousticness[1])^2 +
        (valence - anchor_songs$valence[1])^2
    )
  ) |>
  arrange(feature_dist) |>
  slice_head(n = 20) |>
  select(track_id = id, track_name = name, artist_name = artists)

similar_features

similar_loudness_energy <- SONGS |>
  filter(
    abs(loudness - anchor_songs$loudness[1]) < 2,
    abs(energy - anchor_songs$energy[1]) < 0.1,
    !id %in% anchor_ids
  ) |>
  select(track_id = id, track_name = name, artist_name = artists)

similar_loudness_energy

playlist_candidates <- bind_rows(
  co_occurring_songs,
  same_key_tempo,
  same_artist_songs,
  similar_features,
  similar_loudness_energy
) |>
  distinct(track_id, .keep_all = TRUE)

# Join to get popularity and filter at least 8 non-popular songs
playlist_candidates <- playlist_candidates |>
  left_join(SONGS |> select(id, popularity), by = c("track_id" = "id")) |>
  mutate(is_popular = popularity >= 80)

summary_counts <- playlist_candidates |>
  summarise(
    total_candidates = n(),
    non_popular = sum(!is_popular, na.rm = TRUE)
  )

print(summary_counts)

# Final list of at least 20 candidates, 8+ non-popular
final_candidates <- playlist_candidates |>
  slice_head(n = 30)  # Feel free to tweak
final_candidates

# Example: manually selected final playlist from your candidates
# Replace this with your real selections
final_playlist_ids <- c(
  "1A2GTWGtFfWp7KSQTwWOyo",  # Anchor 1
  "3n3Ppam7vgaVa1iaRUc9Lp",  # Anchor 2
  "6rqhFgbbKwnb9MLmUQDhG6",  # Familiar, not popular
  "5jRDpDgo1zYkBPqt9niKCM",  # Unfamiliar
  "4aWmUDTfIPGksMNLV2rQP2",  # Unfamiliar
  "1UwvwkSZYyJzGzZC9fEPvC",  # Not popular
  "7ouMYWpwJ422jRcDASZB7P",  # Familiar
  "0eGsygTp906u18L0Oimnem",  # Popular
  "5ChkMS8OtdzJeqyybCc9R5",  # Not popular
  "6QgjcU0zLnzq5OrUoSZ3OK",  # Anchor artist
  "3AJwUDP919kvQ9QcozQPxg",  # Unfamiliar
  "3ZOEytgrvLwQaqXreDs2Jx"   # Familiar, not popular
)

ultimate_playlist <- SONGS |>
  filter(id %in% final_playlist_ids) |>
  left_join(tidy_tracks, by = c("id" = "track_id")) |>
  distinct(id, .keep_all = TRUE)  # remove duplicates from multiple playlists

library(ggplot2)

# Order playlist manually or by a vibe-based metric (e.g., energy)
ultimate_playlist <- ultimate_playlist |>
  arrange(desc(energy)) |>
  mutate(order = row_number())

# Melt into long format for plotting multiple metrics
library(tidyr)

ultimate_playlist <- ultimate_playlist |>
  mutate(across(c(danceability, energy, valence, tempo, acousticness), as.numeric))

playlist_long <- ultimate_playlist |>
  select(order, name, danceability, energy, valence, tempo, acousticness) |>
  pivot_longer(cols = c(danceability, energy, valence, tempo, acousticness),
               names_to = "metric", values_to = "value")

playlist_long <- playlist_long |>
  drop_na(value)

# Visualize the emotional and musical flow of the playlist
ggplot(playlist_long, aes(x = order, y = value, color = metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Emotional and Musical Flow of the Playlist",
    x = "Track Order",
    y = "Metric Value"
  ) +
  theme_minimal()



playlist_name <- "Chenbin's Playlist"