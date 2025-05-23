#overarching question: What are the key factors that influence a movie's box office performance, 
##and can we predict a movie's box office earnings based on those factors?
#Does the audience's taste change over time? 
#What is the general trend for the change?

#challenges: both websites have anti-scraping mechanisms so we had to use rvest

#scraping data from The Numbers’ yearly box office summaries
# Load required libraries
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(janitor)
library(lubridate)
library(ggplot2)
library(httr)
library(jsonlite)
library(tidyr)

# Function to scrape data for a given year
scrape_numbers_year <- function(year) {
  url <- paste0("https://www.the-numbers.com/market/", year, "/top-grossing-movies")
  page <- tryCatch(read_html(url), error = function(e) return(NULL))
  if (is.null(page)) return(NULL)
  
  tables <- page |> html_nodes("table")
  table_list <- tables |> map(html_table, fill = TRUE)
  
  # Find the relevant table
  movie_table <- table_list |>
    keep(~ any(str_detect(names(.x), regex("movie", ignore_case = TRUE)))) |>
    pluck(1, .default = NULL)
  
  if (is.null(movie_table)) {
    message(paste("No valid table for", year))
    return(NULL)
  }
  
  movie_table <- clean_names(movie_table)
  
  # Dynamically find gross column
  gross_col <- names(movie_table)[str_detect(names(movie_table), regex("gross", ignore_case = TRUE))][1]
  if (is.na(gross_col)) {
    message(paste("No gross column found for", year))
    return(NULL)
  }
  
  # Create data frame and parse release date safely
  df <- movie_table |>
    rename(gross = all_of(gross_col)) |>
    mutate(
      year = year,
      gross = as.numeric(gsub("[^0-9]", "", gross)),
      release_date = if ("release_date" %in% names(movie_table)) {
        parse_date_time(movie_table$release_date, orders = c("mdy", "ymd", "dmy"), quiet = TRUE)
      } else {
        as.POSIXct(NA)
      }
    )
  
  message(paste("Scraped", nrow(df), "rows for", year))
  return(df)
}

# Scrape data for years 2000 through 2024
years <- 2000:2024
movies_list <- map(years, scrape_numbers_year)
movies_df <- bind_rows(movies_list)

glimpse(movies_df)

#Box Office Gross: Trend over time
#Has Audience Preference Shifted Toward Certain Genres Over Time?
genre_trends <- movies_df |>
  filter(!genre %in% c("Total Gross of All Movies", "Total Tickets Sold", "")) |>
  group_by(year, genre) |>
  summarise(total_gross = sum(gross, na.rm = TRUE), .groups = "drop") |>
  group_by(year) |>
  mutate(genre_share = total_gross / sum(total_gross))

ggplot(genre_trends, aes(x = year, y = genre_share, fill = genre)) +
  geom_area() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Changing Genre Preferences Over Time",
    y = "Share of Annual Box Office Gross", x = "Year"
  ) +
  theme_minimal()

movies_df |>
  mutate(month = month(release_date, label = TRUE)) |>
  filter(!is.na(month)) |>  # Remove rows with NA months
  group_by(month) |>
  summarise(avg_gross = mean(gross, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = month, y = avg_gross)) +
  geom_col(fill = "darkgreen") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title = "Average Box Office Gross by Month",
       y = "Average Gross", x = "Release Month")

# Prepare data
ticket_trend <- movies_df |>
  group_by(year) |>
  summarise(tickets = sum(parse_number(tickets_sold), na.rm = TRUE)) |>
  filter(!is.na(tickets))

# Plot with trend and annotations
ggplot(ticket_trend, aes(x = year, y = tickets)) +
  geom_line(color = "darkorange", linewidth = 1.1) +
  geom_smooth(method = "loess", se = FALSE, color = "steelblue", linewidth = 1) +
  geom_rect(aes(xmin = 2020, xmax = 2021, ymin = 0, ymax = Inf),
            fill = "red", alpha = 0.10, inherit.aes = FALSE) +
  annotate("text", x = 2020.5, y = max(ticket_trend$tickets, na.rm = TRUE) * 0.9,
           label = "COVID-19 Impact", color = "red", size = 4, angle = 90, vjust = -0.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total Tickets Sold by Year (2000–2024)",
       subtitle = "With Trend Line and COVID-19 Period Highlighted",
       x = "Year",
       y = "Tickets Sold") +
  theme_minimal()

#enriching with OMDB API
api_key <- "bfcea4f5"

get_omdb_data <- function(title, year = NA) {
  base_url <- "http://www.omdbapi.com/"
  query <- list(t = title, apikey = api_key)
  if (!is.na(year)) query$y <- year
  
  url <- modify_url(base_url, query = query)
  message("Querying: ", url)
  
  res <- GET(url)
  if (res$status_code != 200) return(tibble())
  
  data <- content(res, as = "parsed", simplifyVector = TRUE)
  if (data$Response == "False") {
    message("❌ Not found: ", title, " (", year, ") — ", data$Error)
    return(tibble())
  }
  
  tibble(
    title = data$Title,
    year = data$Year,
    genre = data$Genre,
    runtime = data$Runtime,
    rating = data$Rated,
    imdb_rating = suppressWarnings(as.numeric(data$imdbRating)),
    box_office = data$BoxOffice
  )
}

# Clean unique titles + years
unique_movies <- movies_df |>
  filter(!is.na(movie), !is.na(year)) |>
  distinct(movie, year)

# Add metadata
omdb_metadata <- unique_movies |>
  rowwise() |>
  mutate(data = list(get_omdb_data(movie, year))) |>
  unnest(cols = c(data), names_sep = "_")

omdb_metadata <- omdb_metadata |>
  mutate(data_year = as.integer(data_year))

movies_enriched <- movies_df |>
  left_join(omdb_metadata, by = c("movie" = "data_title", "year" = "data_year"))

movies_enriched |>
  select(movie, year, data_genre, data_imdb_rating, data_runtime, data_box_office) |>
  filter(!is.na(data_genre)) |>
  head(10)

library(readr)

movies_enriched <- movies_enriched |>
  mutate(
    runtime_min = suppressWarnings(parse_number(data_runtime)),
    box_office_usd = suppressWarnings(parse_number(data_box_office))
  )

#1. Genre Share Over Time
# Split multi-genre movies into separate rows
genre_trends_enriched <- movies_enriched |>
  filter(!is.na(data_genre)) |>
  separate_rows(data_genre, sep = ",\\s*") |>
  group_by(year, data_genre) |>
  summarise(total_gross = sum(gross, na.rm = TRUE), .groups = "drop") |>
  group_by(year) |>
  mutate(genre_share = total_gross / sum(total_gross)) |>
  ungroup()

# Focus on top recurring genres
top_genres <- genre_trends_enriched |>
  group_by(data_genre) |>
  summarise(avg_share = mean(genre_share, na.rm = TRUE)) |>
  top_n(6, avg_share) |>
  pull(data_genre)

# Plot genre share over time
genre_trends_enriched |>
  filter(data_genre %in% top_genres) |>
  ggplot(aes(x = year, y = genre_share, fill = data_genre)) +
  geom_area() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Audience Genre Preferences Over Time",
    x = "Year", y = "Share of Total Gross",
    fill = "Genre"
  ) +
  theme_minimal()
# This shows genre dominance shifting toward action, adventure, and fantasy.


#2. IMDb Rating vs. Box Office
movies_enriched |>
  filter(!is.na(data_imdb_rating), !is.na(box_office_usd)) |>
  ggplot(aes(x = data_imdb_rating, y = box_office_usd)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = "Does Higher IMDb Rating Predict Greater Box Office?",
    x = "IMDb Rating", y = "Box Office (USD)"
  ) +
  theme_minimal()
#A weak but visible upward trend shows quality may impact earnings, especially for theatrical releases.

#3. Average Runtime Over Time
movies_enriched |>
  filter(!is.na(runtime_min)) |>
  group_by(year) |>
  summarise(avg_runtime = mean(runtime_min, na.rm = TRUE)) |>
  ggplot(aes(x = year, y = avg_runtime)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  labs(
    title = "Average Movie Runtime Over Time (2000–2024)",
    x = "Year", y = "Average Runtime (min)"
  ) +
  theme_minimal()

#Shows a rise in runtime — supporting that audiences now tolerate or expect longer films (often tied to epic/franchise stories)

#4. MPAA Ratings Over Time
movies_enriched |>
  filter(!is.na(data_rating)) |>
  group_by(year, data_rating) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(year) |>
  mutate(share = count / sum(count)) |>
  ggplot(aes(x = year, y = share, fill = data_rating)) +
  geom_area() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "MPAA Rating Trends Over Time",
    x = "Year", y = "Share of Releases",
    fill = "MPAA Rating"
  ) +
  theme_minimal()
#Expect PG-13 to dominate recent decades — audience preference consolidates around this rating for broader appeal.

#1. Genre Turnover Analysis: examine which genre dominating each year
#Diversity of Genre Consumption
library(ineq)

genre_diversity <- movies_enriched |>
  filter(!is.na(data_genre)) |>
  separate_rows(data_genre, sep = ",\\s*") |>
  group_by(year, data_genre) |>
  summarise(total_gross = sum(gross, na.rm = TRUE), .groups = "drop") |>
  group_by(year) |>
  mutate(share = total_gross / sum(total_gross)) |>
  summarise(genre_gini = ineq::Gini(share))

ggplot(genre_diversity, aes(x = year, y = genre_gini)) +
  geom_line(color = "purple", linewidth = 1.2) +
  labs(title = "Genre Inequality (Gini Index) Over Time",
       y = "Gini Coefficient (0 = Diverse, 1 = Concentrated)",
       x = "Year") +
  theme_minimal()

