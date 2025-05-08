# Setup Directories and File Paths
dir_path <- "data/mp04"
zip_url <- "https://www2.census.gov/geo/tiger/GENZ2023/shp/cb_2023_us_county_500k.zip"
zip_file <- file.path(dir_path, "cb_2023_us_county_500k.zip")

# Create directories and download shapefile if necessary
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
  message("Created directory: ", dir_path)
}

if (!file.exists(zip_file)) {
  download.file(zip_url, destfile = zip_file, mode = "wb")
  message("Downloaded shapefile to: ", zip_file)
} else {
  message("Shapefile already downloaded.")
}

# Unzip shapefile if not already extracted
unzip_dir <- file.path(dir_path, "cb_2023_us_county_500k")
shp_file <- file.path(unzip_dir, "cb_2023_us_county_500k.shp")

if (!file.exists(shp_file)) {
  unzip(zip_file, exdir = unzip_dir)
  message("Unzipped shapefile to: ", unzip_dir)
} else {
  message("Shapefile already unzipped.")
}

# Load Libraries
library(sf)
library(httr2)
library(rvest)
library(dplyr)
library(janitor)
library(stringr)
library(readr)
library(tidyr)
library(ggplot2)
# Load shapefile
counties_sf <- st_read(shp_file)
glimpse(counties_sf)

# Ensure CRS is set to NAD83 (EPSG:4269) or commonly used WGS 84 (EPSG:4326)
counties_sf <- st_transform(counties_sf, crs = 4326)

# Scraper function for 2024 results
get_state_results <- function(state_name) {
  state_slug <- gsub(" ", "_", state_name)
  wiki_url <- paste0("https://en.wikipedia.org/wiki/2024_United_States_presidential_election_in_", state_slug)
  
  dir_path <- "data/mp04/html_pages"
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  
  html_file <- file.path(dir_path, paste0(state_slug, ".html"))
  if (!file.exists(html_file)) {
    req <- request(wiki_url) |> req_perform()
    writeBin(resp_body_raw(req), html_file)
    message("Downloaded and saved HTML for: ", state_name)
  } else {
    message("Using cached HTML for: ", state_name)
  }
  
  page <- read_html(html_file)
  tables <- page |> html_nodes("table") |> html_table(fill = TRUE)
  
  county_keywords <- c("County", "Parish", "Borough", "District")
  target_table <- NULL
  
  # Find the county-level table
  for (tbl in tables) {
    if (any(str_detect(names(tbl), paste(county_keywords, collapse = "|")))) {
      target_table <- tbl
      break
    }
  }
  
  if (is.null(target_table)) {
    warning("No suitable county-level table found for: ", state_name)
    return(NULL)
  }
  
  target_table <- target_table |> clean_names() |> mutate(state = state_name)
  return(target_table)
}

# Scrape all states for 2024 election results
state_names <- state.name
all_results_2024 <- lapply(state_names, get_state_results)
all_results_2024_df <- bind_rows(Filter(Negate(is.null), all_results_2024))

# Logging missing data
missing_states_2024 <- state_names[sapply(all_results_2024, is.null)]
print(missing_states_2024)

# Scraper function for 2020 results (similar to 2024, but with election year set to 2020)
get_state_results_2020 <- function(state_name) {
  state_slug <- gsub(" ", "_", state_name)
  wiki_url <- paste0("https://en.wikipedia.org/wiki/2020_United_States_presidential_election_in_", state_slug)
  
  dir_path <- "data/mp04/html_pages_2020"
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  
  html_file <- file.path(dir_path, paste0(state_slug, ".html"))
  if (!file.exists(html_file)) {
    req <- request(wiki_url) |> req_perform()
    writeBin(resp_body_raw(req), html_file)
    message("Downloaded and saved HTML for 2020 election: ", state_name)
  } else {
    message("Using cached HTML for 2020 election: ", state_name)
  }
  
  page <- read_html(html_file)
  tables <- page |> html_nodes("table") |> html_table(fill = TRUE)
  
  county_keywords <- c("County", "Parish", "Borough", "District")
  target_table <- NULL
  
  # Find the county-level table for 2020 results
  for (tbl in tables) {
    if (any(str_detect(names(tbl), paste(county_keywords, collapse = "|")))) {
      target_table <- tbl
      break
    }
  }
  
  if (is.null(target_table)) {
    warning("No suitable county-level table found for 2020 election: ", state_name)
    return(NULL)
  }
  
  target_table <- target_table |> clean_names() |> mutate(state = state_name, election_year = 2020)
  return(target_table)
}

# Scrape all states for 2020 election results
all_results_2020 <- lapply(state_names, get_state_results_2020)
all_results_2020_df <- bind_rows(Filter(Negate(is.null), all_results_2020))

# Logging missing data for 2020 results
missing_states_2020 <- state_names[sapply(all_results_2020, is.null)]
print(missing_states_2020)

# Clean county names in election results
all_results_2020_df <- all_results_2020_df |>
  mutate(county = str_to_title(county))  # Normalize county names for better matching
all_results_2024_df <- all_results_2024_df |>
  mutate(county = str_to_title(county))

# Clean and standardize county names for 2020
clean_2020 <- all_results_2020_df |>
  filter(
    !county %in% c("County", "Total", "Parish", "Borough", "District"),
    !str_detect(county, "Total|County|Parish|Borough|District")
  ) |>
  mutate(
    county = str_replace_all(county, " County| Parish| Borough| District", ""),
    county = str_to_title(str_trim(county))
  )

# Convert vote counts to numeric for 2020
clean_2020 <- clean_2020 |>
  mutate(across(
    c(donald_trump_republican, joe_biden_democratic),
    ~ as.numeric(gsub(",", "", .)),
    .names = "{.col}_num"
  ))

# Aggregate by county and state for 2020
agg_2020 <- clean_2020 |>
  group_by(state, county) |>
  summarise(
    trump_votes = sum(donald_trump_republican_num, na.rm = TRUE),
    biden_votes = sum(joe_biden_democratic_num, na.rm = TRUE),
    .groups = "drop"
  )

counties_2020_sf <- counties_sf |>
  mutate(
    NAME = str_to_title(NAME),
    STATE_NAME = str_to_title(STATE_NAME)  # Ensure this matches the actual column name
  ) |>
  left_join(agg_2020, by = c("NAME" = "county", "STATE_NAME" = "state"))

# Clean and standardize county names for 2024
clean_2024 <- all_results_2024_df |>
  filter(
    !county %in% c("County", "Total", "Parish", "Borough", "District"),
    !str_detect(county, "Total|County|Parish|Borough|District")
  ) |>
  mutate(
    county = str_replace_all(county, " County| Parish| Borough| District", ""),
    county = str_to_title(str_trim(county))
  )

# Convert vote counts to numeric for 2024
clean_2024 <- clean_2024 |>
  mutate(across(
    c(donald_trump_republican, kamala_harris_democratic),
    ~ as.numeric(gsub(",", "", .)),
    .names = "{.col}_num"
  ))

# Aggregate by county and state for 2024
agg_2024 <- clean_2024 |>
  group_by(state, county) |>
  summarise(
    trump_votes = sum(donald_trump_republican_num, na.rm = TRUE),
    harris_votes = sum(kamala_harris_democratic_num, na.rm = TRUE),
    .groups = "drop"
  )

counties_2024_sf <- counties_sf |>
  mutate(
    NAME = str_to_title(NAME),
    STATE_NAME = str_to_title(STATE_NAME)  # Ensure this matches the actual column name
  ) |>
  left_join(agg_2024, by = c("NAME" = "county", "STATE_NAME" = "state"))

# Which county or counties cast the most votes for Trump (in absolute terms) in 2024?
counties_2024_sf |>
  filter(!is.na(trump_votes)) |>
  slice_max(trump_votes, n = 1) |>
  select(NAME, STATE_NAME, trump_votes)

# Which county or counties cast the most votes for Biden (as a fraction of total votes cast) in 2020?
counties_2020_sf |>
  filter(!is.na(trump_votes) & !is.na(biden_votes)) |>
  mutate(total_votes = trump_votes + biden_votes,
         biden_share = biden_votes / total_votes) |>
  slice_max(biden_share, n = 1) |>
  select(NAME, STATE_NAME, biden_votes, total_votes, biden_share)

# Which county or counties had the largest shift towards Trump (in absolute terms) in 2024?
# Join both years' vote data
vote_change <- counties_2020_sf |>
  st_drop_geometry() |>
  group_by(NAME, STATE_NAME) |>
  summarise(trump_votes_2020 = sum(trump_votes, na.rm = TRUE), .groups = "drop") |>
  inner_join(
    counties_2024_sf |>
      st_drop_geometry() |>
      group_by(NAME, STATE_NAME) |>
      summarise(trump_votes_2024 = sum(trump_votes, na.rm = TRUE), .groups = "drop"),
    by = c("NAME", "STATE_NAME")
  ) |>
  mutate(trump_shift = trump_votes_2024 - trump_votes_2020) |>
  slice_max(trump_shift, n = 1) |>
  select(NAME, STATE_NAME, trump_shift)
vote_change

# Which state had the largest shift towards Harris (or smallest shift towards Trump) in 2024? (Note that the total votes for a state can be obtained by summing all counties in that state.)
# Aggregate by state
state_shift <- counties_2020_sf |>
  st_drop_geometry() |>
  group_by(STATE_NAME) |>
  summarise(
    trump_2020 = sum(trump_votes, na.rm = TRUE),
    biden_2020 = sum(biden_votes, na.rm = TRUE),
    .groups = "drop"
  ) |>
  inner_join(
    counties_2024_sf |>
      st_drop_geometry() |>
      group_by(STATE_NAME) |>
      summarise(
        trump_2024 = sum(trump_votes, na.rm = TRUE),
        harris_2024 = sum(harris_votes, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "STATE_NAME"
  ) |>
  mutate(trump_shift = trump_2024 - trump_2020) |>
  slice_min(trump_shift, n = 1) |>
  select(STATE_NAME, trump_2020, trump_2024, trump_shift)
state_shift

# What is the largest county, by area, in this data set?
counties_sf |>
  slice_max(ALAND, n = 1) |>
  select(NAME, STATE_NAME, ALAND)

# Calculate Trump percentage for 2020
counties_2020_sf <- counties_2020_sf |>
  mutate(
    total_votes_2020 = trump_votes + biden_votes,  # Total votes for 2020
    trump_pct_2020 = trump_votes / total_votes_2020 * 100  # Trump percentage
  )

# Calculate Trump percentage for 2024
counties_2024_sf <- counties_2024_sf |>
  mutate(
    total_votes_2024 = trump_votes + harris_votes,  # Total votes for 2020
    trump_pct_2024 = trump_votes / total_votes_2024 * 100  # Trump percentage
  )

# Which county has the highest voter density (voters per unit of area) in 2020?
counties_2020_sf |>
  mutate(total_votes = trump_votes + biden_votes,
         voter_density = total_votes / ALAND) |>
  slice_max(voter_density, n = 1) |>
  select(NAME, STATE_NAME, total_votes, ALAND, voter_density)

# Which county had the largest increase in voter turnout in 2024?
turnout_change <- counties_2020_sf |>
  st_drop_geometry() |>
  mutate(total_2020 = trump_votes + biden_votes) |>
  select(NAME, STATE_NAME, total_2020) |>
  inner_join(
    counties_2024_sf |>
      st_drop_geometry() |>
      mutate(total_2024 = trump_votes + harris_votes) |>
      select(NAME, STATE_NAME, total_2024),
    by = c("NAME", "STATE_NAME")
  ) |>
  mutate(turnout_diff = total_2024 - total_2020) |>
  slice_max(turnout_diff, n = 1) |>
  select(NAME, STATE_NAME, total_2020, total_2024, turnout_diff)

print(turnout_change)

# Task 5, Step1: Computing the shift rightwards for each county
# Convert the sf objects to data frames before the join
counties_2020_df <- counties_2020_sf |>
  st_drop_geometry() |>
  select(NAME, STATE_NAME, trump_pct_2020)

counties_2024_df <- counties_2024_sf |>
  st_drop_geometry() |>
  select(NAME, STATE_NAME, trump_pct_2024)

# Perform the join on the data frames
# First, merge the 2020 and 2024 vote data
county_votes <- counties_2020_df |>
  inner_join(counties_2024_df, by = c("NAME", "STATE_NAME")) |>
  mutate(
    shift_pct = trump_pct_2024 - trump_pct_2020
  )

# Then join the vote data into the sf object
counties_sf_combined <- counties_sf |>
  left_join(county_votes, by = c("NAME", "STATE_NAME"))

# Check if the geometry column is added back
glimpse(counties_sf_combined)

#Task 5, Step 2: Modifying geometry for Hawaii and Alaska
# Transform Alaska and Hawaii geometries separately
counties_2024_sf <- counties_2024_sf |>
  mutate(geometry = st_transform(geometry, crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lon_0=-96"))

# Revert geometries for Alaska and Hawaii to the original ones after transformation
counties_2024_sf <- counties_2024_sf |>
  mutate(
    geometry = case_when(
      STATE_NAME == "Alaska" ~ geometry,
      STATE_NAME == "Hawaii" ~ geometry,
      TRUE ~ geometry
    )
  )

#Task 5, Step 3: Draw the Map with Modified Geometry
# Calculate the shift in Trump percentage (2024 - 2020)
# After joining 2020 and 2024 data frames and adding the geometry
counties_sf_combined <- counties_sf_combined |>
  mutate(
    shift_pct = trump_pct_2024 - trump_pct_2020  # Positive values indicate rightward shift
  )

# Now plot the map with shift_pct
counties_sf_combined |>
  filter(!is.na(shift_pct)) |>
  ggplot() +
  geom_sf(aes(fill = shift_pct), color = "white", size = 0.1) +
  scale_fill_gradient2(
    low = "#0041C2", mid = "white", high = "#d7301f", midpoint = 0,
    name = "Shift in % Trump Vote"
  ) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
  labs(
    title = "County-Level Shift in Trump Vote Share (2020–2024)",
    subtitle = "Red = rightward shift, Blue = leftward shift"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 10))
  )
