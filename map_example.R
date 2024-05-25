library(dplyr)
library(readr)
library(ggplot2)
library(maps)

# Set working directory and load data
setwd("/users/yinyu/INFO201/a4-incarceration-assignment-qianyz6")
data <- read_csv("csv_files/us-prison-pop.csv")

# Mapping table for state abbreviations to full state names
state_abbreviations <- data.frame(
  state = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", 
            "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", 
            "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", 
            "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", 
            "VT", "VA", "WA", "WV", "WI", "WY"),
  region = c("alabama", "alaska", "arizona", "arkansas", "california", "colorado", 
             "connecticut", "delaware", "florida", "georgia", "hawaii", "idaho", 
             "illinois", "indiana", "iowa", "kansas", "kentucky", "louisiana", 
             "maine", "maryland", "massachusetts", "michigan", "minnesota", 
             "mississippi", "missouri", "montana", "nebraska", "nevada", 
             "new hampshire", "new jersey", "new mexico", "new york", 
             "north carolina", "north dakota", "ohio", "oklahoma", "oregon", 
             "pennsylvania", "rhode island", "south carolina", "south dakota", 
             "tennessee", "texas", "utah", "vermont", "virginia", "washington", 
             "west virginia", "wisconsin", "wyoming")
)

# Load state map data
states_map <- map_data("state")

# Data preparation
data_map <- data %>%
  filter(year == 2016) %>%
  group_by(state) %>%
  mutate(total_prison_pop = replace_na(total_prison_pop, 0)) %>%
  summarise(total_prison_pop_state = sum(total_prison_pop)) %>%
  left_join(state_abbreviations, by = "state") %>%
  select(region, total_prison_pop_state)

# Merge map data with prison population data
map_data <- merge(states_map, data_map, by = "region", all.x = TRUE)

# Plot the map with high-contrast color gradient
ggplot(map_data, aes(x = long, y = lat, group = group, fill = total_prison_pop_state)) +
  geom_polygon(color = "white") +
  coord_map() +
  scale_fill_gradientn(
    colors = c("blue", "yellow", "red"),
    na.value = "gray",
    name = "Total Prison Population"
  ) +
  labs(
    title = "Geographic Distribution of Total Prison Population (2016)",
    fill = "Total Prison Population"
  ) +
  theme_minimal()