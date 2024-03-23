library(tidyverse)
library(leaflet)
library(geosphere)
library(lubridate)

dat <- list.files("data/", "*.csv", full.names = T) %>% 
  read_csv(., id = "run") %>% 
  mutate(run = dense_rank(run), time = as.period(hms(time)))

# Filter data for the selected run
run_data_filtered <- dat %>%
    filter(run == 1)


# Get coordinates for the run
lat <- run_data_filtered$lat
lng <- run_data_filtered$lng
  
# Create a leaflet map
leaflet() %>%
  setView(lng = mean(lng), lat = mean(lat), zoom = 12) %>%
  addTiles() %>% # Add base map tiles
  addPolylines(lng = lng, lat = lat, data = run_data_filtered, color = "blue") # Plot run path

# Group by run and sum distances to get total distance for each run
run.stats <- dat %>%
  group_by(run) %>%
  mutate(dist = distHaversine(cbind(lag(lng), lag(lat)), cbind(lng, lat))) %>%
  summarise(
    total.distance = sum(dist, na.rm = TRUE),
    total.time = sum(time),
    average.elevation = mean(elevation),
    elevation.gain = sum(diff(elevation[elevation > lag(elevation, default = first(elevation))])),
    date = first(date)
  ) %>% 
  mutate(total.distance = total.distance / 1000)

curr.run.stats <- run.stats %>% 
  filter(run == 1)

curr.run.stats$total.distance
  