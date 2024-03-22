library(tidyverse)
library(leaflet)
library(geosphere)
dat <- list.files("data/", "*.csv", full.names = T) %>% 
  read_csv(., id = "run") %>% 
  mutate(run = dense_rank(run))

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

# Compute distance between consecutive points
dat <- dat %>% 
  mutate(distance = c(0, distHaversine(
    cbind(dat$lng[-nrow(dat)], dat$lat[-nrow(dat)]),
    cbind(dat$lng[-1], dat$lat[-1])
  ))) %>% 
  mutate(distance = distance / 1000)

# Group by run and sum distances to get total distance for each run
run_distances <- dat %>%
  group_by(run) %>%
  summarize(total_distance = sum(distance, na.rm = TRUE))
  