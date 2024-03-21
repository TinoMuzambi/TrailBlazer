library(tidyverse)
library(leaflet)
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
  