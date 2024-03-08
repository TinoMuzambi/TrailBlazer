library(tidyverse)

dat <- list.files("data/", "*.csv", full.names = T) %>% 
  read_csv(., id = "run")

head(dat)
