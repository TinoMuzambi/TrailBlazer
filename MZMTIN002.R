library(shiny)
library(tidyverse)
library(leaflet)
library(plotly)
library(semantic.dashboard)
library(shinycssloaders)
library(viridis)
library(leaflet.extras)

dat <- list.files("data/", "*.csv", full.names = T) %>% 
  read_csv(., id = "run") %>% 
  mutate(run = dense_rank(run), time = ymd_hms(paste(date, time)))

# Group by run and sum distances to get total distance for each run
run.stats <- dat %>%
  group_by(run) %>%
  mutate(dist = distHaversine(cbind(lag(lng), lag(lat)), cbind(lng, lat))) %>%
  summarise(
    total.distance = sum(dist, na.rm = T),
    total.time = as.numeric(difftime(last(time), first(time), units = "secs")),
    average.elevation = mean(elevation, na.rm = T),
    elevation.gain = sum(diff(elevation[elevation > lag(elevation, default = first(elevation))]), na.rm = T),
    date = as.character(first(date))
  ) %>% 
  mutate(total.distance = total.distance / 1000) %>% 
  mutate(pace = (as.numeric(total.time) / 60) / total.distance)

run.list <- dat %>%
  distinct(run, .keep_all = TRUE) %>%
  pull(run)

ui <- dashboardPage(
  dashboardHeader(title = "Runner"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Runs", tabName = "runs")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(
        "
body, .tab-content, .dashboard-sidebar, .dashboard-header {
  background-color: rgb(22, 22, 22) !important;
  color: white !important;
}

a.item, .dashboard-title {
  color: white !important;
}

.ui.segment {
  border: none !important;
}

.ui.top.menu.dashboard-header {
  border: none !important;
}

.leaflet {
  border-radius: 1rem;
  border: 1px solid white;
}

.leaflet-layer,
.leaflet-control-zoom-in,
.leaflet-control-zoom-out,
.leaflet-control-attribution {
  filter: invert(100%) hue-rotate(180deg) brightness(95%) contrast(90%);
}

.selectize-input {
  background-color: black !important;
  color: white !important;
}

.selectize-control.single .selectize-input:not(.no-arrow):after {
    border-color: white transparent transparent transparent;
}
"
      )
    ),
    tabItems(
      tabItem(
        tabName = "home",
        
      ),
      tabItem(
        tabName = "runs",
        fluidRow(  # Wrap elements in fluidRow
          column(width = 4, selectInput("run.selector", label = "Select Run:", choices = run.list, selected = 1)),
          column(width = 12, 
                 leafletOutput("run.map") %>% 
                   withSpinner(color="#0dc5c1"), 
                 plotlyOutput("elevation.chart", height = "200px") %>% 
                   withSpinner(color="#0dc5c1"),
                 textOutput("run.dist"),
                 textOutput("run.time"),
                 textOutput("run.date"),
                 textOutput("run.pace")
          )
        ),
      )
    )
  )
)



# ui = shiny::htmlTemplate(
#   # Index Page
#   "www/index.html",
# 
#   run.selector = selectInput("run.selector",
#                              label = "Select Run:",
#                              choices = run.list,
#                              selected = 1
#   ),
# 
#   run.path.map = leafletOutput("run.map") %>%
#     withSpinner(color="#0dc5c1"),
# 
#   run.distribution = plotlyOutput("run.dist")
# )


server <- function(input, output, session) {
  # Filter data for the selected run
  run.data.filtered <- reactive({
    dat %>%
      filter(run == input$run.selector)
  })
  
  curr.run.stats <- reactive({
    run.stats %>% 
    filter(run == input$run.selector)
  })
  
  output$run.map <- renderLeaflet({
    # Get coordinates for the run
    lat <- run.data.filtered()$lat
    lng <- run.data.filtered()$lng
    elevation <- run.data.filtered()$elevation
    
    # Create a color vector based on elevation using the viridis color scale
    pal <- viridis(n = nrow(run.data.filtered()), option = "viridis")
    colors <- pal[order(elevation)]  # Match colors to elevation values
    
    start.icon <- makeIcon(
      iconUrl = "www/start.png",
      iconWidth = 32, iconHeight = 32,
      iconAnchorX = 16, iconAnchorY = 32,
    )
    
    finish.icon <- makeIcon(
      iconUrl = "www/finish.png",
      iconWidth = 32, iconHeight = 32,
      iconAnchorX = 16, iconAnchorY = 32
    )
    
    # Create a leaflet map
    leaflet() %>%
      setView(lng = mean(lng), lat = mean(lat), zoom = 13) %>%
      addTiles() %>% # Add base map tiles
      addPolylines(lng = lng, lat = lat, data = run.data.filtered(), color = colors) %>% # Plot run path
      addMarkers(lng = lng[1], lat = lat[1], icon = start.icon) %>% 
      addMarkers(lng = lng[length(lng)], lat = lat[length(lat)], icon = finish.icon)
  })
  
  output$run.dist <- renderText({
    paste0("Distance: ", round(curr.run.stats()$total.distance, 2), "km")
  })
  
  output$run.time <- renderText({
    # Function to convert seconds to a formatted string
    format.run.time <- function(seconds) {
      # Calculate hours, minutes, and seconds
      hours <- floor(seconds() / 3600)
      minutes <- floor((seconds %% 3600) / 60)
      remaining.seconds <- seconds %% 60
      
      # Build the formatted string
      formatted.time <- paste0("Time: ", 
                              if (hours > 0) paste0(hours, " hour", if (hours > 1) "s", ", "), 
                              if (minutes > 0) paste0(minutes, " minute", if (minutes > 1) "s ", " and "), 
                              if (remaining.seconds > 0 || (hours == 0 && minutes == 0)) paste0(remaining.seconds, " second", if (remaining.seconds > 1) "s"), 
                              ".")
      
      return(formatted.time)
    }
    format.run.time(curr.run.stats()$total.time)
  })
  
  output$run.date <- renderText({
   paste0("Date: ", curr.run.stats()$date)
  })
  
  output$run.pace <- renderText({
    paste0("Pace: ", round(curr.run.stats()$pace, 2), " min/km")
  })
  
  output$elevation.chart <- renderPlotly({
    run.data.filtered() %>% 
      ggplot(aes(time, elevation)) +
      geom_line(color = "white") + 
      xlab("Time") +
      ylab("Elevation (m)") +
      theme(
        plot.background = element_rect(fill = "#161616"),  # Set plot background color to black
        panel.background = element_rect(fill = "#161616"), # Set panel background color to black
        panel.border = element_blank(),                   # Remove panel border
        axis.text = element_text(color = "white"),        # Set axis text color to white
        axis.title = element_text(color = "white")        # Set axis title color to white
      )
  })
}

shinyApp(ui, server)