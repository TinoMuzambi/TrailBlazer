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
  mutate(run = dense_rank(run))

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
"
      )
    ),
    tabItems(
      tabItem(
        tabName = "home",
        fluidRow(  # Wrap elements in fluidRow
          column(width = 4, selectInput("run.selector", label = "Select Run:", choices = run.list, selected = 1)),
          column(width = 12, leafletOutput("run.map") %>% 
                   withSpinner(color="#0dc5c1"))
        ),
        fluidRow(
          column(width = 16, plotlyOutput("run.dist") %>% 
                   withSpinner(color="#0dc5c1"))
        )
      ),
      tabItem(
        tabName = "runs",
        fluidPage(

        )
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
      filter(run == as.numeric(input$run.selector))
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
      addMarkers(lng = lng[1], lat = lat[1], icon = start.icon, popup = "Start") %>% 
      addMarkers(lng = lng[length(lng)], lat = lat[length(lat)], icon = finish.icon, popup = "End")
  })
  
  output$run.dist <- renderPlotly({
    p <- ggplot(dat, aes(run)) +
      geom_histogram()
    p
  })
}

shinyApp(ui, server)