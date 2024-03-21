library(shiny)
library(tidyverse)
library(leaflet)
library(plotly)
library(shinydashboard)

dat <- list.files("data/", "*.csv", full.names = T) %>%
  read_csv(., id = "run") %>%
  mutate(run = dense_rank(run))

run_list <- dat %>%
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
body, .tab-content, .dashboard-sidebar {
  background-color: rgb(22, 22, 22) !important;
  color: white !important;
}

a {
  color: white !important;
}

.ui.segment {
  border: none !important;
}
"
      )
    ),
    tabItems(
      tabItem(
        tabName = "home",
        selectInput("run_selector",
                    label = "Select Run:",
                    choices = run_list,
                    selected = 1
                    ),
        
        leafletOutput("run_map"),
        
        plotlyOutput("run_dist")
      ),
      tabItem(
        tabName = "runs",
        fluidPage(
          
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Filter data for the selected run
  run_data_filtered <- reactive({
    dat %>%
      filter(run == as.numeric(input$run_selector))
  }) 
  
  
  output$run_map <- renderLeaflet({
    # Get coordinates for the run
    lat <- run_data_filtered()$lat
    lng <- run_data_filtered()$lng
    
    # Create a leaflet map
    leaflet() %>%
      setView(lng = mean(lng), lat = mean(lat), zoom = 12) %>%
      addTiles() %>% # Add base map tiles
      addPolylines(lng = lng, lat = lat, data = run_data_filtered(), color = "blue") # Plot run path
  })
  
  output$run_dist <- renderPlotly({
    p <- ggplot(dat, aes(run)) +
      geom_histogram()
    p
  })
}

shinyApp(ui, server)