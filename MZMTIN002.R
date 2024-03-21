library(shiny)
library(tidyverse)
library(leaflet)
library(plotly)
library(semantic.dashboard)
library(shinycssloaders)

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
body, .tab-content, .dashboard-sidebar, .dashboard-header {
  background-color: rgb(22, 22, 22) !important;
  color: white !important;
}

a, .dashboard-title {
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
        fluidRow(  # Wrap elements in fluidRow
          column(width = 4, selectInput("run_selector", label = "Select Run:", choices = run_list, selected = 1)),
          column(width = 12, leafletOutput("run_map") %>% 
                   withSpinner(color="#0dc5c1"))
        ),
        fluidRow(
          column(width = 16, plotlyOutput("run_dist") %>% 
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
#   run_selector = selectInput("run_selector",
#                              label = "Select Run:",
#                              choices = run_list,
#                              selected = 1
#   ),
# 
#   run_path_map = leafletOutput("run_map") %>%
#     withSpinner(color="#0dc5c1"),
# 
#   run_distribution = plotlyOutput("run_dist")
# )


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
      setView(lng = mean(lng), lat = mean(lat), zoom = 13) %>%
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