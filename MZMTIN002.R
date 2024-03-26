library(shiny)
library(tidyverse)
library(leaflet)
library(plotly)
library(semantic.dashboard)
library(shinycssloaders)
library(viridis)
library(leaflet.extras)
library(DT)

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
    elevation.gain = sum(diff(elevation[elevation > lag(elevation, default = first(elevation))]), na.rm = T),
    date = as.character(first(date))
  ) %>% 
  mutate(total.distance = total.distance / 1000) %>% 
  mutate(pace = (as.numeric(total.time) / 60) / total.distance) %>% 
  mutate(speed = total.distance /  (as.numeric(total.time) / 3600)) %>% 
  mutate(date = ymd(date))

run.list <- dat %>%
  distinct(run, .keep_all = TRUE) %>%
  pull(run)

ui <- function(req) {
  fluidPage(
    # dashboardHeader(title = "Runner", logo_path = "logo.png", logo_align = "left"),
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

.selectize-input, .selectize-dropdown {
  background-color: black !important;
  color: white !important;
  z-index: 2000 !important;
}

.selectize-control.single .selectize-input:not(.no-arrow):after {
    border-color: white transparent transparent transparent;
}

.info-card {
  background: #333;
  padding: 1rem 2rem;
  width: fit-content;
  border-radius: 1rem;
  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.5);
  text-align: center;
  line-height: 1;
  height: 12rem;
  display: flex;
  flex-direction: column;
  justify-content: center;
  position: relative;
}

.bold {
  font-weight: bold;
  font-size: 4rem;
  padding-bottom: 0.5rem;
  color: limegreen;
}

.run-table {
  color: white !important;
}

i.icon {
  font-size: 1.5em;
  color: limegreen;
}

.info-card i.icon {
  font-size: 4rem;
  position: absolute;
  top: -1rem;
  right: -1rem;
  color: white;
}
"
      )),
    tabsetPanel(
      id = "tabs",
      type = "tabs",
      selected = "home",
        tabPanel(title = "home",
                 fluidPage(
                   titlePanel("Lifetime Stats"),
                   tags$div(
                     style = "display: flex; gap: 1rem",
                     # First element
                     tags$div(
                       htmlOutput("num.runs")
                     ),
                     # Second element
                     tags$div(
                       htmlOutput("total.distance")
                     ),
                     # Third element
                     tags$div(
                       htmlOutput("total.time")
                     ),
                     # Fourth element
                     tags$div(
                       htmlOutput("average.pace")
                     ),
                   ),
                   tags$div(
                     style = "margin-block: 1rem;"
                   ),
                   tags$div(
                     style = "display: grid; gap: 1rem; grid-template-columns: 1fr 1fr;",
                     # First element
                     tags$div(
                       plotlyOutput("pace.chart", height = "200px") %>% 
                         withSpinner(color="#0dc5c1")
                     ),
                     # Second element
                     tags$div(
                       plotlyOutput("speed.chart", height = "200px") %>% 
                         withSpinner(color="#0dc5c1")
                     ),
                   ),
                   h2("Last 5 Runs Stats"),
                   tags$div(
                     style = "margin-block: 1rem;"
                   ),
                   tags$div(
                     style = "display: flex; gap: 1rem",
                     # Second element
                     tags$div(
                       htmlOutput("total.distance.last.five")
                     ),
                     # Third element
                     tags$div(
                       htmlOutput("total.time.last.five")
                     ),
                     # Fourth element
                     tags$div(
                       htmlOutput("average.pace.last.five")
                     ),
                   ),
                   fluidRow(
                     column(width = 16,
                            tags$div(
                              style = "margin-block: 1rem;"
                            ),
                            dataTableOutput("runs.table") %>% 
                              withSpinner(color="#0dc5c1")
                     )
                   )
                 )
        ),
        tabPanel(title = "runs",
                 fluidRow(  # Wrap elements in fluidRow
                   column(width = 16, 
                          selectInput("run.selector", label = "Select Run:", choices = run.list, selected = 1),
                          tags$div(
                            style = "margin-block: 1rem;"
                          ),
                          leafletOutput("run.map") %>% 
                            withSpinner(color="#0dc5c1"), 
                          tags$div(
                            style = "margin-block: 2rem;"
                          ),
                          tags$div(
                            style = "display: flex; gap: 1rem",
                            htmlOutput("run.dist"),
                            htmlOutput("run.time"),
                            htmlOutput("run.date"),
                            htmlOutput("run.pace"),
                            htmlOutput("run.speed")
                          ),
                          tags$div(
                            style = "margin-block: 1rem;"
                          ),
                          plotlyOutput("elevation.chart", height = "200px") %>% 
                            withSpinner(color="#0dc5c1")
                   )
                 )
        ),
        tabPanel(title = "featured",
                 fluidRow(column(width = 16,
                                 h1("Featured Runs"))),
                 fluidRow(
                   column(width = 4,
                          h2("Longest Run"),
                          htmlOutput("longest.run")
                   ),
                   column(width = 12,
                          leafletOutput("longest.run.map") %>% 
                            withSpinner(color="#0dc5c1")
                   )
                 ),
                 fluidRow(
                   column(width = 12,
                          leafletOutput("shortest.run.map") %>% 
                            withSpinner(color="#0dc5c1")
                   ),
                   column(width = 4,
                          h2("Shortest Run"),
                          htmlOutput("shortest.run")
                   )
                 ),
                 fluidRow(
                   column(width = 4,
                          h2("Fastest Run"),
                          htmlOutput("fastest.run")
                   ),
                   column(width = 12,
                          leafletOutput("fastest.run.map") %>% 
                            withSpinner(color="#0dc5c1")
                   )
                 ),
                 fluidRow(
                   column(width = 12,
                          leafletOutput("slowest.run.map") %>% 
                            withSpinner(color="#0dc5c1")
                   ),
                   column(width = 4,
                          h2("Slowest Run"),
                          htmlOutput("slowest.run")
                   )
                 ),
                 fluidRow(
                   column(width = 4,
                          h2("Highest Elevation Gain"),
                          htmlOutput("elevated.run")
                   ),
                   column(width = 12,
                          leafletOutput("elevated.run.map") %>% 
                            withSpinner(color="#0dc5c1")
                   )
                 ))
    ))
}

server <- function(input, output, session) {
  setBookmarkExclude(c("runs.table_rows_selected", "runs.table_columns_selected", "runs.table_cells_selected", "runs.table_rows_current", "runs.table_rows_all", "runs.table_state", "runs.table_search", "runs.table_cell_clicked", ".clientValue-default-plotlyCrosstalkOpts", "plotly_afterplot-A", "run.map_bounds", "run.map_center", "run.map_zoom", "plotly_relayout-A", "plotly_hover-A", "longest.run.map_bounds", "longest.run.map_center", "longest.run.map_zoom", "shortest.run.map_bounds", "shortest.run.map_center", "shortest.run.map_zoom","fastest.run.map_bounds", "fastest.run.map_center", "fastest.run.map_zoom","slowest.run.map_bounds", "slowest.run.map_center", "slowest.run.map_zoom","elevated.run.map_bounds", "elevated.run.map_center", "elevated.run.map_zoom", "longest.run.map_shape_mouseover", "shortest.run.map_shape_mouseover", "fastest.run.map_shape_mouseover", "slowest.run.map_shape_mouseover", "elevated.run.map_shape_mouseover", "longest.run.map_shape_mouseout", "shortest.run.map_shape_mouseout", "fastest.run.map_shape_mouseout", "slowest.run.map_shape_mouseout", "elevated.run.map_shape_mouseout", "longest.run.map_shape_mouseout", "shortest.run.map_shape_mouseout", "fastest.run.map_shape_mouseout", "slowest.run.map_shape_mouseout", "elevated.run.map_shape_mouseout", "longest.run.map_shape_marker_mouseover", "shortest.run.map_shape_marker_mouseover", "fastest.run.map_shape_marker_mouseover", "slowest.run.map_shape_marker_mouseover", "elevated.run.map_shape_marker_mouseover", "longest.run.map_shape_marker_mouseout", "shortest.run.map_shape_marker_mouseout", "fastest.run.map_shape_marker_mouseout", "slowest.run.map_shape_marker_mouseout", "elevated.run.map_shape_marker_mouseout"))
  
  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })
  
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
  # Filter data for the selected run
  run.data.filtered <- reactive({
    dat %>%
      filter(run == input$run.selector)
  })
  
  curr.run.stats <- reactive({
    run.stats %>% 
      filter(run == input$run.selector)
  })
  
  # Function to convert seconds to a formatted string
  format.run.time <- function(seconds, hours.only = F) {
    # Calculate hours, minutes, and seconds
    hours <- floor(seconds / 3600)
    minutes <- floor((seconds %% 3600) / 60)
    remaining.seconds <- seconds %% 60
    
    # Build the formatted string
    formatted.time <- paste0(if (hours > 0) paste0(hours, ":"), 
                             if (minutes > 0) paste0(minutes, ":"), 
                             if (remaining.seconds > 0 || (hours == 0 && minutes == 0)) paste0(remaining.seconds))
    
    if (hours.only) {
      return(hours) 
    }
    return(formatted.time)
  }
  
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
  
  output$run.dist <- renderUI({
    HTML(
      paste0("<div class='info-card'><span class='bold'>",
             round(curr.run.stats()$total.distance, 2),
             ' km</span><p>run</p></div>')
    )
  })
  
  output$run.time <- renderUI({
    HTML(
      paste0("<div class='info-card'><span class='bold'>",
             format.run.time(curr.run.stats()$total.time),
             '</span><p>running</p></div>')
    )
    
  })
  
  output$run.date <- renderUI({
    HTML(
      paste0("<div class='info-card'><span class='bold'>",
             curr.run.stats()$date,
             '</span></div>')
    )
  })
  
  output$run.pace <- renderUI({
    HTML(
      paste0("<div class='info-card'><span class='bold'>",
             round(curr.run.stats()$pace, 2),
             ' "/km</span><p>average pace</p></div>')
    )
  })
  
  output$run.speed <- renderUI({
    HTML(
      paste0("<div class='info-card'><span class='bold'>",
             round(curr.run.stats()$speed, 2),
             ' km/h</span><p>average speed</p></div>')
    )
  })
  
  output$elevation.chart <- renderPlotly({
    run.data.filtered() %>% 
      ggplot(aes(time, elevation)) +
      geom_line(color = "limegreen") + 
      labs(x = "Time", y = "Elevation (m)", title = "Elevation over Run") +
      theme(
        plot.background = element_rect(fill = "#161616"),  # Set plot background color to black
        panel.background = element_rect(fill = "#161616"), # Set panel background color to black
        panel.border = element_blank(),                   # Remove panel border
        axis.text = element_text(color = "white"),        # Set axis text color to white
        axis.title = element_text(color = "white"),       # Set axis title color to white
        title = element_text(color = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  output$num.runs <- renderUI({
    HTML(
      paste0("<div class='info-card'><span class='bold'>",
             nrow(run.stats),
             "</span><p>total runs</p></div>")
    )
  })
  
  output$total.distance <- renderUI({
    HTML(
      paste0("<div class='info-card'><span class='bold'>",
             round(sum(run.stats$total.distance), 2),
             " km</span><p>ran</p></div>")
    )
  })
  
  output$total.distance.last.five <- renderUI({
    HTML(
      paste0("<div class='info-card'><span class='bold'>",
             round(sum(head(run.stats, 5)$total.distance), 2),
             " km</span><p>ran</p></div>")
    )
  })
  
  output$total.time <- renderUI({
    HTML(
      paste0("<div class='info-card'><span class='bold'>>",
             format.run.time(sum(run.stats$total.time), hours.only = T),
             " hours</span><p>spent running</p></div>")
    )
  })
  
  output$total.time.last.five <- renderUI({
    HTML(
      paste0("<div class='info-card'><span class='bold'>>",
             format.run.time(sum(head(run.stats, 5)$total.time), hours.only = T),
             " hours</span><p>spent running</p></div>")
    )
  })
  
  output$average.pace <- renderUI({
    HTML(
      paste0("<div class='info-card'><span class='bold'>",
             round(mean(run.stats$pace), 2),
             ' "/km</span><p>average pace</p></div>')
    )
  })
  
  output$average.pace.last.five <- renderUI({
    HTML(
      paste0("<div class='info-card'><span class='bold'>",
             round(mean(head(run.stats, 5)$pace), 2),
             ' "/km</span><p>average pace</p></div>')
    )
  })
  
  output$runs.table <- renderDataTable({
    datatable(run.stats, class = "run-table", options = list(searching = F), selection = "none")
  })
  
  output$pace.chart <- renderPlotly({
    run.stats %>% 
      ggplot(aes(date, pace)) +
      geom_line(color = "limegreen") +
      labs(x = "Date", y = "Pace (\"/km)", title = "Pace over Runs") +
      theme(
        plot.background = element_rect(fill = "#161616"),  # Set plot background color to black
        panel.background = element_rect(fill = "#161616"), # Set panel background color to black
        panel.border = element_blank(),                   # Remove panel border
        axis.text = element_text(color = "white"),        # Set axis text color to white
        axis.title = element_text(color = "white"),       # Set axis title color to white
        title = element_text(color = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  output$speed.chart <- renderPlotly({
    run.stats %>% 
      ggplot(aes(date, speed)) +
      geom_line(color = "limegreen") +
      labs(x = "Date", y = "Speed (km/h)", title = "Speed over Runs") +
      theme(
        plot.background = element_rect(fill = "#161616"),  # Set plot background color to black
        panel.background = element_rect(fill = "#161616"), # Set panel background color to black
        panel.border = element_blank(),                   # Remove panel border
        axis.text = element_text(color = "white"),        # Set axis text color to white
        axis.title = element_text(color = "white"),       # Set axis title color to white
        title = element_text(color = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  longest.run.sum <- run.stats %>% 
    arrange(desc(total.distance)) %>% 
    head(1)
  
  longest.run <- dat %>% 
    filter(run == longest.run.sum$run)
  
  output$longest.run <- renderUI({
    HTML(paste0("<div class='info-card'><span class='bold'>Run #", longest.run.sum$run, "</span><p>", round(longest.run.sum$total.distance, 2), "km run on ", icon("angle double right"), as.character(longest.run.sum$date), "</p></div>"))
  })
  
  output$longest.run.map <- renderLeaflet({
    # Get coordinates for the run
    lat <- longest.run$lat
    lng <- longest.run$lng
    
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
      addPolylines(lng = lng, lat = lat, data = longest.run, color = "limegreen") %>% # Plot run path
      addMarkers(lng = lng[1], lat = lat[1], icon = start.icon) %>% 
      addMarkers(lng = lng[length(lng)], lat = lat[length(lat)], icon = finish.icon)
  })
  
  shortest.run.sum <- run.stats %>% 
    arrange(total.distance) %>% 
    head(1)
  
  shortest.run <- dat %>% 
    filter(run == shortest.run.sum$run)
  
  output$shortest.run <- renderUI({
    HTML(paste0("<div class='info-card'><span class='bold'>Run #", shortest.run.sum$run, "</span><p>", round(shortest.run.sum$total.distance, 2), "km run on ", icon("angle right"), as.character(shortest.run.sum$date), "</p></div>"))
  })
  
  output$shortest.run.map <- renderLeaflet({
    # Get coordinates for the run
    lat <- shortest.run$lat
    lng <- shortest.run$lng
    
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
      addPolylines(lng = lng, lat = lat, data = shortest.run, color = "limegreen") %>% # Plot run path
      addMarkers(lng = lng[1], lat = lat[1], icon = start.icon) %>% 
      addMarkers(lng = lng[length(lng)], lat = lat[length(lat)], icon = finish.icon)
  })
  
  fastest.run.sum <- run.stats %>% 
    arrange(pace) %>% 
    head(1)
  
  fastest.run <- dat %>% 
    filter(run == fastest.run.sum$run)
  
  output$fastest.run <- renderUI({
    HTML(paste0("<div class='info-card'><span class='bold'>Run #", fastest.run.sum$run, "</span><p>", icon("shipping fast"), round(fastest.run.sum$pace, 2), " \"/km pace on ", as.character(fastest.run.sum$date), "</p></div>"))
  })
  
  output$fastest.run.map <- renderLeaflet({
    # Get coordinates for the run
    lat <- fastest.run$lat
    lng <- fastest.run$lng
    
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
      addPolylines(lng = lng, lat = lat, data = fastest.run, color = "limegreen") %>% # Plot run path
      addMarkers(lng = lng[1], lat = lat[1], icon = start.icon) %>% 
      addMarkers(lng = lng[length(lng)], lat = lat[length(lat)], icon = finish.icon)
  })
  
  slowest.run.sum <- run.stats %>% 
    arrange(desc(pace)) %>% 
    head(1)
  
  slowest.run <- dat %>% 
    filter(run == slowest.run.sum$run)
  
  output$slowest.run <- renderUI({
    HTML(paste0("<div class='info-card'><span class='bold'>Run #", slowest.run.sum$run, "</span><p>", round(slowest.run.sum$pace, 2), icon("hourglass outline"), " \"/km pace on ", as.character(slowest.run.sum$date), "</p></div>"))
  })
  
  output$slowest.run.map <- renderLeaflet({
    # Get coordinates for the run
    lat <- slowest.run$lat
    lng <- slowest.run$lng
    
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
      addPolylines(lng = lng, lat = lat, data = slowest.run, color = "limegreen") %>% # Plot run path
      addMarkers(lng = lng[1], lat = lat[1], icon = start.icon) %>% 
      addMarkers(lng = lng[length(lng)], lat = lat[length(lat)], icon = finish.icon)
  })
  
  elevated.run.sum <- run.stats %>% 
    arrange(desc(abs(elevation.gain))) %>% 
    head(1)
  
  elevated.run <- dat %>% 
    filter(run == elevated.run.sum$run)
  
  output$elevated.run <- renderUI({
    HTML(paste0("<div class='info-card'><span class='bold'>Run #", elevated.run.sum$run, "</span><p>", round(elevated.run.sum$elevation.gain, 2), "m gained on ", icon("chart line"), as.character(elevated.run.sum$date), "</p></div>"))
  })
  
  output$elevated.run.map <- renderLeaflet({
    # Get coordinates for the run
    lat <- elevated.run$lat
    lng <- elevated.run$lng
    
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
      addPolylines(lng = lng, lat = lat, data = elevated.run, color = "limegreen") %>% # Plot run path
      addMarkers(lng = lng[1], lat = lat[1], icon = start.icon) %>% 
      addMarkers(lng = lng[length(lng)], lat = lat[length(lat)], icon = finish.icon)
  })
}

shinyApp(ui, server, enableBookmarking = "url")