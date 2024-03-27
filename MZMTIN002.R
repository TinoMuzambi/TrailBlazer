library(shiny)
library(tidyverse)
library(leaflet)
library(plotly)
library(semantic.dashboard)
library(shinycssloaders)
library(viridis)
library(leaflet.extras)
library(DT)

# Read in data.
dat <- list.files("data/", "*.csv", full.names = T) %>% 
  read_csv(., id = "run") %>% 
  # Add column for representing run and parse date & time fields into time field.
  mutate(run = dense_rank(run), time = ymd_hms(paste(date, time)))

# Group by run and get summary statistics for each run.
run.stats <- dat %>%
  group_by(run) %>%
  # Use 'Haversine' great circle distance to get cumulative distances between consecutive points.
  mutate(dist = distHaversine(cbind(lag(lng), lag(lat)), cbind(lng, lat))) %>%
  summarise(
    total.distance = sum(dist, na.rm = T),
    total.time = as.numeric(difftime(last(time), first(time), units = "secs")),
    elevation.gain = sum(diff(elevation[elevation > lag(elevation, default = first(elevation))]), na.rm = T),
    date = as.character(first(date))
  ) %>% 
  # Convert distance to km.
  mutate(total.distance = total.distance / 1000) %>% 
  mutate(pace = (as.numeric(total.time) / 60) / total.distance) %>% 
  mutate(speed = total.distance /  (as.numeric(total.time) / 3600)) %>% 
  mutate(date = ymd(date))

# Get unique list of all runs to use for select input.
run.list <- dat %>%
  distinct(run, .keep_all = TRUE) %>%
  pull(run)

ui <- function(req) {
  fluidPage(
    title = "Trailblazer",
    sidebarLayout(
      sidebarPanel(
        width = 2,
        # Use radio buttons as navigation buttons.
        radioButtons(inputId = "tabswitcher", 
                     label = "TrailBlazer",
                     choiceNames = list("Home", 
                                        tags$div(
                                          tags$p("Run",
                                                 style = "margin: 0;"),
                                        # Select input for filtering to a specific run.
                                        selectInput("run.selector", 
                                                    label = "Select Run:",
                                                    choices = run.list, 
                                                    selected = 1),
                                        ),
                                        "Featured"),
                     choiceValues = list("home", "run", "featured"),
                     selected = "home"),
      ),
      mainPanel(
        width = 10,
        # Custom CSS styles.
        tags$head(
          tags$style(
            "
body, form.well {
  background-color: rgb(22, 22, 22) !important;
  color: white !important;
}

a.item {
  color: white !important;
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
  background-color: rgb(22, 22, 22) !important;
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
  height: 12rem;
  display: flex;
  flex-direction: column;
  justify-content: center;
  position: relative;
}

.info-card a {
  color: inherit;
  text-decoration: none;
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

i {
  font-size: 1.5em;
  color: limegreen;
}

.info-card i {
  font-size: 4rem;
  position: absolute;
  top: -1rem;
  right: -1rem;
  color: white;
}

form.well {
  border: none;
}

form.well input[type=radio] {
  visibility: hidden;
}

h3 {
  margin-top: 0 !important;
}

#tabswitcher, #tabswitcher-label {
  font-size: 3rem !important;
}

.radio label {
  padding-left: 0 !important;
}

.shiny-input-container .control-label, .shiny-input-container .shiny-input-select {
  font-size: 1rem !important;
}
"
          ),
          tags$link(rel = "shortcut icon", href = "logo.png", type = "image/png")
        ),
        # Use hidden tabsetPanel controlled by radio buttons for different app tabs.
        tabsetPanel(
          id = "tabs",
          type = "hidden",
          # Home tab.
          tabPanelBody("home",
                       fluidPage(
                         # Use display flex to center items.
                         tags$div(
                           style = "display: flex; flex-direction: column; gap: 1rem; justify-content: center; align-items: center;",
                           
                           tags$h1(
                             style = "font-size: 12rem; text-align: center;",
                            "Welcome to TrailBlazer"  
                           ),
                           
                           tags$img(
                             style = "width: 45rem; margin-block: 2rem;",
                             src = "runners.png",
                             alt = "Runners"
                           )
                         ),
                         
                         # Use display flex to place items next to each other.
                         tags$div(
                           style = "display: flex; gap: 1rem",
                           
                           # Output card for total number of runs.
                           tags$div(
                             htmlOutput("num.runs")
                           ),
                           
                           # Output card for total distance run.
                           tags$div(
                             htmlOutput("total.distance")
                           ),
                           
                           # Output card for total time run.
                           tags$div(
                             htmlOutput("total.time")
                           ),
                           
                           # Output card for average pace across runs.
                           tags$div(
                             htmlOutput("average.pace")
                           ),
                         ),
                         
                         # Spacer.
                         tags$div(
                           style = "margin-block: 1rem;"
                         ),
                         
                         # Use display grid to place items next to each other.
                         tags$div(
                           style = "display: grid; gap: 1rem; grid-template-columns: 1fr 1fr;",
                           
                           # Output line chart for pace over time.
                           tags$div(
                             plotlyOutput("pace.chart", height = "200px") %>% 
                               withSpinner(color="limegreen")
                           ),
                           
                           # Output line chart for speed over time.
                           tags$div(
                             plotlyOutput("speed.chart", height = "200px") %>% 
                               withSpinner(color="limegreen")
                           ),
                         ),
                         
                         h2("Last 5 Runs Stats"),
                         
                         # Spacer.
                         tags$div(
                           style = "margin-block: 1rem;"
                         ),
                         
                         # Use display flex to place items next to each other.
                         tags$div(
                           style = "display: flex; gap: 1rem",
                           
                           # Output card for total distance run over last five runs.
                           tags$div(
                             htmlOutput("total.distance.last.five")
                           ),
                           
                           # Output card for total time run over last five runs.
                           tags$div(
                             htmlOutput("total.time.last.five")
                           ),
                           
                           # Output card for average pace over last five runs.
                           tags$div(
                             htmlOutput("average.pace.last.five")
                           ),
                         ),
                         
                         fluidRow(
                           column(width = 16,
                                  # Spacer.
                                  tags$div(
                                    style = "margin-block: 1rem;"
                                  ),
                                  
                                  # Output datatable for all runs.
                                  dataTableOutput("runs.table") %>% 
                                    withSpinner(color="limegreen")
                           )
                         )
                       )
          ),
          # Run tab.
          tabPanelBody("run",
                       fluidPage(
                         fluidRow(
                           column(width = 16,
                                  # Spacer.
                                  tags$div(
                                    style = "margin-block: 2rem;"
                                  ),
                                  
                                  # Spacer.
                                  tags$div(
                                    style = "margin-block: 1rem;"
                                  ),
                                  
                                  # Output Leaflet map for selected run.
                                  leafletOutput("run.map") %>% 
                                    withSpinner(color="limegreen"), 
                                  
                                  # Spacer.
                                  tags$div(
                                    style = "margin-block: 2rem;"
                                  ),
                                  
                                  # Use display flex to place items next to each other.
                                  tags$div(
                                    style = "display: flex; gap: 1rem",
                                    
                                    # Output card for distance ran for selected run.
                                    htmlOutput("run.dist"),
                                    
                                    # Output card for time ran for selected run.
                                    htmlOutput("run.time"),
                                    
                                    # Output card for run date for selected run.
                                    htmlOutput("run.date"),
                                    
                                    # Output card for average pace for selected run.
                                    htmlOutput("run.pace"),
                                    
                                    # Output card for average speed for selected run.
                                    htmlOutput("run.speed")
                                  ),
                                  
                                  # Spacer.
                                  tags$div(
                                    style = "margin-block: 1rem;"
                                  ),
                                  
                                  # Output line chart for elevation over run.
                                  plotlyOutput("elevation.chart", height = "200px") %>% 
                                    withSpinner(color="limegreen")
                           )
                         )
                       )
          ),
          # Featured tab.
          tabPanelBody("featured",
                       fluidPage(
                         titlePanel("Featured Runs"),
                         
                         # Use display flex to place items next to each other.
                         tags$div(
                           style = "display: flex; gap: 1rem",
                           
                           # Output card for longest run.
                           tags$div(
                             style = "flex: 1;",
                             h3("Longest Run"),
                             htmlOutput("longest.run")
                           ),
                           
                           # Output Leaflet map for longest run.
                           tags$div(
                             style = "flex: 4;",
                             leafletOutput("longest.run.map") %>% 
                               withSpinner(color="limegreen"),
                             tags$div(
                               style = "margin-block: 2rem;"
                             )
                           ),
                         ),
                         
                         # Use display flex to place items next to each other.
                         tags$div(
                           style = "display: flex; gap: 1rem",
                           # Output Leaflet map for shortest run.
                           tags$div(
                             style = "flex: 4;",
                             leafletOutput("shortest.run.map") %>% 
                               withSpinner(color="limegreen"),
                             
                             # Spacer.
                             tags$div(
                               style = "margin-block: 2rem;"
                             )
                           ),
                           
                           # Output card for shortest run.
                           tags$div(
                             style = "flex: 1;",
                             h3("Shortest Run"),
                             htmlOutput("shortest.run")
                           ),
                         ),
                         
                         # Use display flex to place items next to each other.
                         tags$div(
                           style = "display: flex; gap: 1rem",
                           
                           # Output card for fastest run.
                           tags$div(
                             style = "flex: 1;",
                             h3("Fastest Run"),
                             htmlOutput("fastest.run")
                           ),
                           
                           # Output Leaflet map for fastest run.
                           tags$div(
                             style = "flex: 4;",
                             leafletOutput("fastest.run.map") %>% 
                               withSpinner(color="limegreen"),
                             
                             # Spacer.
                             tags$div(
                               style = "margin-block: 2rem;"
                             )
                           ),
                         ),
                         
                         # Use display flex to place items next to each other.
                         tags$div(
                           style = "display: flex; gap: 1rem",
                           
                           # Output Leaflet map for slowest run.
                           tags$div(
                             style = "flex: 4;",
                             leafletOutput("slowest.run.map") %>% 
                               withSpinner(color="limegreen"),
                             
                             # Spacer.
                             tags$div(
                               style = "margin-block: 2rem;"
                             )
                           ),
                           
                           # Output card for slowest run.
                           tags$div(
                             style = "flex: 1;",
                             h3("Slowest Run"),
                             htmlOutput("slowest.run")
                           ),
                         ),
                         
                         # Use display flex to place items next to each other.
                         tags$div(
                           style = "display: flex; gap: 1rem",
                           
                           # Output card for highest elevation gained.
                           tags$div(
                             style = "flex: 1;",
                             h3("Highest Elevation Gain"),
                             htmlOutput("elevated.run")
                           ),
                           
                           # Output Leaflet map for highest elevation gained.
                           tags$div(
                             style = "flex: 4;",
                             leafletOutput("elevated.run.map") %>% 
                               withSpinner(color="limegreen"),
                             
                             # Spacer.
                             tags$div(
                               style = "margin-block: 2rem;"
                             )
                           ),
                         ),
                       )
          )
        )
      )
    )
  )
}

server <- function(input, output, session) {
  # Exclude inputs from being bookmarked in the URL.
  setBookmarkExclude(c("runs.table_rows_selected", "runs.table_columns_selected", "runs.table_cells_selected", "runs.table_rows_current", "runs.table_rows_all", "runs.table_state", "runs.table_search", "runs.table_cell_clicked", ".clientValue-default-plotlyCrosstalkOpts", "plotly_afterplot-A", "run.map_bounds", "run.map_center", "run.map_zoom", "plotly_relayout-A", "plotly_hover-A", "longest.run.map_bounds", "longest.run.map_center", "longest.run.map_zoom", "shortest.run.map_bounds", "shortest.run.map_center", "shortest.run.map_zoom","fastest.run.map_bounds", "fastest.run.map_center", "fastest.run.map_zoom","slowest.run.map_bounds", "slowest.run.map_center", "slowest.run.map_zoom","elevated.run.map_bounds", "elevated.run.map_center", "elevated.run.map_zoom", "longest.run.map_shape_mouseover", "shortest.run.map_shape_mouseover", "fastest.run.map_shape_mouseover", "slowest.run.map_shape_mouseover", "elevated.run.map_shape_mouseover", "longest.run.map_shape_mouseout", "shortest.run.map_shape_mouseout", "fastest.run.map_shape_mouseout", "slowest.run.map_shape_mouseout", "elevated.run.map_shape_mouseout", "longest.run.map_shape_mouseout", "shortest.run.map_shape_mouseout", "fastest.run.map_shape_mouseout", "slowest.run.map_shape_mouseout", "elevated.run.map_shape_mouseout", "longest.run.map_shape_marker_mouseover", "shortest.run.map_shape_marker_mouseover", "fastest.run.map_shape_marker_mouseover", "slowest.run.map_shape_marker_mouseover", "elevated.run.map_shape_marker_mouseover", "longest.run.map_shape_marker_mouseout", "shortest.run.map_shape_marker_mouseout", "fastest.run.map_shape_marker_mouseout", "slowest.run.map_shape_marker_mouseout", "elevated.run.map_shape_marker_mouseout"))
  
  # Observer for reacting and bookmarking desired input changes in the URL for persistence across refreshes.
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  
  # Callback function to sync the URL with bookmarked inputs.
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
  # Handler for using radio buttons to switch tabs.
  observeEvent(input$tabswitcher, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = paste0(input$tabswitcher))
  })
  
  # Filter full data for the selected run.
  run.data.filtered <- reactive({
    dat %>%
      filter(run == input$run.selector)
  })
  
  # Filter summarised data for the selected run.
  curr.run.stats <- reactive({
    run.stats %>% 
      filter(run == input$run.selector)
  })
  
  # Function to convert seconds to a formatted string.
  format.run.time <- function(seconds, hours.only = F) {
    # Calculate hours, minutes, and seconds
    hours <- floor(seconds / 3600)
    minutes <- floor((seconds %% 3600) / 60)
    remaining.seconds <- seconds %% 60
    
    # Build the formatted string
    formatted.time <- paste0(if (hours > 0) if (hours < 10) paste0("0", hours, ":") else paste0(hours, ":"), 
                             if (minutes > 0) if (minutes < 10) paste0("0", minutes, ":") else paste0(minutes, ":"), 
                             if (remaining.seconds > 0 || (hours == 0 && minutes == 0)) if (remaining.seconds < 10) paste0("0", remaining.seconds) else paste0(remaining.seconds))
    
    if (hours.only) {
      return(hours) 
    }
    return(formatted.time)
  }
  
  # Define start icon to be used on run maps.
  start.icon <- makeIcon(
    iconUrl = "www/start.png",
    iconWidth = 32, iconHeight = 32,
    iconAnchorX = 16, iconAnchorY = 32,
  )
  
  # Define finish icon to be used on run maps.
  finish.icon <- makeIcon(
    iconUrl = "www/finish.png",
    iconWidth = 32, iconHeight = 32,
    iconAnchorX = 16, iconAnchorY = 32
  )
  
  # Render Leaflet map for selected run.
  output$run.map <- renderLeaflet({
    lat <- run.data.filtered()$lat
    lng <- run.data.filtered()$lng
    elevation <- run.data.filtered()$elevation
    
    # Create a color vector based on elevation using the viridis color scale.
    pal <- viridis(n = 7, option = "viridis")
    # Match colors to elevation values.
    colors <- pal[order(elevation)]  
    
    # Create the legend.
    elevation.pal <- colorBin(
      palette = pal,
      domain = range(dat$elevation),
      bins = 7,
      pretty = TRUE
    )
    
    # Create the Leaflet map.
    leaflet() %>%
      setView(lng = mean(lng), lat = mean(lat), zoom = 13) %>%
      addTiles() %>% # Add base map tiles
      addPolylines(lng = lng, lat = lat, data = run.data.filtered(), color = colors) %>% # Plot run path
      addMarkers(lng = lng[1], lat = lat[1], icon = start.icon) %>% 
      addMarkers(lng = lng[length(lng)], lat = lat[length(lat)], icon = finish.icon) %>% 
      addLegend(position = "bottomright", pal = elevation.pal, values = range(elevation),
                title = "Elevation (m)", opacity = 1)
  })
  
  # Render card for run distance.
  output$run.dist <- renderUI({
    HTML(
      paste0("<div class='info-card'><span class='bold'>",
             round(curr.run.stats()$total.distance, 2),
             ' km</span><p>run</p></div>')
    )
  })
  
  # Render card for run time.
  output$run.time <- renderUI({
    HTML(
      paste0("<div class='info-card'><span class='bold'>",
             format.run.time(curr.run.stats()$total.time),
             '</span><p>running</p></div>')
    )
    
  })
  
  # Render card for run date.
  output$run.date <- renderUI({
    HTML(
      paste0("<div class='info-card'><span class='bold'>",
             curr.run.stats()$date,
             '</span></div>')
    )
  })
  
  # Render card for run pace.
  output$run.pace <- renderUI({
    HTML(
      paste0("<div class='info-card'><span class='bold'>",
             round(curr.run.stats()$pace, 2),
             ' "/km</span><p>average pace</p></div>')
    )
  })
  
  # Render card for run speed.
  output$run.speed <- renderUI({
    HTML(
      paste0("<div class='info-card'><span class='bold'>",
             round(curr.run.stats()$speed, 2),
             ' km/h</span><p>average speed</p></div>')
    )
  })
  
  # Render chart for run elevation.
  output$elevation.chart <- renderPlotly({
    run.data.filtered() %>% 
      ggplot(aes(time, elevation)) +
      geom_line(color = "limegreen") + 
      labs(x = "Time", y = "Elevation (m)", title = "Elevation over Run") +
      theme(
        plot.background = element_rect(fill = "#161616"),
        panel.background = element_rect(fill = "#161616"),
        panel.border = element_blank(),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        title = element_text(color = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  # Render card for total number of runs.
  output$num.runs <- renderUI({
    HTML(
      paste0("<div class='info-card'><span class='bold'>",
             nrow(run.stats),
             "</span><p>total runs</p></div>")
    )
  })
  
  # Render card for total run distance.
  output$total.distance <- renderUI({
    HTML(
      paste0("<div class='info-card'><span class='bold'>",
             round(sum(run.stats$total.distance), 2),
             " km</span><p>ran</p></div>")
    )
  })
  
  # Render card for total time.
  output$total.time <- renderUI({
    HTML(
      paste0("<div class='info-card'><span class='bold'>>",
             format.run.time(sum(run.stats$total.time), hours.only = T),
             " hours</span><p>spent running</p></div>")
    )
  })
  
  # Render card for average pace.
  output$average.pace <- renderUI({
    HTML(
      paste0("<div class='info-card'><span class='bold'>",
             round(mean(run.stats$pace), 2),
             ' "/km</span><p>average pace</p></div>')
    )
  })
  
  # Render line chart for pace.
  output$pace.chart <- renderPlotly({
    run.stats %>% 
      ggplot(aes(date, pace)) +
      geom_line(color = "limegreen") +
      labs(x = "Date", y = "Pace (\"/km)", title = "Pace over Runs") +
      theme(
        plot.background = element_rect(fill = "#161616"),
        panel.background = element_rect(fill = "#161616"),
        panel.border = element_blank(),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        title = element_text(color = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  # Render line chart for speed.
  output$speed.chart <- renderPlotly({
    run.stats %>% 
      ggplot(aes(date, speed)) +
      geom_line(color = "limegreen") +
      labs(x = "Date", y = "Speed (km/h)", title = "Speed over Runs") +
      theme(
        plot.background = element_rect(fill = "#161616"),
        panel.background = element_rect(fill = "#161616"),
        panel.border = element_blank(),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        title = element_text(color = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  # Get five most recent runs.
  last.five.runs <- run.stats %>% 
    arrange(desc(date)) %>% 
    head(5)
  
  # Render card for total run distance for last five runs.
  output$total.distance.last.five <- renderUI({
    HTML(
      paste0("<div class='info-card'><span class='bold'>",
             round(sum(last.five.runs$total.distance), 2),
             " km</span><p>ran</p></div>")
    )
  })
  
  # Render card for total time for last five runs.
  output$total.time.last.five <- renderUI({
    HTML(
      paste0("<div class='info-card'><span class='bold'>>",
             format.run.time(sum(last.five.runs$total.time), hours.only = T),
             " hours</span><p>spent running</p></div>")
    )
  })
  
  # Render card for average pace for last five runs.
  output$average.pace.last.five <- renderUI({
    HTML(
      paste0("<div class='info-card'><span class='bold'>",
             round(mean(last.five.runs$pace), 2),
             ' "/km</span><p>average pace</p></div>')
    )
  })
  
  # Render data table for all runs.
  output$runs.table <- renderDataTable({
    datatable(run.stats %>% 
                mutate(total.distance = round(total.distance, 2),
                       elevation.gain = round(elevation.gain, 2),
                       pace = round(pace, 2),
                       speed = round(speed, 2)) %>% 
                dplyr::rename("Run #" = run, 
                              "Total Distance (km)" = total.distance, 
                              "Total Time (s)" = total.time, 
                              "Elevation Gain (m)" = elevation.gain, 
                              "Date" = date, 
                              "Average Pace (\"/km)" = pace, 
                              "Average Speed (km/h)" = speed),
              class = "run-table",
              options = list(searching = F),
              selection = "none",
              rownames = F)
  })
  
  # Get longest run from summary runs.
  longest.run.sum <- run.stats %>% 
    arrange(desc(total.distance)) %>% 
    head(1)
  
  # Get longest run from all runs.
  longest.run <- dat %>% 
    filter(run == longest.run.sum$run)
  
  # Render card for longest run.
  output$longest.run <- renderUI({
    HTML(paste0("<div class='info-card'><a href='?_inputs_&tabs=\"run\"&go.runs=1&run.selector=\"", longest.run.sum$run, "\"&tabswitcher=\"run\"'><span class='bold'>Run #", longest.run.sum$run, "</span><p>", round(longest.run.sum$total.distance, 2), "km run on ", as.character(longest.run.sum$date), "</p></a>", shiny::icon("angles-right"), "</div>"))
  })
  
  # Render Leaflet map for longest run.
  output$longest.run.map <- renderLeaflet({
    lat <- longest.run$lat
    lng <- longest.run$lng
    elevation <- longest.run$elevation
    
    # Create a color vector based on elevation using the viridis color scale.
    pal <- viridis(n = 7, option = "viridis")
    # Match colors to elevation values.
    colors <- pal[order(elevation)]  
    
    # Create the legend.
    elevation.pal <- colorBin(
      palette = pal,
      domain = range(dat$elevation),
      bins = 7,
      pretty = TRUE
    )
    
    # Create the Leaflet map
    leaflet() %>%
      setView(lng = mean(lng), lat = mean(lat), zoom = 13) %>%
      addTiles() %>% # Add base map tiles
      addPolylines(lng = lng, lat = lat, data = longest.run, color = "limegreen") %>% # Plot run path
      addMarkers(lng = lng[1], lat = lat[1], icon = start.icon) %>% 
      addMarkers(lng = lng[length(lng)], lat = lat[length(lat)], icon = finish.icon) %>% 
      addLegend(position = "bottomright", pal = elevation.pal, values = range(elevation),
                title = "Elevation (m)", opacity = 1)
  })
  
  # Get shortest run from summary runs.
  shortest.run.sum <- run.stats %>% 
    arrange(total.distance) %>% 
    head(1)
  
  # Get shortest run from all runs.
  shortest.run <- dat %>% 
    filter(run == shortest.run.sum$run)
  
  # Render card for shortest run.
  output$shortest.run <- renderUI({
    HTML(paste0("<div class='info-card'><a href='?_inputs_&tabs=\"run\"&go.runs=1&run.selector=\"", shortest.run.sum$run, "\"&tabswitcher=\"run\"'><span class='bold'>Run #", shortest.run.sum$run, "</span><p>", round(shortest.run.sum$total.distance, 2), "km run on ", icon("angle right"), as.character(shortest.run.sum$date), "</p></a></div>"))
  })
  
  # Render Leaflet map for shortest run.
  output$shortest.run.map <- renderLeaflet({
    lat <- shortest.run$lat
    lng <- shortest.run$lng
    elevation <- shortest.run$elevation
    
    # Create a color vector based on elevation using the viridis color scale.
    pal <- viridis(n = 7, option = "viridis")
    # Match colors to elevation values.
    colors <- pal[order(elevation)]  
    
    # Create the legend.
    elevation.pal <- colorBin(
      palette = pal,
      domain = range(dat$elevation),
      bins = 7,
      pretty = TRUE
    )
    
    # Create the Leaflet map
    leaflet() %>%
      setView(lng = mean(lng), lat = mean(lat), zoom = 13) %>%
      addTiles() %>% # Add base map tiles
      addPolylines(lng = lng, lat = lat, data = shortest.run, color = "limegreen") %>% # Plot run path
      addMarkers(lng = lng[1], lat = lat[1], icon = start.icon) %>% 
      addMarkers(lng = lng[length(lng)], lat = lat[length(lat)], icon = finish.icon) %>% 
      addLegend(position = "bottomright", pal = elevation.pal, values = range(elevation),
                title = "Elevation (m)", opacity = 1)
  })
  
  # Get fastest run from summary runs.
  fastest.run.sum <- run.stats %>% 
    arrange(pace) %>% 
    head(1)
  
  # Get fastest run from all runs.
  fastest.run <- dat %>% 
    filter(run == fastest.run.sum$run)
  
  # Render card for fastest run.
  output$fastest.run <- renderUI({
    HTML(paste0("<div class='info-card'><a href='?_inputs_&tabs=\"run\"&go.runs=1&run.selector=\"", fastest.run.sum$run, "\"&tabswitcher=\"run\"'><span class='bold'>Run #", fastest.run.sum$run, "</span><p>", icon("shipping fast"), round(fastest.run.sum$pace, 2), " \"/km pace on ", as.character(fastest.run.sum$date), "</p></a></div>"))
  })
  
  # Render Leaflet map for fastest run.
  output$fastest.run.map <- renderLeaflet({
    lat <- fastest.run$lat
    lng <- fastest.run$lng
    elevation <- fastest.run$elevation
    
    # Create a color vector based on elevation using the viridis color scale.
    pal <- viridis(n = 7, option = "viridis")
    # Match colors to elevation values.
    colors <- pal[order(elevation)]  
    
    # Create the legend.
    elevation.pal <- colorBin(
      palette = pal,
      domain = range(dat$elevation),
      bins = 7,
      pretty = TRUE
    )
    
    # Create the Leaflet map
    leaflet() %>%
      setView(lng = mean(lng), lat = mean(lat), zoom = 13) %>%
      addTiles() %>% # Add base map tiles
      addPolylines(lng = lng, lat = lat, data = fastest.run, color = "limegreen") %>% # Plot run path
      addMarkers(lng = lng[1], lat = lat[1], icon = start.icon) %>% 
      addMarkers(lng = lng[length(lng)], lat = lat[length(lat)], icon = finish.icon) %>% 
      addLegend(position = "bottomright", pal = elevation.pal, values = range(elevation),
                title = "Elevation (m)", opacity = 1)
  })
  
  # Get slowest run from summary runs.
  slowest.run.sum <- run.stats %>% 
    arrange(desc(pace)) %>% 
    head(1)
  
  # Get slowest run from all runs.
  slowest.run <- dat %>% 
    filter(run == slowest.run.sum$run)
  
  # Render card for slowest run.
  output$slowest.run <- renderUI({
    HTML(paste0("<div class='info-card'><a href='?_inputs_&tabs=\"run\"&go.runs=1&run.selector=\"", slowest.run.sum$run, "\"&tabswitcher=\"run\"'><span class='bold'>Run #", slowest.run.sum$run, "</span><p>", round(slowest.run.sum$pace, 2), icon("hourglass outline"), " \"/km pace on ", as.character(slowest.run.sum$date), "</p></a></div>"))
  })
  
  # Render Leaflet map for slowest run.
  output$slowest.run.map <- renderLeaflet({
    lat <- slowest.run$lat
    lng <- slowest.run$lng
    elevation <- slowest.run$elevation
    
    # Create a color vector based on elevation using the viridis color scale.
    pal <- viridis(n = 7, option = "viridis")
    # Match colors to elevation values.
    colors <- pal[order(elevation)]  
    
    # Create the legend.
    elevation.pal <- colorBin(
      palette = pal,
      domain = range(dat$elevation),
      bins = 7,
      pretty = TRUE
    )
    
    # Create the Leaflet map
    leaflet() %>%
      setView(lng = mean(lng), lat = mean(lat), zoom = 13) %>%
      addTiles() %>% # Add base map tiles
      addPolylines(lng = lng, lat = lat, data = slowest.run, color = "limegreen") %>% # Plot run path
      addMarkers(lng = lng[1], lat = lat[1], icon = start.icon) %>% 
      addMarkers(lng = lng[length(lng)], lat = lat[length(lat)], icon = finish.icon) %>% 
      addLegend(position = "bottomright", pal = elevation.pal, values = range(elevation),
                title = "Elevation (m)", opacity = 1)
  })
  
  # Get most elevated run from summary runs.
  elevated.run.sum <- run.stats %>% 
    arrange(desc(abs(elevation.gain))) %>% 
    head(1)
  
  # Get most elevated run from all runs.
  elevated.run <- dat %>% 
    filter(run == elevated.run.sum$run)
  
  # Render card for most elevated run.
  output$elevated.run <- renderUI({
    HTML(paste0("<div class='info-card'><a href='?_inputs_&tabs=\"run\"&go.runs=1&run.selector=\"", elevated.run.sum$run, "\"&tabswitcher=\"run\"'><span class='bold'>Run #", elevated.run.sum$run, "</span><p>", round(elevated.run.sum$elevation.gain, 2), "m gained on ", icon("chart line"), as.character(elevated.run.sum$date), "</p></a></div>"))
  })
  
  # Render Leaflet map for most elevated run.
  output$elevated.run.map <- renderLeaflet({
    lat <- elevated.run$lat
    lng <- elevated.run$lng
    elevation <- elevated.run$elevation
    
    # Create a color vector based on elevation using the viridis color scale.
    pal <- viridis(n = 7, option = "viridis")
    # Match colors to elevation values.
    colors <- pal[order(elevation)]  
    
    # Create the legend.
    elevation.pal <- colorBin(
      palette = pal,
      domain = range(dat$elevation),
      bins = 7,
      pretty = TRUE
    )
    
    # Create the Leaflet map
    leaflet() %>%
      setView(lng = mean(lng), lat = mean(lat), zoom = 13) %>%
      addTiles() %>% # Add base map tiles
      addPolylines(lng = lng, lat = lat, data = elevated.run, color = "limegreen") %>% # Plot run path
      addMarkers(lng = lng[1], lat = lat[1], icon = start.icon) %>% 
      addMarkers(lng = lng[length(lng)], lat = lat[length(lat)], icon = finish.icon) %>% 
      addLegend(position = "bottomright", pal = elevation.pal, values = range(elevation),
                title = "Elevation (m)", opacity = 1)
  })
}

shinyApp(ui, server, enableBookmarking = "url")