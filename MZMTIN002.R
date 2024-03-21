library(shiny)
library(shinythemes)
library(tidyverse)

ui <- dashboardPage(
  theme = shinytheme("flatly"),
  titlePanel("Runner"),
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
        h1("Home")
        #includeHTML("www/index.html")
      ),
      tabItem(
        tabName = "runs",
        fluidPage(
          h1("Runsss")
        )
        #includeHTML("www/runs.html")
      )
    )
  )
)

server <- function(input, output, session) {
  dat <- list.files("data/", "*.csv", full.names = T) %>% 
    read_csv(., id = "run") %>% 
    mutate(run = dense_rank(run))
}

shinyApp(ui, server)