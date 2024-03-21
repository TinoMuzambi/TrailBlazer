# app.R
library(shiny)
library(semantic.dashboard)
library(tidyverse)

ui <- dashboardPage(
  dashboardHeader(title = "Runner"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Runs", tabName = "runs")
    )
  ),
  dashboardBody(
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