# app.R
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
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
        includeHTML("www/index.html")
      ),
      tabItem(
        tabName = "runs",
        includeHTML("www/runs.html")
      )
    )
  )
)

server <- function(input, output, session) {
  # Server logic goes here
}

shinyApp(ui, server)