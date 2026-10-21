# app.R — application shell
# Shiny automatically sources global.R and files in R/ when run from this folder.


# app.R — application shell

ui <- navbarPage(
  title = div(
    tags$img(src = "logo.png", class = "navbar-logo"),
    "Extinction Games"
  ),
  
  header = tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    )
  ),
  
  tabPanel(
    "Extinction Dynamics",
    extinctionUI("extinctions")
  ),
  
  tabPanel(
    "Robustness",
    connectanceUI(
      "connectance",
      n = length(connectance_plots)
    )
  ),
  
  tabPanel(
    "Extinction Scenarios",
    scenarioMatchingUI("scenario_matching")
  )
)

server <- function(input, output, session) {
  extinctionServer(
    "extinctions",
    initial_community = initial_community
  )
  
  connectanceServer("connectance")
  
  scenarioMatchingServer("scenario_matching")
}

shinyApp(ui = ui, server = server)
