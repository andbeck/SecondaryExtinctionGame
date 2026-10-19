# app.R — application shell
# Shiny automatically sources global.R and files in R/ when run from this folder.

ui <- navbarPage(
  title = div(
    tags$img(src = "logo.png", class = "navbar-logo"),
    "Secondary Extinction Dynamics"
  ),
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  tabPanel(
    "Extinction Dynamics",
    extinctionUI("extinctions")
  )
  # Future tabs can be added here, for example:
  # , tabPanel("Scenario Matching", scenarioUI("scenarios"))
  # , tabPanel("Robustness & Structure", robustnessUI("robustness"))
)

server <- function(input, output, session) {
  extinctionServer("extinctions", initial_community = initial_community)
}

shinyApp(ui = ui, server = server)
