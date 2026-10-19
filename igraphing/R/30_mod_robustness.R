
# Load the plot list once when the app starts
connectance_plots <- readRDS("data/network_plots.RDS")


connectanceUI <- function(id, n) {
  ns <- NS(id)
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          ns("connectance"),
          "Connectance",
          min = 1,
          max = n,
          value = 1,
          step = 1,
          ticks = FALSE,
          animate = FALSE
        )
        ,
        fluidRow(
          column(6, tags$small("Low")),
          column(
            6,
            tags$small("High", style = "float: right;")
          )
        )
      ),
      
      mainPanel(
        plotOutput(ns("plot1"), height = "350px"),
        plotOutput(ns("plot2"), height = "350px")
      )
    )
  )
}


connectanceServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$plot1 <- renderPlot({
      connectance_plots[[input$connectance]][1]
    })
    
    output$plot2 <- renderPlot({
      connectance_plots[[input$connectance]][2]
    })
    
  })
}
