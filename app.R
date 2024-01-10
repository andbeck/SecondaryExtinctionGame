#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(datamods)

# Define UI for application that draws a histogram
ui <- fluidPage(
  selectInput('selectfile','Select File', choice = list_pkg_data("cheddar")),
  textOutput('fileselected')

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$fileselected<-renderText({
    paste0('You have selected: ', input$selectfile)
  })
  
  output$fileselected <- renderPrint({
    dataset <- reactive(data(input$selectfile))
    NPS(dataset)
  })
  
  # output$summary <- renderPrint({
  #   dataset <- data(input$dataset, "cheddar")
  #   summary(dataset)
  # })
  # 
  # output$table <- renderTable({
  #   dataset <- get(input$dataset, "package:datasets")
  #   dataset
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)
