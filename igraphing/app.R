library(shiny)
library(shinyjs)
library(igraph)
library(cheddar)

# simple random example ----
#g <- make_ring(10) %>% set_vertex_attr("name", value = LETTERS[1:10])
#g <- graph_from_adjacency_matrix(PredationMatrix(TL84))

# n <- 6
# g <- erdos.renyi.game(n, p.or.m = 0.4, directed = TRUE) %>% set_vertex_attr("name", value = LETTERS[1:n])
# plot(g)
# plot(delete_vertices(g, 6))

# Cheddar Code ----

# nodes and links (pairs are links)
g <- graph(c("Pred 1","Herb 2",
             "Pred 1","Herb 3",
             "Herb 2",'Plant 4',
             "Herb 2","Plant 5",
             "Herb 3","Plant 6"))

# cheddar community pieces
prop <- list(title = "foodweb")
nn <- data.frame(node = names(V(g)))
TL <- as_data_frame(g, what = "edges")
colnames(TL) <- c("consumer","resource")

# build community
gg <- Community(properties = prop, 
                nodes = nn, 
                trophic.links = TL)

## view of inital community by trophic level - test plot ----

# PlotWebByLevel(gg, show.nodes.as = "labels", colour.by = 'resolved.to', node.labels = NPS(gg)$node)

#plot(igraph::graph_from_adjacency_matrix(PredationMatrix(gg)))

# example removal
# gg2 <- RemoveNodes(gg, remove = 6, method = "cascade")
# labs2 <- gg2$nodes$node
# plot(gg2, show.nodes.as = "labels", node.labels = labs2)

# Shiny Units ----

## UI ----
ui <- fluidPage( 
  titlePanel("Secondary Extinction Dynamics - A Game"),
  sidebarLayout(
    sidebarPanel(
      selectInput("vertexToDelete", "Species to delete", choices=NPS(gg)$node),
      actionButton("goButtonDelete", "Evaluate"),
      
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
      actionButton("refresh", "Refresh Food Web")
    ),
    
    # Show a plot of the graph
    mainPanel(
      p("This is a simple game where you choose to make a species extinct and reveal what the consequences 
        are as these extinctions 'cascade' through
        a food web."),
      p("The food web is shown below and has 6 species comprised of 3 plants, 2 herbivores and 1 predator.  
        The numbers on the left defined 'trophic levels'"),
      br(),
      p("SECONDARY EXTINCTIONS: When a species goes extinct, sometimes it can affect other species, leading
        to a secondary extinction. The most often happens when a resource goes extinct leaving
        a species with nothing else to eat."),
      br(),
      p("Let's see if you you identify for which species there will be no secondary extinctions and for which species there
        will be at least one secondary extinction"),
      p("Choose the species you want to make extinct (primary extinction) and then press evaluate to see the consequences.  
        You can reset the foodweb (Refresh Food Web) to try another example."),
      p("Are there any primary extinctions that might lead to the loss of more than one additional species"),
      p("Is there a sequence of primary extinctions that can lead to 'community collapse'?"),
      plotOutput("graphPlot"))
  )
)

## SERVER ----

server <- function(input, output, session) {
  my <- reactiveValues(gg=gg,
                       labs = NPS(gg)$node)
  
  observeEvent(input$goButtonDelete,{
    my$gg <- RemoveNodes(my$gg, input$vertexToDelete, method = "cascade")
    my$labs <- NPS(my$gg)$node
    
    updateSelectInput(session, "vertexToDelete", choices=NPS(gg)$node)
    
    observeEvent(input$refresh, {
      shinyjs::js$refresh_page()
    })
    
  })
  
  output$graphPlot <- renderPlot(PlotWebByLevel(my$gg, 
                                      show.nodes.as = "labels", 
                                      node.labels = my$labs,
                                      cex = 1.2, main = ""))
}

shinyApp(ui = ui, server = server)

# Run the application ----
shinyApp(ui = ui, server = server)