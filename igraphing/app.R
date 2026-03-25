library(shiny)
library(shinyjs)
library(igraph)
library(cheddar)
library(png) 
library(ggplot2)
library(ggimage)
library(ggnetwork)

# nodes and links (pairs are links)
#g <- make_graph(c(
#  # The Keystone Bottleneck
#  "Lion", "Impala (Keystone)",
#  "Leopard", "Impala (Keystone)",
#  "Impala (Keystone)", "SuperPlant",
#  
#  # Peripheral dependencies
#  "Impala (Keystone)", "Plant2",
#  "Zebra (Specialist)", "SuperPlant",
#  "Wild Dog", "Zebra (Specialist)"))

#g <- make_graph(c(
#  
#  # Level 4 → Level 3
#  "Tyrannosaurus rex", "Allosaurus",
#  "Tyrannosaurus rex", "Velociraptor",
#  
#  # Level 3 → Level 2
#  "Allosaurus", "Stegosaurus",
#  "Allosaurus", "Edmontosaurus",
#  "Velociraptor", "Edmontosaurus",
#  "Velociraptor", "Triceratops",
#  
#  # Level 2 → Level 1
#  "Stegosaurus", "Cycads",
#  "Stegosaurus", "Ferns",
#  
#  "Edmontosaurus", "Cycads",
#  "Edmontosaurus", "Ginkgo",
#  
#  "Triceratops", "Ferns",
#  "Triceratops", "Ginkgo"
#))

g <- make_graph(c(
  
  # Apex level
  "Tyrannosaurus rex", "Allosaurus",
  "Tyrannosaurus rex", "Velociraptor",
  
  # Secondary → Primary
  "Velociraptor", "Triceratops",
  
  "Allosaurus", "Stegosaurus",
  "Allosaurus", "Triceratops",
  
  # Primary → Producers (shared but not perfectly symmetrical)
  "Stegosaurus", "Cycads",
  "Stegosaurus", "Ferns",
  
  "Triceratops", "Ferns"
))

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

# for style matching


## UI ----
ui <- navbarPage(
  
  title = div(
    tags$img(src = "logo.png", class = "navbar-logo"),
    "Secondary Extinction Dynamics"
  ),
  
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  tabPanel(
    "Game",
    
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput("vertexToDelete", "Species to delete", choices=NPS(gg)$node),
          actionButton("goButtonDelete", "Evaluate"),
          
          shinyjs::useShinyjs(),
          shinyjs::extendShinyjs(
            text = "shinyjs.refresh_page = function() { location.reload(); }",
            functions = "refresh_page"
          ),
          
          actionButton("refresh", "Refresh Food Web"),
          hr(),
          h4("☠️ Graveyard"),
          h5("Primary Extinctions"),
          uiOutput("primaryExtinctList"),
          h5("Secondary Extinctions"),
          uiOutput("secondaryExtinctList"),
          hr()
        ),
        
        mainPanel(
          p("This is a simple game where you choose to make a species extinct and reveal what the consequences are as these extinctions cascade through a food web."),
          
          p("The food web is shown below and has 6 species comprised of 2 plants, 2 herbivores and 3 predator. The numbers on the left define trophic levels."),
          
          br(),
          
          p(HTML("<strong>PRIMARY EXTINCTION:</strong> A species goes extinct usually due to an environmental factor or overharvesting.")),
          
          p(HTML("<strong>SECONDARY EXTINCTION:</strong> When a species goes extinct, it can affect other species, leading to cascading loss.")),
          
          br(),
          
          p("Try to identify which species cause no cascade vs at least one secondary extinction."),
          
          plotOutput("graphPlot", height = "70vh")
        )
      ),
      
      hr(),
      
      tags$footer(
        class = "footer-credits",
        p(HTML("<strong>Credits:</strong> Silhouette images are by Cy Marchant (<i>Wuerhosaurus ultimus</i>), <i>Ferran Sayol</i> 
           (Blechum), Jagged Fang Designs (<i> Allosaurus europaeus, Triceratops prorsus </i>), Pete Buchholz (<i>Edmontosaurus 
           regalis</i>), Richard Rich (<i>Tyrannosaurus rex</i>), Scott Hartman (<i>Velociraptor mongoliensis</i>), and others 
           (<i>Cycas revoluta</i>)."))
      )
    )
  )
)

## SERVER ----

server <- function(input, output, session) {
  
  # Load images ONCE (unchanged — no reactivity)
  img_list <- lapply(NPS(gg)$node, function(x) {
    img_path <- file.path("pics", paste0(x, ".png"))
    
    if (file.exists(img_path)) {
      img <- readPNG(img_path)
      
      if (is.matrix(img)) {
        new_img <- array(1, dim = c(nrow(img), ncol(img), 4))
        new_img[,,1:3] <- img
        return(new_img)
      } 
      
      if (dim(img)[3] == 2) {
        new_img <- array(1, dim = c(nrow(img), ncol(img), 4))
        new_img[,,1:3] <- img[,,1]
        new_img[,,4] <- img[,,2]
        return(new_img)
      }
      
      if (dim(img)[3] == 3) {
        new_img <- array(1, dim = c(nrow(img), ncol(img), 4))
        new_img[,,1:3] <- img
        return(new_img)
      }
      
      return(img)
    }
    return(NULL)
  })
  names(img_list) <- NPS(gg)$node
  
  # Reactive values
  my <- reactiveValues(
    gg = gg,
    extinct_primary = character(),
    extinct_secondary = character()
  )
  
  # =========================
  # DELETE + CASCADE LOGIC
  # =========================
  observeEvent(input$goButtonDelete, {
    
    req(input$vertexToDelete)
    
    valid_nodes <- NPS(my$gg)$node
    
    # safety check
    if(!(input$vertexToDelete %in% valid_nodes)) return()
    
    before_nodes <- valid_nodes
    deleted <- input$vertexToDelete
    
    # HANDLE LAST SPECIES (avoid cheddar crash)
    if(length(before_nodes) == 1) {
      
      # record extinction
      my$extinct_primary <- unique(c(my$extinct_primary, deleted))
      
      # no more secondary extinctions possible
      my$extinct_secondary <- my$extinct_secondary
      
      # mark ecosystem as empty
      my$gg <- NULL
      
      # clear dropdown
      updateSelectInput(session, "vertexToDelete", choices = character(0))
      
      return()
    }
    
    # NORMAL CASCADE
    # Get nodes BEFORE removal
    nodes_before <- NPS(my$gg)$node
    
    # Predict nodes AFTER removal using a safe clone
    gg_test <- my$gg
    
    gg_test <- tryCatch(
      RemoveNodes(gg_test, deleted, method = "cascade"),
      error = function(e) NULL
    )
    
    # If removal would empty the community → handle manually
    if(is.null(gg_test) || length(NPS(gg_test)$node) == 0) {
      
      my$extinct_primary <- unique(c(my$extinct_primary, deleted))
      my$extinct_secondary <- unique(c(my$extinct_secondary, setdiff(nodes_before, deleted)))
      
      my$gg <- NULL
      
      updateSelectInput(session, "vertexToDelete", choices = character(0))
      
      return()
    }
    
    # Otherwise safe to apply
    my$gg <- gg_test
    
    after_nodes <- NPS(my$gg)$node
    
    extinct <- setdiff(before_nodes, after_nodes)
    
    my$extinct_primary <- unique(c(my$extinct_primary, deleted))
    my$extinct_secondary <- unique(c(my$extinct_secondary, setdiff(extinct, deleted)))
    
    updateSelectInput(session, "vertexToDelete", choices = after_nodes)
  })
  
  observeEvent(input$refresh, {
    
    # reset ecosystem
    my$gg <- gg
    
    # clear graveyard
    my$extinct_primary <- character()
    my$extinct_secondary <- character()
    
    # reset dropdown
    updateSelectInput(
      session,
      "vertexToDelete",
      choices = NPS(my$gg)$node
    )
  })
  
  # =========================
  # GRAVEYARD UI OUTPUTS
  # =========================
  output$primaryExtinctList <- renderUI({
    if(length(my$extinct_primary) == 0) return(NULL)
    
    tags$ul(
      lapply(my$extinct_primary, function(x) {
        tags$li(style = "color:red; text-decoration: line-through;", x)
      })
    )
  })
  
  output$secondaryExtinctList <- renderUI({
    if(length(my$extinct_secondary) == 0) return(NULL)
    
    tags$ul(
      lapply(my$extinct_secondary, function(x) {
        tags$li(style = "color:gray; text-decoration: line-through;", x)
      })
    )
  })
  
  # =========================
  # PLOTTING ENGINE
  # =========================
  output$graphPlot <- renderPlot({
    
    if(is.null(my$gg)) {
      plot.new()
      title("Food Web Cascade")
      text(0.5, 0.5, "Entire ecosystem extinct", cex = 1.5)
      return()
    }
    
    current_igraph <- graph_from_adjacency_matrix(PredationMatrix(my$gg))
    
    V(current_igraph)$name <- NPS(my$gg)$node
    
    if(vcount(current_igraph) == 0) {
      plot.new()
      title("Food Web Cascade")
      text(0.5, 0.5, "All species extinct", cex = 1.5)
      return()
    }
    
    l <- layout_with_sugiyama(current_igraph)$layout
    
    if(is.null(l) || nrow(l) == 0) {
      l <- matrix(runif(vcount(current_igraph) * 2), ncol = 2)
    }
    
    tl <- tryCatch(TrophicLevels(my$gg), error = function(e) NULL)
    
    if(!is.null(tl)) {
      matched <- match(V(current_igraph)$name, names(tl))
      y_vals <- tl[matched]
      
      if(!all(is.na(y_vals))) {
        l[,2] <- y_vals
      }
    }
    
    l[,1] <- (l[,1] - min(l[,1])) / (max(l[,1]) - min(l[,1]) + 1e-6)
    l[,1] <- l[,1] * 1.4 - 0.2
    
    l[,2] <- (l[,2] - min(l[,2])) / (max(l[,2]) - min(l[,2]) + 1e-6)
    l[,2] <- 1 - l[,2]
    
    plot(l,
         type="n",
         xlab="", ylab="",
         axes=FALSE,
         xlim=c(0,1),
         ylim=c(-0.2,1.1),
         asp=1,
         main="Food Web Cascade")
    
    edges <- as_edgelist(current_igraph)
    
    if(!is.null(edges) && nrow(edges) > 0) {
      for(i in 1:nrow(edges)) {
        
        from_idx <- which(V(current_igraph)$name == edges[i,1])
        to_idx   <- which(V(current_igraph)$name == edges[i,2])
        
        if(length(from_idx) == 1 && length(to_idx) == 1) {
          arrows(
            l[from_idx,1], l[from_idx,2],
            l[to_idx,1], l[to_idx,2],
            col="gray70",
            length=0.05,
            lwd=1.5
          )
        }
      }
    }
    
    base_size <- 0.09
    node_names <- V(current_igraph)$name
    
    for(i in seq_along(node_names)) {
      
      s_name <- node_names[i]
      img <- img_list[[s_name]]
      
      x <- l[i,1]
      y <- l[i,2]
      
      if(!is.null(img)) {
        
        h <- nrow(img)
        w <- ncol(img)
        aspect <- h / w
        
        width  <- base_size
        height <- base_size * aspect
        
        rasterImage(
          img,
          x - width,
          y - height,
          x + width,
          y + height
        )
        
        text(x, y - height - 0.02, labels = s_name, font = 2)
        
      } else {
        points(x, y, pch=21, bg="white", cex=3)
        text(x, y, labels=s_name)
      }
    }
  })
}
shinyApp(ui = ui, server = server)

# Run the application ----
shinyApp(ui = ui, server = server)