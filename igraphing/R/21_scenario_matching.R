
# R/scenario_matching.R

# --------------------------------------------------
# Helper: plot a network using shared node positions
# --------------------------------------------------

plot_extinction_network <- function(
    edges,
    plot_title,
    shared_layout
) {
  
  edges <- unique(
    as.data.frame(
      edges[, c("consumer", "resource"), drop = FALSE]
    )
  )
  
  if (nrow(edges) == 0L) {
    plot.new()
    title(main = plot_title)
    text(0.5, 0.5, "No network data available")
    return(invisible(NULL))
  }
  
  present_nodes <- unique(c(
    as.character(edges$consumer),
    as.character(edges$resource)
  ))
  
  graph <- igraph::graph_from_data_frame(
    edges,
    directed = TRUE,
    vertices = data.frame(name = present_nodes)
  )
  
  network_layout <- shared_layout[
    igraph::V(graph)$name,
    ,
    drop = FALSE
  ]
  
  plot(
    graph,
    layout = network_layout,
    main = plot_title,
    vertex.size = 7,
    vertex.label = NA,
    edge.arrow.size = 0.25,
    edge.color = "grey65",
    vertex.color = "#005C53",
    margin = 0.05
  )
}


# --------------------------------------------------
# UI
# --------------------------------------------------

scenarioMatchingUI <- function(id) {
  
  ns <- NS(id)
  
  fluidPage(
    
    titlePanel("Best-fit Extinction Mechanism"),
    
    sidebarLayout(
      
      # LEFT SIDEBAR
      sidebarPanel(
        width = 4,
        
        selectInput(
          ns("mechanism"),
          "Extinction mechanism",
          choices = NULL
        ),
        
        # Information box
        wellPanel(
          h4("About this comparison"),
          
          p(
            "Compare the simulated final community against ",
            "the known post-extinction community."
          ),
          
          p(
            "The pre-extinction network is provided as a ",
            "reference for the original community structure."
          ),
          
          tags$hr(),
          
          h5("Node TSS interpretation"),
          
          p(
            strong("+1: "),
            "Perfect agreement."
          ),
          
          p(
            strong("0: "),
            "No discriminatory skill."
          ),
          
          p(
            strong("-1: "),
            "Perfectly incorrect predictions."
          ),
          
          p(
            class = "text-muted",
            "Higher scores indicate better agreement ",
            "between predicted and observed outcomes."
          )
        ),
        
        # Pre-extinction network in sidebar
        h4("Known pre-extinction network"),
        
        plotOutput(
          ns("pre_network"),
          height = "450px"
        )
      ),
      
      # MAIN PANEL
      mainPanel(
        width = 8,
        
        # Two networks in one row
        fluidRow(
          
          column(
            width = 6,
            
            h4("Simulated network"),
            
            plotOutput(
              ns("simulated_network"),
              height = "420px"
            )
          ),
          
          column(
            width = 6,
            
            h4("Known post-extinction network"),
            
            plotOutput(
              ns("post_network"),
              height = "420px"
            )
          )
        ),
        
        tags$hr(),
        
        # Selected mechanism's Node TSS
        h3("Model fit"),
        
        uiOutput(ns("selected_score")),
        
        tags$hr(),
        
        # Collapsible comparison table
        checkboxInput(
          ns("show_table"),
          "Show comparison of all extinction mechanisms",
          value = FALSE
        ),
        
        conditionalPanel(
          condition = "input.show_table == true",
          ns = ns,
          
          h4("Node TSS ranking"),
          
          p(
            "Mechanisms are ordered from highest to lowest ",
            "Node TSS. The arrow identifies the selected mechanism."
          ),
          
          tableOutput(ns("tss_table"))
        )
      )
    )
  )
}


# --------------------------------------------------
# Server
# --------------------------------------------------

scenarioMatchingServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    # ----------------------------------------------
    # Validate inputs
    # ----------------------------------------------
    
    stopifnot(
      all(c(
        "extinction_mechanism",
        "tss_node"
      ) %in% names(tss)),
      
      all(c(
        "consumer",
        "resource"
      ) %in% names(pre_edge)),
      
      all(c(
        "consumer",
        "resource"
      ) %in% names(post_edge)),
      
      all(c(
        "extinction_mechanism",
        "consumer",
        "resource"
      ) %in% names(extinction_edge))
    )
    
    mechanisms <- sort(
      unique(as.character(tss$extinction_mechanism))
    )
    
    # Update selector with available mechanisms
    updateSelectInput(
      session,
      "mechanism",
      choices = mechanisms,
      selected = mechanisms[1]
    )
    
    
    # ----------------------------------------------
    # Shared layout for all networks
    # ----------------------------------------------
    
    all_edges <- unique(rbind(
      pre_edge[, c("consumer", "resource"), drop = FALSE],
      post_edge[, c("consumer", "resource"), drop = FALSE],
      extinction_edge[, c("consumer", "resource"), drop = FALSE]
    ))
    
    all_graph <- igraph::graph_from_data_frame(
      all_edges,
      directed = TRUE
    )
    
    set.seed(42)
    
    shared_layout <- igraph::layout_with_fr(all_graph)
    
    rownames(shared_layout) <- igraph::V(all_graph)$name
    
    
    # ----------------------------------------------
    # Known pre-extinction network
    # ----------------------------------------------
    
    output$pre_network <- renderPlot({
      
      plot_extinction_network(
        edges = pre_edge,
        plot_title = "Known pre-extinction",
        shared_layout = shared_layout
      )
      
    })
    
    
    # ----------------------------------------------
    # Known post-extinction network
    # ----------------------------------------------
    
    output$post_network <- renderPlot({
      
      plot_extinction_network(
        edges = post_edge,
        plot_title = "Known post-extinction",
        shared_layout = shared_layout
      )
      
    })
    
    
    # ----------------------------------------------
    # Simulated network for selected mechanism
    # ----------------------------------------------
    
    output$simulated_network <- renderPlot({
      
      req(input$mechanism)
      
      simulated_edges <- extinction_edge[
        extinction_edge$extinction_mechanism ==
          input$mechanism,
        c("consumer", "resource"),
        drop = FALSE
      ]
      
      plot_extinction_network(
        edges = simulated_edges,
        plot_title = paste(
          "Simulated:",
          gsub("_", " ", input$mechanism)
        ),
        shared_layout = shared_layout
      )
      
    })
    
    
    # ----------------------------------------------
    # Selected mechanism's Node TSS
    # ----------------------------------------------
    
    output$selected_score <- renderUI({
      
      req(input$mechanism)
      
      score <- tss[
        tss$extinction_mechanism == input$mechanism,
        "tss_node",
        drop = TRUE
      ]
      
      req(length(score) > 0L)
      
      score <- score[1]
      
      score_colour <- if (is.na(score)) {
        "#777777"
      } else if (score >= 0.4) {
        "#62771B"
      } else if (score >= 0) {
        "#042940"
      } else {
        "#DB8655"
      }
      
      fluidRow(
        
        column(
          width = 7,
          
          wellPanel(
            h4(
              gsub("_", " ", input$mechanism)
            ),
            
            p("Node True Skill Statistic"),
            
            h1(
              if (is.na(score)) {
                "NA"
              } else {
                sprintf("%.3f", score)
              },
              style = paste0(
                "color: ",
                score_colour,
                "; font-weight: bold;"
              )
            ),
            
            p(
              class = "text-muted",
              if (is.na(score)) {
                "Node TSS is unavailable for this mechanism."
              } else if (score == 1) {
                "Perfect agreement."
              } else if (score == -1) {
                "Perfectly incorrect predictions."
              } else if (score > 0) {
                "Positive agreement with the observed outcome."
              } else if (score == 0) {
                "No discriminatory skill."
              } else {
                "Negative agreement with the observed outcome."
              }
            )
          )
        )
      )
      
    })
    
    
    # ----------------------------------------------
    # Optional comparison table
    # ----------------------------------------------
    
    output$tss_table <- renderTable({
      
      result <- tss[
        ,
        c(
          "extinction_mechanism",
          "tss_node"
        ),
        drop = FALSE
      ]
      
      result$Selected <- ifelse(
        result$extinction_mechanism == input$mechanism,
        "\u25B6",
        ""
      )
      
      result <- result[
        order(-result$tss_node, na.last = TRUE),
        c(
          "Selected",
          "extinction_mechanism",
          "tss_node"
        ),
        drop = FALSE
      ]
      
      names(result) <- c(
        "",
        "Extinction mechanism",
        "Node TSS"
      )
      
      result$`Node TSS` <- round(
        result$`Node TSS`,
        3
      )
      
      result
      
    }, striped = TRUE, hover = TRUE)
    
  })
}
