# Tab 1: primary and secondary extinction dynamics.

extinctionUI <- function(id) {
  ns <- NS(id)

  fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput(
          ns("species_to_delete"),
          "Species to delete",
          choices = NULL
        ),
        actionButton(ns("evaluate"), "Evaluate"),
        actionButton(ns("reset"), "Refresh Food Web"),
        hr(),
        h4("☠️ Graveyard"),
        h5("Primary Extinctions"),
        uiOutput(ns("primary_extinct_list")),
        h5("Secondary Extinctions"),
        uiOutput(ns("secondary_extinct_list"))
      ),
      mainPanel(
        p(paste(
          "Choose a species to remove and observe how its extinction",
          "can trigger further losses through the food web."
        )),
        p(HTML(paste0(
          "<strong>PRIMARY EXTINCTION:</strong> A species is removed, ",
          "for example through environmental change or overharvesting."
        ))),
        p(HTML(paste0(
          "<strong>SECONDARY EXTINCTION:</strong> Additional species ",
          "disappear as a consequence of the initial removal."
        ))),
        plotOutput(ns("foodweb_plot"), height = "70vh")
      )
    ),
    hr(),
    tags$footer(
      class = "footer-credits",
      p(HTML(paste0(
        "<strong>Credits:</strong> Add the original species-image ",
        "attributions here before publishing."
      )))
    )
  )
}

extinctionServer <- function(id, initial_community) {
  moduleServer(id, function(input, output, session) {

    initial_species <- get_species(initial_community)
    img_list <- load_species_images(initial_species)

    state <- reactiveValues(
      community = initial_community,
      primary = character(),
      secondary = character()
    )

    observe({
      updateSelectInput(
        session,
        "species_to_delete",
        choices = get_species(state$community),
        selected = if (length(get_species(state$community))) {
          get_species(state$community)[1]
        } else {
          character(0)
        }
      )
    })

    observeEvent(input$evaluate, {
      req(input$species_to_delete)

      result <- tryCatch(
        remove_species_cascade(
          state$community,
          input$species_to_delete
        ),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )

      if (is.null(result)) return()

      state$community <- result$community
      state$primary <- unique(c(state$primary, result$primary))
      state$secondary <- unique(c(state$secondary, result$secondary))
    }, ignoreInit = TRUE)

    observeEvent(input$reset, {
      state$community <- initial_community
      state$primary <- character()
      state$secondary <- character()
    }, ignoreInit = TRUE)

    output$primary_extinct_list <- renderUI({
      if (length(state$primary) == 0L) return(NULL)
      tags$ul(lapply(state$primary, function(sp) {
        tags$li(
          style = "color: red; text-decoration: line-through;",
          sp
        )
      }))
    })

    output$secondary_extinct_list <- renderUI({
      if (length(state$secondary) == 0L) return(NULL)
      tags$ul(lapply(state$secondary, function(sp) {
        tags$li(
          style = "color: gray; text-decoration: line-through;",
          sp
        )
      }))
    })

    output$foodweb_plot <- renderPlot({
      plot_foodweb(state$community, img_list)
    })
  })
}
