#' intro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_intro_ui <- function(id){
  ns <- NS(id)
  tagList(
    absolutePanel(
      actionButton(inputId = ns("trigger"), label = "About", icon = icon("circle-info")),
      top = "3%", right = "2%",
      # fixed = TRUE,
      width = "auto"
    )
  )
}

#' intro Server Functions
#'
#' @noRd
mod_intro_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Content of the intro modal
    intro_modal <- function() {
      modalDialog(
        withTags({
          div(id = "intro-modal-content",
              h3("What is a Research Transparency Statement?"),
              p("lorem ipsum...")
          )
        }),
        easyClose = TRUE,
        footer = NULL
      )
    }

    # Open intro modal
    observeEvent(input$trigger, {
      showModal(intro_modal())
    })
  })
}

## To be copied in the UI
# mod_intro_ui("intro")

## To be copied in the server
# mod_intro_server("intro")
