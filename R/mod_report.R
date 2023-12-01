#' report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_report_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("responses")),
    textOutput(ns("test"))
  )
}

#' report Server Functions
#'
#' @noRd
mod_report_server <- function(id, answers){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # checks which sections are complete
    whichComplete <- reactive({
      isComplete(
        answers = answers(),
        sectionsList = ResearchTransparencyStatement:::questions$sectionsList
      )
    })

    # checks whether the report is complete
    isFinalized <- reactive({
      all(whichComplete())
    })

    #### Reactive animations ----
    observe({
      if(isFinalized()) {
        # TODO: replace with app specific logic
        # shinyjs::enable("report")
      } else {
        # shinyjs::disable("report")
      }
    })

    output$responses <- renderUI({
      # paste(names(answers()), answers(), sep = ": ", collapse = "\n")
      compose_report(answers = answers(), sectionsList = ResearchTransparencyStatement:::questions$sectionsList, answerList = ResearchTransparencyStatement:::questions$answerList)
      })

    output$test <- renderText({
      # paste(names(answers()), answers(), sep = ": ", collapse = "\n")
      compose_report(answers = answers(), sectionsList = ResearchTransparencyStatement:::questions$sectionsList, answerList = ResearchTransparencyStatement:::questions$answerList)
    })

  })
}

## To be copied in the UI
# mod_report_ui("report")

## To be copied in the server
# mod_report_server("report")
