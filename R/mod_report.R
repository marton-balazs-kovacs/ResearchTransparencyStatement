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
    uiOutput(ns("responses"))
    # textOutput(ns("test"))
  )
}

#' report Server Functions
#'
#' @noRd
mod_report_server <- function(id, answers, sections){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # checks which sections are complete
    whichComplete <- reactive({
      isComplete(
        answers = answers(),
        sectionsList = sections()
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
      compose_report(answers = answers(), sectionsList = sections(), answerList = ResearchTransparencyStatement:::questions$answerList)
      })

    # output$test <- renderText({
    #   compose_report(answers = answers(), sectionsList = sections(), answerList = ResearchTransparencyStatement:::questions$answerList)
    # })

  })
}

## To be copied in the UI
# mod_report_ui("report")

## To be copied in the server
# mod_report_server("report")
