#' sections UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sections_ui <- function(id){
  ns <- NS(id)

  tagList(
    uiOutput(ns("input_form")),
    actionButton(ns("add_section"), "Add a new study")
  )
}

#' sections Server Functions
#'
#' @noRd
mod_sections_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Get questions
    sectionsList <- reactiveVal(ResearchTransparencyStatement:::questions$sectionsList)

    # Generate input form
    sectionsHTML <- reactive({
      sectionsHTML <- lapply(sectionsList(), renderSection, id = id)
      names(sectionsHTML) <- NULL
      do.call(tabsetPanel, c(sectionsHTML, id = NS(id, "sections")))
    })

    # Render input form
    # This solution is a bit slower but allows for dynamic UI update
    output$input_form <- renderUI({sectionsHTML()})

    # Add new study section
    observeEvent(input$add_section, {
      # Get current list
      current_list <- sectionsList()
      # Calculate new tab index
      new_element_index <- sum(grepl("Study", names(sectionsList()))) + 1
      # Get the number of sections
      new_element_name <- paste0("Study", new_element_index)
      # Change ids on new level
      new_element <- duplicate_section(sectionsList()$Study, name = new_element_name, suffix = new_element_index)
      # Add new level
      current_list[[new_element_name]] <- new_element
      # Add new study section
      sectionsList(current_list)
    })

    # changing icons when item is answered
    # change id to fit module
    observe({
      items <- getItemList(sectionsList(), all = FALSE) # loop only on items

      for(item in items){
        session$sendCustomMessage(
          type = "toggleChecker",
          message = list(
            id = ns(paste0(item, "Checker")),
            val = input[[item]],
            divId = ns(paste0("div", item, "Checker"))
          )
        )
      }

    })

    # stores the answers in a list
    answers <- reactive({
      reactiveValuesToList(input)
    })

    # return answers
    return(list(answers = answers, sectionList = sectionsList))
  })
}

## To be copied in the UI
# mod_sections_ui("sections")

## To be copied in the server
# mod_sections_server("sections")
