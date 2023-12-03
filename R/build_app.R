#' Functions to create the HTML structure of the shiny app dynamically
#'
#'These functions, first, read the question sections content and structure
#'from the appropriate internal datafile. Second, they create the HTML DOM tree
#'dynamically when they called. To save computing power we use them only once on app start.
#'
#' @section Warning: The dynamically created HTML element ids should be namespaced
#'   in order to reach their inputs from the server side of the modules.
#'
renderSection <- function(section,
                          id = NULL) {
  # Each section is on a different panel
  tabPanel(
    # section$Name is the navigation button text
    title = section$Name,
    value = section$Value,
    div(
      class = "section-container",
      # Show section$Label on the top of the tab
      if (!is.null(section$Label)) {
        strong(section$Label)
      },
      # Render all inputfields within this section
      lapply(
        section$Questions,
        customField,
        id = id
      )
    )
  )
}

customField <- function(ind, id = NULL) {
  # If the input is not a question, it is assumed that it is some guidance text in between the items
  if (ind$Type == "text") {
    # The guidance text itself can be conditional
    if (is.null(ind$Conditional)) {
      strong(ind$Label)
    } else{
      # Add module id
      ind$Conditional <- dep_ns(ind$Conditional, id = id)
      # Render guidance text conditionally
      conditionalPanel(condition = dep_create(ind$Conditional), strong(ind$Label))
    }
    # Create question
  } else {
    customButton(ind, id = id)
  }
}

customButton <- function(ind, id = NULL) {
  # Always display unconditional items
  if (is.null(ind$Conditional)) {
    condition <- "true"
  } else {
    ind$Conditional <- dep_ns(ind$Conditional, id = id)
    condition <- dep_create(ind$Conditional)
  }

  # Render question conditionally
  conditionalPanel(
    condition = condition,
    div(
      class = "question-container",
      # Question label
      ind$Label,
      # Render answer buttons
      switchButtons(ind, id = id),
      # Icon for opening textbox for additional explanation
      # We decided to provide the option to add explanation by a toggle button
      # We currently add explanation as a separate response dependent on the button which is not in questions.json
      # This is a makeshift solution should be replaced by nested dependency structure (by rows and button element e.g.) if app gets picked up
      # in that case json-schema should be provided for the app so it is more generalizable for other tasks in the future
      # Also sections should be optional as well
      # if (ind$Mandatory) {
      #   actionButton(
      #     inputId = paste0(id, "-", ind$Name, "_button"),
      #     label = "",
      #     icon = icon("far fa-pen-to-square", lib = "font-awesome", class = "dependency-icon")
      #   )
      # },
      # Icon to show whether the question is answered
      # Only for mandatory questions

      if (ind$Mandatory) {
        # Adds exclamation circle next to the item
        tags$div(
          class = "toggle-icon",
          id = shiny::NS(id, paste0("div", ind$Name, "Checker")),
          title = "This question needs to be answered.",
          tags$i(id = shiny::NS(id,  paste0(
            ind$Name, "Checker"
          )),
          class = 'fa fa-exclamation-circle')
        )
      }
    )
  )
}


switchButtons <- function(ind, id = NULL) {
  # Add module id
  ind$Name <- shiny::NS(id, ind$Name)

  answerList <- ResearchTransparencyStatement:::questions$answerList
  # If the AnswerType is specified in the answerList object (from .json), the button options should be rendered from
  # those options otherwise, the AnswerType is passed directly to the options
  if (ind$AnswerType %in% names(answerList)) {
    answerOptions <- answerList[[ind$AnswerType]]
  } else{
    answerOptions <- ind$AnswerType
  }

  # Switch between different input types
  switch (
    ind$Type,
    "select"    = shinyWidgets::pickerInput (inputId = ind$Name, choices = answerOptions),
    "radio"     = shiny::radioButtons (
      inputId = ind$Name,
      choices = answerOptions,
      label = NULL,
      inline = TRUE,
      selected = character(0)
    ),
    "textInput" = textInput (inputId = ind$Name, label = ind$Label),
    "textArea"  = shiny::textAreaInput (
      inputId = ind$Name,
      label = NULL,
      placeholder = answerOptions,
      rows = 2
    )
  )
}

getItemList <- function(sectionsList,
                        all = TRUE,
                        id = NULL) {
  # Get list of question ids
  items <- unlist(sapply(sectionsList,
                         function(section) {
                           sapply(section$Questions, function(item)
                             item$Name)
                         }))

  # Add module id
  items <- shiny::NS(id, items)

  if (all) {
    return(items)
  } else {
    return(items[grep("ind", items)])
  }
}
