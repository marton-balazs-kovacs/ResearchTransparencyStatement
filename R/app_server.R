#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Generate sections
  section_data <- mod_sections_server("sections")

  # Intro
  mod_intro_server("intro")

  # Generate report
  mod_report_server(
    "report",
    answers = reactive({section_data$answers()}),
    sections = reactive({section_data$sectionList()})
  )
}
