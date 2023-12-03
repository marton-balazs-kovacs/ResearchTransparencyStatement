#' Duplicate section
#'
#' @description The function duplicates a section and assigns unique ids to dynamically to the dynamically created elements.
#'
#' @param section section element according formatted according to the json schema in Questions.json
#' @param name name of the new section
#' @param prefix character to add to the beginning of existing ids to make them unique on dynamic rendering
#' @param suffix character to add to the end of existing ids to make them unique on dynamic rendering
duplicate_section <- function(section,
                              name,
                              prefix = NULL,
                              suffix = NULL) {
  # Change name
  if (is.null(section$Name)) {
    stop("The section$Name property is missing!")
  }
  section$Name <- name
  # Iterate over questions in the section
  section$Questions <-
    purrr::map(section$Questions, function(question) {
      # Check if Question$Name exists
      if (is.null(question$Name)) {
        stop("The Question$Name property is missing!")
      }
      # Attach prefix and/or suffix to the Name
      question$Name <- paste0(prefix, question$Name, suffix)
      # Change dependency if there is any
      if (!is.null(question$Conditional)) {
        # Change the names of the dependency questions
        question$Conditional$DependsOn <-
          purrr::map(question$Conditional$DependsOn, function(dependency) {
            dependency$Name <- paste0(prefix, dependency$Name, suffix)

            dependency
          })
      }
      # Return the modified question
      return(question)
    })

  return(section)
}
