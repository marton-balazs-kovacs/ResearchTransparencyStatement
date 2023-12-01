#' Helper functions to generate report
#'
#' @description
#' These functions help to generate the output in a nicely formatted matter.
NULL

#' @rdname compose_report
compose_report <- function(answers = NULL, sectionsList = NULL, answerList = NULL){
  # Fill in answers with "not answered"
  bundleQuestions <- getItemList(sectionsList)
  not.answered <- !bundleQuestions %in% names(answers)
  answers[bundleQuestions[not.answered]] <- "Not answered"

  # We create sections
  sections <- sapply(sectionsList, compose_sections, answers = answers)

  shiny::HTML(sections)
}

#' @rdname compose_report
compose_sections <- function(section, answers = NULL){
  stringr::str_glue(
    "
    <div>
      <h2>{section_name}</h2>
      <p><b>{section_label}</b></p>

      {questions}
    </div>
    ",
    section_name = ifelse(is.null(section$Name) || section$Name == "", "", section$Name),
    section_label = ifelse(is.null(section$Label) || section$Label == "", "", section$Label),
    questions = paste(sapply(section$Questions, compose_questions, answers = answers), collapse = " \n")
    )
}

#' @rdname render_document
compose_questions <- function(question, answers = answers){
  # Check whether the section is supposed to be shown
  # TODO: fix this
  show <- TRUE
  # if(!is.null(question$Depends)){
  #   show <- gsub(".ind_", "answers$ind_", question$Depends)
  #   show <- eval(parse(text = show))
  # }

  # If the question is not shown, return empty space
  if(!show){
    return("")
  }

  # Format answers to questions
  if( !(question$AnswerType == "Explain") ){
    # Make answer bold if it is not a comment
    answer <- stringr::str_glue("<b>{answers[[question$Name]]}</b>")
  } else if( question$AnswerType == "Explain" ){
    # Add no comment otherwise return formatted comment
    answer <- ifelse(answers[[question$Name]] == "", "No comments.", answers[[question$Name]])
    answer <- stringr::str_glue("<em>{answer}</em>")
  } else{
    # Just to catch non-valid question.json data
    answer <- ""
  }

  # Format question labels
  if( !(question$AnswerType == "Explain") ){
    label <- question$Label
  } else if( question$AnswerType == "Explain" ){
    if(question$Label == ""){
      label <- ""
    } else{
      label <- paste0("<b>", question$Label, "</b>")
    }
  } else{
    label <- ""
  }

  stringr::str_glue(
    "
    <p>{question_label} {answer}</p>
    <br>
    ",
    question_label = label,
    answer = answer
  )
}
