# Questions
questions <- jsonlite::read_json(path = "inst/app/www/questions.json")

# Get UI elements for the inputs
# Use the assign_id() func if there are unnamed functions
questions <- rlang::list2(
  sectionsList = questions$Sections,
  answerList = questions$Answers
)

# Save list as internal data
usethis::use_data(questions, overwrite = TRUE, internal = TRUE)
