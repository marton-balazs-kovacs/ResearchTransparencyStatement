#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %|NA|% 1
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- function(...) shiny::reactiveValues(...)
rvtl <- function(...) shiny::reactiveValuesToList(...)

#' Add namespace to conditional elements in json
dep_ns <- function(conditional, id) {
  # Assign id to dependency id(s)
  conditional$DependsOn <- purrr::map(conditional$DependsOn, function(dependency) {
    dependency$Name <- shiny::NS(id, dependency$Name)

    dependency
  })

  return(conditional)
}

#' Generate condition for conditional elements
dep_create <- function(conditional) {
  # Extract elements from the conditional
  condition <- conditional$Condition
  depends_on <- conditional$DependsOn

  # Replace placeholders in the condition string
  for (i in seq_along(depends_on)) {
    placeholder <- paste0("\\", i)
    replace_value <- depends_on[[i]]$Name
    condition <- gsub(placeholder, replace_value, condition, fixed = TRUE)
  }

  return(condition)
}
