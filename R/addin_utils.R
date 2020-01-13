
# TODO Create util for generating expect_error, expect_warning etc.
# that extracts message and sets fixed = TRUE

# TODO convert package to a general package dev utilities package
# Can also include base_*select fns and the set_seed utils
# Perhaps it's stupid that users will get the dev addins when using the packages?

apply_capture <- function(string, fn, envir = NULL) {
  if (is.null(envir)){
    out <- capture.output(fn(eval_string(string)))
  } else {
    out <- capture.output(fn(eval_string(string, envir = envir)))
  }
  out
}

eval_string <- function(string, envir = NULL) {
  if (is.null(envir)){
    out <- eval(parse(text = string))
  } else {
    out <- eval(parse(text = string), envir = envir)
  }
  out
}

capture <- function(string) {
  apply_capture(string, identity)
}

prepare_output <- function(string){
  paste(paste(string, collapse = "\n"), "\n")
}

insert_code <- function(strings, prepare=TRUE) {

  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_list(types = "character",
    x = strings,
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)

  if(isTRUE(prepare)){
    code <- prepare_output(strings)
  } else {
    code <- strings
  }

  # TODO Set indentation based on current indentation

  rstudioapi::insertText(code)
}

get_selection <- function() {
  # Get context
  context <- rstudioapi::getActiveDocumentContext()
  # Get the selected variable name
  selection <- rstudioapi::primary_selection(context)[["text"]]
  if (selection == "") {
    warning("Selection was empty")
  }
  selection
}

get_side_effects <- function(string, envir) {

  # Get side effects
  # Note: Without rtilities2:: it complains
  # that it cannot find capture_side_effects
  catcher_string <- paste0("rtilities2::capture_side_effects(",
                           "function(){", string, "}",
                           ")")
  side_effects <- eval_string(catcher_string, envir = envir)

  side_effects

}

any_side_effects <- function(error, messages, warnings){
  !(is.null(error) || length(error) == 0) ||
  !(is.null(messages) || length(messages) == 0) ||
  !(is.null(warnings) || length(warnings) == 0)
}

#' Capture side effects
#' @export
#' @keywords internal
capture_side_effects <- function(fn){

  # Capture error
  error <- testthat::capture_error(suppressMessages(suppressWarnings(fn())))
  # If no error, capture messages and warnings
  if (is.null(error)){
    messages <- testthat::capture_messages(suppressWarnings(fn()))
    warnings <- testthat::capture_warnings(suppressMessages(fn()))
  } else {
    error <- error$message
    messages <- NULL
    warnings <- NULL
  }

  list("error" = error,
       "warnings" = warnings,
       "messages" = messages,
       "has_side_effects" = any_side_effects(error, warnings, messages))
}

create_space_string <- function(n = 2){
  paste0(rep(" ", n), collapse = "")
}

create_expect_equal <- function(x, y,
                                add_tolerance = FALSE,
                                spaces = 2) {

  spaces_string <- create_space_string(n = spaces)
  if (isTRUE(add_tolerance)) {
    tolerance_string <- paste0(",\n", spaces_string, "tolerance = 1e-4")
  } else {
    tolerance_string <- ""
  }

  # In case a string has \n, \t, etc.
  y <- escape_metacharacters(y)

  paste0("expect_equal(\n",
         spaces_string,
         x,
         ",\n",
         spaces_string,
         y,
         tolerance_string,
         ")")
}

create_expect_side_effect <- function(x, y,
                                      side_effect_type = "error",
                                      spaces = 2) {

  checkmate::assert_choice(x = side_effect_type,
                           choices = c("error", "warning", "message"))

  spaces_string <- create_space_string(n = spaces)

  expect_fn <- dplyr::case_when(
    side_effect_type == "error" ~ "expect_error",
    side_effect_type == "warning" ~ "expect_warning",
    side_effect_type == "message" ~ "expect_message",
    TRUE ~ "" # Won't get here anyway
  )

  y <- escape_metacharacters(y)
  y <- split_to_paste0(y, spaces = spaces)

  paste0(expect_fn, "(\n",
         spaces_string,
         x,
         ",\n",
         spaces_string,
         y,
         ",\n",
         spaces_string,
         "fixed = TRUE",
         ")")
}

escape_metacharacters <- function(string){
  # TODO must be a way to do it in one gsub with groups?
  string <- gsub("\n", "\\n", string, fixed = TRUE)
  string <- gsub("\r", "\\r", string, fixed = TRUE)
  string <- gsub("\t", "\\t", string, fixed = TRUE)
  string <- gsub("\v", "\\v", string, fixed = TRUE)
  string <- gsub("\f", "\\f", string, fixed = TRUE)
  string
}

split_string_every <- function(string, per = 60){
  # https://stackoverflow.com/a/26497700/11832955
  n <- seq(1, nc <- nchar(string), by = per)
  substring(string, n, c(n[-1]-1, nc))
}

split_to_paste0 <- function(string, per = 60, tolerance = 10, spaces = 2){

  if (nchar(string) > per + tolerance)
    splits <- split_string_every(paste0(string))
  else {
    return(paste0("\"", string, "\""))
  }

  spaces_string <- create_space_string(n = spaces + 7)
  paste0(
    "paste0(\"",
    paste0(splits, collapse = paste0("\",\n", spaces_string, "\"")), "\")"
  )
}

# split x at each index in pos
split_at <- function(x, pos){
  pos <- c(1L, pos, length(x) + 1L)
  Map(function(x, i, j) x[i:j], list(x),
      head(pos, -1L), tail(pos, -1L) - 1L)}

get_null_indices <- function(l){
  which(sapply(l, is.null))
}

# Get names in a list
# Remove empty and NULL elements from names list
get_element_names <- function(l, remove_empty_names = TRUE) {
  l_names <- names(l)

  # Remove empty names ""
  if (length(l_names) > 0 && isTRUE(remove_empty_names)){
    empty_indices <- which(sapply(l_names, function(x){
      x == "" || is.null(x)}))
    if (length(empty_indices) > 0){
      l_names <- l_names[-empty_indices]
    }
  }

  l_names
}

# For testing a list of string expectations
# TODO This could be an exported function
# TODO The error messages should be better
eval_expectations <- function(l, envir){
  plyr::l_ply(l,.fun = function(x){eval(parse(text=x), envir = envir)})
}
