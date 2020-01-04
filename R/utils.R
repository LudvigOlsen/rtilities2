


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

insert_code <- function(string, prepare=TRUE) {
  if(isTRUE(prepare)){
    code <- prepare_output(string)
  } else {
    code <- string
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

  paste0("expect_equal(\n",
         spaces_string,
         x,
         ",\n",
         spaces_string,
         y,
         tolerance_string,
         ")")
}

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
