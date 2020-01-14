
## Utils for addins ##

# Evaluate string -> apply function -> capture output
apply_capture <- function(string, fn, envir = NULL) {
  if (is.null(envir)) {
    out <- capture.output(fn(eval_string(string)))
  } else {
    out <- capture.output(fn(eval_string(string, envir = envir)))
  }
  out
}

# parse, eval
eval_string <- function(string, envir = NULL) {
  if (is.null(envir)) {
    out <- eval(parse(text = string))
  } else {
    out <- eval(parse(text = string), envir = envir)
  }
  out
}

# Evaluate string and capture output
capture <- function(string) {
  apply_capture(string, identity)
}

# Prepare output for insertion
prepare_output <- function(string, indentation = 0, trim_right = FALSE) {

  spaces_string <- create_space_string(n = indentation)

  string <- paste(string, collapse = paste0("\n", spaces_string))
  if (isTRUE(trim_right))
    string <- trimws(string, which="right")
  string
}

# Insert code at selection with rstudioapi
insert_code <- function(strings, prepare = TRUE, indentation = 0) {

  stop_if(!(is.list(strings) || is.character(strings)),
          "strings should be either a list or a character vector.")

  if (isTRUE(prepare)) {
    code <- prepare_output(strings, indentation = indentation)
  } else {
    code <- strings
  }

  # TODO Set indentation based on current indentation

  rstudioapi::insertText(code)
}

# Get user's selection. Warn if empty.
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

# Get user's selection. Warn if empty.
get_indentation <- function() {
  # Get context
  context <- rstudioapi::getActiveDocumentContext()
  # Get the column of the beginning of the selection
  context$selection[[1]]$range$start[[2]] - 1 # starts at 1
}



# split x at each index in pos
# Found on stackoverflow (TODO check)
split_at <- function(x, pos) {
  pos <- c(1L, pos, length(x) + 1L)
  Map(
    function(x, i, j) x[i:j], list(x),
    head(pos, -1L), tail(pos, -1L) - 1L
  )
}

# Get indices of list elements that are NULL
get_null_indices <- function(l) {
  which(sapply(l, is.null))
}

# Get names in a list
# Remove empty and NULL elements from names list
get_element_names <- function(l, remove_empty_names = TRUE) {
  l_names <- names(l)

  # Remove empty names ""
  if (length(l_names) > 0 && isTRUE(remove_empty_names)) {
    empty_indices <- which(sapply(l_names, function(x) {
      x == "" || is.null(x)
    }))
    if (length(empty_indices) > 0) {
      l_names <- l_names[-empty_indices]
    }
  }

  l_names
}

# For testing a list of string expectations
# TODO This could be an exported function
# TODO The error messages should be better
eval_expectations <- function(l, envir) {
  plyr::l_ply(l, .fun = function(x) {
    eval(parse(text = x), envir = envir)
  })
}
