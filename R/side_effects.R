

#   __________________ #< 0d3e1cc7677b500a32d83bc726793163 ># __________________
#   Process side effects                                                    ####


#' Capture side effects
#' @export
#' @keywords internal
capture_side_effects <- function(fn) {

  # Capture error
  error <- testthat::capture_error(suppressMessages(suppressWarnings(fn())))
  # If no error, capture messages and warnings
  if (is.null(error)) {
    messages <- testthat::capture_messages(suppressWarnings(fn()))
    warnings <- testthat::capture_warnings(suppressMessages(fn()))
  } else {
    error <- error$message
    messages <- NULL
    warnings <- NULL
  }

  list(
    "error" = error,
    "warnings" = warnings,
    "messages" = messages,
    "has_side_effects" = any_side_effects(error, warnings, messages)
  )
}

get_side_effects <- function(string, envir) {

  # Get side effects
  # Note: Without rtilities2:: it complains
  # that it cannot find capture_side_effects
  catcher_string <- paste0(
    "rtilities2::capture_side_effects(",
    "function(){", string, "}",
    ")"
  )
  side_effects <- eval_string(catcher_string, envir = envir)

  side_effects
}

any_side_effects <- function(error, messages, warnings) {
  !(is.null(error) || length(error) == 0) ||
    !(is.null(messages) || length(messages) == 0) ||
    !(is.null(warnings) || length(warnings) == 0)
}
