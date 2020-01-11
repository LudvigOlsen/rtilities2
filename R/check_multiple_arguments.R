#' @title Report multiple checks at once
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Wraps a set of variable checks and reports the messages.
#'  The checks must return the message as a string.
#' @param ... The messages to report. As made with \code{\link[rtilities2:check_arg_str]{check_arg_str()}}.
#'
#'  \code{NULL} elements will be ignored, why it's fine to supply a check that doesn't fail.
#'
#'  Can be named. Names will only be used if \emph{all} the messages to be reported are named.
#' @param message_fn Function for communicating the error message to the user.
#'
#'  By default, \code{\link[base:stop]{stop}} is used to throw an error.
#'
#'  The \code{\link[base:return]{return}} function can be supplied in order to return the message as a string.
#' @param context Usually the name of the function calling the check. (Character)
#' @param message_type The type of message in lower case. Defaults to \code{"error"}.
#' @param first_n The maximum number of messages to message.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @return Throws an error with the collected error messages.
#'
#'  Does not return anything by default.
#' @examples
#' # Attach packages
#' library(rtilities2)
#'
#' # Defining a set of variables
#' x <- c(1,2,3,4,5)
#' y <- c("a","b","d")
#' z <- data.frame("o" = c(0,0,0), "u" = c(1,2,3))
#' \donttest{
#' # Reporting tests for all variables at once
#' # Tip: use check_arg_str
#' multicheck(
#'   check_arg_str(x, has_length = 6), # fails due to wrong length
#'   check_arg_str(y, type_check_fn = is.numeric), # fails due to wrong type
#'   check_arg_str(z, check_not_named = TRUE), # fails due to names
#'   check_arg_str(x), # passes
#'   context = "myFunction"
#' )
#'
#' # Named checks
#' multicheck(
#'   x.length = check_arg_str(x, has_length = 6), # fails due to wrong length
#'   y.type = check_arg_str(y, type_check_fn = is.numeric), # fails due to wrong type
#'   z.named = check_arg_str(z, check_not_named = TRUE), # fails due to names
#'   x.none = check_arg_str(x), # passes
#'   context = "myFunction"
#' )
#' }
multicheck <- function(...,
                       context = "",
                       message_fn = stop,
                       message_type = "error",
                       first_n = 5) {

  # Runs check_arg for each argument and gathers the first error for each
  # Formats and reports

  # Extract error messages
  checks <- list(...)
  # Remove NULL entries
  errors <- unlist(plyr::compact(checks))

  # Find number of failed checks to report
  n_original_errors <- length(errors)

  if (n_original_errors > 0){

    # Get the first n errors
    if (!is.null(first_n))
      errors <- head(errors, first_n)

    # update count
    n_errors <- length(errors)

    # Extract names if any
    error_names <- non_empty_names(errors)
    errors <- unname(errors)

    # Error indices if more than one error
    if (n_errors>1) error_indices <- as.character(seq_len(n_errors))
    else error_indices <- ""

    # In case not all errors were named
    # we use the error indices instead
    if (length(error_names) != n_errors){
      error_names <- error_indices
    }

    # Indentation to apply
    spaces <- "    "

    # Create error message
    errors_message <- paste0(
      paste0("Error ", error_names, ": ", as.character(errors)),
      collapse = paste0("\n", spaces))

    # Create message with total number of errors
    error_count_message <- paste0(ifelse(context == "", "F", "f"),
                                  "ound ",
                                  n_original_errors,
                                  " ", message_type,
                                  ifelse(n_original_errors > 1, "s", ""),
                                  ":\n",
                                  spaces)

    # Add context and error count
    if (context != "")
      errors_message <- paste0(context, " ", error_count_message, errors_message)
    else
      errors_message <- paste0(error_count_message, errors_message)

    # Add three dots if we don't show all error messages
    if (n_errors < n_original_errors){
      errors_message <- paste0(errors_message, "\n", spaces, "...")
    }

    # Send the message!
    message_fn(errors_message)
  }
}

