#' @title Apply common checks to a passed argument
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Apply a set of common checks to a variable,
#'  like the \strong{type}, \strong{length}, \strong{allowed values}, and whether
#'  elements are \strong{named} properly.
#' @param ... Any argument will be passed directly to \code{check_arg}.
#' @param arg The variable to apply the checks to.
#'  Can for instance be used to check the input of a function argument.
#' @param type_check_fn Function for checking the type of \code{arg}.
#'  Like \code{\link[base:is.character]{is.character}} and \code{\link[base:is.numeric]{is.numeric}}.
#'
#'  When passing anonymous functions or functions with ambigious names,
#'  consider passing \code{type_name} as well, for better error messages.
#' @param has_length A length to check against. Throws an error if \code{arg} has a \emph{different} length.
#' @param not_length A length to check against. Throw an error if \code{arg} has \emph{this} length.
#' @param allowed_values The values that \code{arg} is allowed to contain.
#'
#'  This check is implemented by applying \code{\link[base:setdiff]{setdiff()}}
#'  to the recursively unlisted values in \code{arg}.
#'  Note: If \code{arg} contains values of type character, the unlisting will
#'  convert all elements to type character.
#' @param in_range The numeric range the values in \code{arg} should lie within.
#'  Vector with 2 elements: \code{c(min, max)}.
#'
#'  1) Throws error if the minimum value in \code{arg} is lower than the first value of \code{in_range}.
#'
#'  2) Throws error if the maximum value in \code{arg} is higher than the second value of \code{in_range}.
#'
#'  \code{\link[base:range]{range()}} is called on the recursively unlisted values in \code{arg} and
#'  compared to the sorted \code{in_range}.
#' @param check_not_named Whether to ensure that \emph{none} of the elements are named. \emph{Not recursive}.
#' @param check_all_named Whether to ensure that \emph{all} of the elements are named. \emph{Not recursive}.
#' @param check_all_uniquely_named Whether to ensure that \emph{all} of the elements are \emph{uniquely} named. \emph{Not recursive}.
#' @param allow_null Whether \code{arg} is allowed to be \code{NULL}.
#'  If \code{TRUE} and \code{arg} is \code{NULL}, the remaining checks are ignored.
#' @param arg_name Name of the argument, for improved error messaging.
#'  If \code{NULL}, it is inferred via \code{deparse(substitute(arg))}.
#' @param type_name Name of the type(s) checked for with \code{type_check_fn}, for improved error messaging.
#'  If \code{NULL}, it may be inferred via \code{deparse(substitute(type_check_fn))}.
#' @param message_fn Function for communicating the error message to the user.
#'
#'  By default, \code{\link[base:stop]{stop}} is used to throw an error.
#'
#'  The \code{\link[base:return]{return}} function is treated specially: If a check doesn't pass,
#'  the error message is returned as a string.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @return Throws an error with a suitable error message if any of the checks fail.
#'
#'  Does not return anything by default.
check_arg <- function(arg,
                      type_check_fn = NULL,
                      has_length = NULL,
                      not_length = NULL,
                      allowed_values = NULL,
                      in_range = NULL,
                      check_not_named = FALSE,
                      check_all_named = FALSE,
                      check_all_uniquely_named = FALSE,
                      allow_null = TRUE,
                      arg_name = NULL,
                      type_name = NULL,
                      message_fn = stop){

  # Get current frame for extracting argument names
  current_frame <- sys.frame(which = sys.nframe())

  # Get name from parent environment
  if (is.null(arg_name)){
    arg_name <- tryCatch( # Doesn't really work to do this in subfunction
      deparse(substitute(expr = arg, env = current_frame)),
      error = function(e) {
        return(NULL)
      },
      warning = function(w) {
        return(NULL)
      }
    )
    if (is.null(arg_name)){
      stop("Could not extract arg_name from 'arg'. Please specify it as a character string.")
    }
  }
  if (!is.character(arg_name) || length(arg_name) != 1){
    stop("'arg_name' must be a character string or NULL.")
  }

  # Add '' around arg_name
  arg_name <- paste0("'", arg_name, "'")

  # return() is a special message_fn for returning the string
  return_string <- identical(message_fn, return)

  if (!(isTRUE(allow_null) && is.null(arg))){

    # Check if arg is NULL
    arg_not_null <- check_arg_not_null(
      arg = arg,
      arg_name = arg_name,
      message_fn = message_fn)
    if (!is.null(arg_not_null) && isTRUE(return_string)) return(arg_not_null)

    # Check length of arg
    arg_has_length <- check_arg_has_length(
      arg = arg,
      has_length = has_length,
      arg_name = arg_name,
      message_fn = message_fn)
    if (!is.null(arg_has_length) && isTRUE(return_string)) return(arg_has_length)

    arg_not_length <- check_arg_not_length(
      arg = arg,
      not_length = not_length,
      arg_name = arg_name,
      message_fn = message_fn)
    if (!is.null(arg_not_length) && isTRUE(return_string)) return(arg_not_length)

    # Check type
    # TODO Perhaps we want to pass the name of the parent function to the error message?
    type_check_fn_name <- tryCatch({
      tcfn_name <- deparse(substitute(expr = type_check_fn, env = current_frame))
      if (!(is.character(tcfn_name) && length(tcfn_name)==1) ||
          (nchar(tcfn_name)>9 && substr(tcfn_name, 1, 9) == "function(")){
        tcfn_name <- NULL
      }
      tcfn_name
      }, error = function(e) {
        return(NULL)
      }, warning = function(w) {
        return(NULL)
      }
    )

    arg_type <- check_arg_type(
      arg = arg,
      type_check_fn = type_check_fn,
      type_check_fn_name = type_check_fn_name,
      type_name = type_name,
      current_frame = current_frame,
      arg_name = arg_name,
      message_fn = message_fn
    )
    if (!is.null(arg_type) && isTRUE(return_string)) return(arg_type)

    # Check if any values are disallowed
    arg_values_allowed <- check_arg_values_allowed(
      arg = arg,
      allowed_values = allowed_values,
      arg_name = arg_name,
      message_fn = message_fn)
    if (!is.null(arg_values_allowed) && isTRUE(return_string)) return(arg_values_allowed)

    # Check that the values of arg is in numeric range
    arg_in_range <- check_arg_in_range(
      arg = arg,
      in_range = in_range,
      arg_name = arg_name,
      message_fn = message_fn)
    if (!is.null(arg_in_range) && isTRUE(return_string)) return(arg_in_range)

    ## Names
    # TODO Check up on cases where colnames()
    # returns strings but not names()

    # Check that arg has no name
    arg_not_named <- check_arg_not_named(
      arg = arg,
      check_not_named = check_not_named,
      arg_name = arg_name,
      message_fn = message_fn)
    if (!is.null(arg_not_named) && isTRUE(return_string)) return(arg_not_named)

    # Check that all elements have names
    arg_all_named <- check_arg_all_named(
      arg = arg,
      check_all_named = check_all_named,
      arg_name = arg_name,
      message_fn = message_fn)
    if (!is.null(arg_all_named) && isTRUE(return_string)) return(arg_all_named)

    # Check that all elements have a unique name
    arg_all_uniquely_named <- check_arg_all_uniquely_named(
      arg = arg,
      check_all_uniquely_named = check_all_uniquely_named,
      arg_name = arg_name,
      message_fn = message_fn)
    if (!is.null(arg_all_uniquely_named) && isTRUE(return_string)) return(arg_all_uniquely_named)

  }

}




#' @describeIn check_arg Wrapper for returning the message as a string.
check_arg_str <- function(..., message_fn = return){
  # Wrapper for returning error message as string
  check_arg(..., message_fn = message_fn)
}

