#' @title Is between
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Tests if \code{x} is between \code{lower} and \code{upper}.
#'  By default, the limits are not included.
#' @param x Number(s) to test. (Numeric)
#' @param lower Lower limit. (Numeric)
#' @param upper Upper limit. (Numeric)
#' @param include_limits Whether to include limits in the allowed values for \code{x}. (Logical)
#' @param allow_na Whether to allow \code{x} to contain \code{NA}s.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @details
#' When include_limits is FALSE:
#' \code{x > lower & x < upper}
#'
#' When include_limits is TRUE:
#' \code{x >= lower & x <= upper}
#' @examples
#' # Attach packages
#' library(rtilities2)
#'
#' is_between(3, 1, 5) # TRUE
#' is_between(-1, 1, 5) # FALSE
#' is_between(1, 1, 5) # FALSE
#' is_between(1, 1, 5, include_limits = TRUE) # TRUE
#' is_between(5, 1, 5) # FALSE
#' is_between(5, 1, 5, include_limits = TRUE) # TRUE
is_between <- function(x, lower, upper, include_limits = FALSE, allow_na = TRUE) {
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_flag(allow_na, add = assert_collection)
  checkmate::reportAssertions(assert_collection) # Must check allow_na first!
  checkmate::assert_numeric(x = x, any.missing = allow_na, add = assert_collection)
  checkmate::assert_number(x = lower, add = assert_collection)
  checkmate::assert_number(x = upper, add = assert_collection)
  checkmate::assert_flag(x = include_limits, add = assert_collection)
  checkmate::reportAssertions(assert_collection)

  # Checks if x is between lower and upper
  # Must be single & to work in dplyr::mutate
  if (isTRUE(include_limits)) {
    return(x >= lower & x <= upper)
  } else {
    return(x > lower & x < upper)
  }
}
