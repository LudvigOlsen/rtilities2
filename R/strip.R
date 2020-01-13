#' @title Strips strings of non-alphanumeric characters
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  1) Removes any character that is not alphanumeric or a space.
#'
#'  2) Reduces multiple consequtive whitespaces to a single whitespace.
#'
#'  Can for instance be used to simplify error messages before checking them.
#' @param strings Vector of strings. (Character)
#' @param allow_na Whether to allow \code{strings} to contain \code{NA}s. (Logical)
#' @param replacement What to replace blocks of punctuation with. (Character)
#' @param remove_spaces Whether to remove all whitespaces. (Logical)
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @details
#' 1) \code{gsub("[^[:alnum:][:blank:]]", replacement, strings))}
#' 3) \code{gsub("[[:blank:]]+", " ", strings)} (Or \code{""} if \code{remove_spaces} is \code{TRUE})
#' @examples
#' # Attach packages
#' library(rtilities2)
#'
#' strip(c(
#'   "Hello! I am George.  \n\rDon't call me Frank!",
#'   "    \tAs that, is, not, my, name!"
#' ))
#'
#' strip(c(
#'   "Hello! I am George.  \n\rDon't call me Frank!",
#'   "    \tAs that, is, not, my,     name!"
#' ), remove_spaces = TRUE)
strip <- function(strings, replacement = "", remove_spaces = FALSE, allow_na = TRUE){

  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_flag(allow_na, add = assert_collection)
  checkmate::reportAssertions(assert_collection) # Must check allow_na first!
  checkmate::assert_character(x = strings, any.missing = allow_na, add = assert_collection)
  checkmate::assert_string(replacement, add = assert_collection)
  checkmate::assert_flag(remove_spaces, add = assert_collection)
  checkmate::reportAssertions(assert_collection)

  # # Replace all non-alphanumeric and non-space
  strings <- gsub("[^[:alnum:][:blank:]]", replacement, strings)
  # Reduce multiple consequtive whitespaces to a single whitespace (or non if specified)
  gsub("[[:blank:]]+", ifelse(isTRUE(remove_spaces), "", " "), strings)
}
