#' @title Wraps the selection with paste0
#' @description Splits the selection every n characters
#'  and inserts it in a \code{\link[base:paste0]{paste0()}} call.
#'
#'  See \code{Details} for how to set a key command.
#' @param selection String of code. (Character)
#'
#'  N.B. Mainly intended for testing the addin programmatically.
#' @param insert Whether to insert the wrapped text via
#'  \code{\link[rstudioapi:insertText]{rstudioapi::insertText()}}
#'  or return it. (Logical)
#'
#'  N.B. Mainly intended for testing the addin programmatically.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @return Inserts \code{paste0("first n chars", "next n chars")} etc.
#'
#'  Returns \code{NULL} invisibly.
#' @details
#'  \subsection{How to set up a key command in RStudio}{
#'
#'  After installing the package.
#'  Go to:
#'
#'  \code{Tools >> Addins >> Browse Addins >> Keyboard Shortcuts}.
#'
#'  Find \code{"Insert Expectations"} and press its
#'  field under \code{Shortcut}.
#'
#'  Press desired key command, e.g. \code{Alt+P}.
#'
#'  Press \code{Apply}.
#'
#'  Press \code{Execute}.
#'  }
wrapStringAddin <- function(selection = NULL, insert = TRUE) {

  # Add asserts
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(x = selection, null.ok = TRUE,
                           add = assert_collection)
  checkmate::assert_flag(x = insert, add = assert_collection)
  checkmate::reportAssertions(assert_collection)

  # Get the selected variable name
  # either from argument or from selection
  selection <- do_if(is.null(selection),
                     fn = get_selection,
                     otherwise = selection
  )

  # Get parent environment
  parent_envir <- parent.frame()

  if (selection != "") {

    wrapped <- split_to_paste0(selection)

    if (!isTRUE(insert)) {
      # Return the wrapped string instead of inserting it
      return(wrapped)
    } else {
      # Insert the wrapped
      insert_code(list(wrapped))
    }
  }

  invisible()
}
