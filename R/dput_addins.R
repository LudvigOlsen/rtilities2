
#' @title Replaces selected code with its dput() output
#' @description RStudio Addin:
#'  Runs \code{\link[base:dput]{dput()}} on the selected code and inserts
#'  it instead of the selection.
#'
#'  See \code{Details} for how to set a key command.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @return Inserts the output of running \code{\link[base:dput]{dput()}} on the selected code.
#'
#'  Does not return anything.
#' @details
#'  \subsection{How}{
#'  Parses and evaluates the selected code string,
#'  applies \code{\link[base:dput]{dput()}} and
#'  inserts the output instead of the selection.
#'  }
#'  \subsection{How to set up a key command in RStudio}{
#'
#'  After installing the package.
#'  Go to:
#'
#'  \code{Tools >> Addins >> Browse Addins >> Keyboard Shortcuts}.
#'
#'  Find \code{"dput() Selected"} and press its field under \code{Shortcut}.
#'
#'  Press desired key command, e.g. \code{Alt+D}.
#'
#'  Press \code{Apply}.
#'
#'  Press \code{Execute}.
#'  }
dputSelectedAddin <- function() {
  # Get the selected variable name
  selection <- get_selection()

  # Get parent environment
  parent_envir <- parent.frame()

  if (selection != "") {
    dput_out <- apply_capture(selection, dput, envir = parent_envir)
    insert_code(dput_out)
  }
}

