#' @title Insert pipe
#' @description RStudio Addin:
#'  Inserting \code{\link[magrittr:magrittr]{magrittr}} pipe
#'  \code{\link[magrittr:%>%]{%>%}}.
#'
#'  See \code{Details} for how to set up a key command.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
#' @export
#' @return Inserts \code{\%>\% }
#' @details
#'  \subsection{How to set up a key command in RStudio}{
#'
#'  After installing the package.
#'  Go to:
#'
#'  \code{Tools >> Addins >> Browse Addins >> Keyboard Shortcuts}.
#'
#'  Find \code{"Insert Pipe"} and press its field under \code{Shortcut}.
#'
#'  Press desired key command, e.g. \code{Alt+Shift+-}.
#'
#'  Press \code{Apply}.
#'
#'  Press \code{Execute}.
#'  }
#' @importFrom dplyr %>%
insertPipeAddin <- function() {
  rstudioapi::insertText("%>% ")
}
