

#   __________________ #< c402467242308fa93e897c7ecda52e75 ># __________________
#   Generate testthat tests                                                 ####


#' @title Creates testthat tests for selected code
#' @description Inserts relevant \code{expect_*} tests based
#'  on the evaluation of the selected code.
#'
#'  Example: If the selected code is the name of a data frame object,
#'  it will create an \code{\link[testthat:expect_equal]{expect_equal}}
#'  test for each column,
#'  along with a test of the column names.
#'
#'  Currently works for data frames, vectors
#'  and side effects (errors, warnings, messages).
#'
#'  List columns (like nested tibbles) are currently skipped.
#'
#'  See \code{Details} for how to set a key command.
#' @param selection String of code. (Character)
#'
#'  E.g. \code{"stop('This gives an expect_error test')"}.
#'
#'  N.B. Mainly intended for testing the addin programmatically.
#' @param indentation Indentation of the selection. (Numeric)
#'
#'  N.B. Mainly intended for testing the addin programmatically.
#' @param insert Whether to insert the expectations via
#'  \code{\link[rstudioapi:insertText]{rstudioapi::insertText()}}
#'  or return them. (Logical)
#'
#'  N.B. Mainly intended for testing the addin programmatically.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @return Inserts \code{\link[testthat:expect_equal]{testthat::expect_*}}
#'  unit tests for the selected code.
#'
#'  Returns \code{NULL} invisibly.
#' @details
#'  \subsection{How}{
#'  Parses and evaluates the selected code string
#'  within the parent environment.
#'  Depending on the output, it creates a set of unit tests
#'  (like \code{expect_equal(data[["column"]], c(1,2,3))}),
#'  and inserts them instead of the selection.
#'  }
#'  \subsection{How to set up a key command in RStudio}{
#'
#'  After installing the package.
#'  Go to:
#'
#'  \code{Tools >> Addins >> Browse Addins >> Keyboard Shortcuts}.
#'
#'  Find \code{"Insert Expectations"} and press its field under \code{Shortcut}.
#'
#'  Press desired key command, e.g. \code{Alt+E}.
#'
#'  Press \code{Apply}.
#'
#'  Press \code{Execute}.
#'  }
#' @importFrom utils capture.output head tail
#' @importFrom rlang :=
insertExpectationsAddin <- function(selection = NULL, insert = TRUE, indentation = 0) {


##  .................. #< b0cdec1a6600a8fd8cd7906a8d9db6b7 ># ..................
##  Check arguments                                                         ####


  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(x = selection, null.ok = TRUE,
                           add = assert_collection)
  checkmate::assert_flag(x = insert, add = assert_collection)
  checkmate::assert_number(x = indentation, lower = 0,
                           add = assert_collection)
  checkmate::reportAssertions(assert_collection)


##  .................. #< 862c178b529c00056dfd2fd58c0695d5 ># ..................
##  Get selection and context                                               ####


  # Get the selection and indentation
  if (is.null(selection)){
    selection <- get_selection()
    indentation <- get_indentation()
  }

  # Get parent environment
  parent_envir <- parent.frame()


##  .................. #< ecd748064e6b56931e034e2b083657a7 ># ..................
##  Create expectations                                                     ####


  if (selection != "") {

    # Check for side effects
    side_effects <- capture_parse_eval_side_effects(selection, parent_envir)
    has_side_effects <- side_effects[["has_side_effects"]]

    if (isTRUE(has_side_effects)) {

      # Create expectations for error, warnings, and messages
      expectations <- create_expectations_side_effect(
        side_effects, name = selection, indentation = indentation)
    } else {

      # Get data frame object
      obj <- eval_string(selection, envir = parent_envir)

      # Create expectations based on the type of the objects
      if (is.data.frame(obj)) {
        expectations <- create_expectations_data_frame(obj, name = selection,
                                                       indentation = indentation)
      } else if (is.vector(obj)) {
        expectations <- create_expectations_vector(obj, name = selection,
                                                   indentation = indentation)
      } else {
        stop("The selection is not of a currently supported class.")
      }
    }

    if (!isTRUE(insert)) {
      # Return the expectations instead of inserting them
      return(expectations)
    } else {
      # Insert the expectations
      insert_code(expectations)
    }
  }

  invisible()
}
