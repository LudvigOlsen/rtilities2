#' @title Creates testthat tests for selected code
#' @description Inserts relevant \code{expect_*} tests based
#'  on the evaluation of the selected code.
#'
#'  Example: If the selected code is the name of a data frame object,
#'  it will create an \code{\link[testthat:expect_equal]{expect_equal}}
#'  test for each column,
#'  along with a test of the column names.
#'
#'  Currently works for data frames and vectors.
#'
#'  See \code{Details} for how to set a key command.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @return Inserts \code{\link[testthat:expect_equal]{testthat::expect_*}} unit tests for the selected code.
#'
#'  Does not return anything.
#' @details
#'  \subsection{How}{
#'  Parses and evaluates the selected code string within the parent environment.
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
#' @importFrom utils capture.output head
#' @importFrom rlang :=
insertExpectationsAddin <- function(){
  # Get the selected variable name
  selection <- get_selection()

  # Get parent environment
  parent_envir <- parent.frame()

  if (selection != "") {
    # Get data frame object
    obj <- eval_string(selection, envir = parent_envir)

    # Create expectations based on the type of the objects
    if (is.data.frame(obj)){
      expectations <- create_expectations_data_frame(obj, name = selection)
    } else if (is.vector(obj)){
      expectations <- create_expectations_vector(obj, name = selection)
    } else {
      stop("The selection is not of a currently supported class.")
    }

    # Insert the expectations
    insert_code(expectations)

  }

}
