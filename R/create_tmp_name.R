
#' @title Create unique temporary name
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Adds underscore to \code{name} until it is unique in \code{data}.
#'
#'  Use case: Adding a temporary index column to a data frame within
#'  a function, where you don't want to overwrite an existing column.
#' @param data Any data structure where names can be accessed with \code{\link[base:names]{names()}}.
#'  E.g. a data frame or list.
#' @param name Initial name to try.
#'  If it's already in \code{data}, an underscore will be appended until it is unique.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @return Name not already in \code{data}.
#' @examples
#' # Attach packages
#' library(rtilities2)
#'
#' # Create data frame
#' df <- data.frame("a" = c(1,2), "b" = c(2,3))
#' # Create named list
#' nl <- list("a" = c(1,2), "b" = c(2,3))
#'
#' # Create unique name for the data frame
#' # As "a" is already in df, it appends a "_"
#' create_tmp_name(df, "a")
#'
#' # Create unique name for the list
#' # As "a" is already in df, it appends a "_"
#' create_tmp_name(nl, "a")
#'
#' # Using it within a function
#' # in order not to overwrite a user's column
#' foo <- function(data){
#'   # Create unique temporary name
#'   tmp_colname <- create_tmp_name(data, ".tmp_index_")
#'
#'   # Create index column with the name
#'   data[[tmp_colname]] <- seq_len(nrow(data))
#'
#'   # Do something that reorders the data set
#'   data <- dplyr::sample_frac(data)
#'
#'   # Order by the temporary index
#'   data <- dplyr::arrange_at(data, tmp_colname)
#'
#'   # Remove the temporary index
#'   data[[tmp_colname]] <- NULL
#'
#'   data
#' }
#'
#' foo(df)
create_tmp_name <- function(data, name = ".tmp_") {

  # Assert input
  # 'data' can be anything where names() can be used,
  # so we don't add assertions for that
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(x = name, min.chars = 1, add = assert_collection)
  checkmate::reportAssertions(assert_collection)

  # Extract non-empty names from 'data'
  data_names <- non_empty_names(data)

  # Append underscore until unique
  while (name %in% data_names) {
    name <- paste0(name, "_")
  }

  name
}
