create_expectations_data_frame <- function(data, name = NULL) {

  if (!is.data.frame(data))
    stop("'data' must be a data frame.")

  if (is.null(name)){
    name <- deparse(substitute(data))
  }

  # Create expect_equal expectations
  expectations <- plyr::llply(colnames(data), function(col_name) {
    # Get current column
    current_col <- data[[col_name]]
    # Left side of expectation
    x <- paste0(name, "[[\"", col_name, "\"]]")
    # Right side of expectation
    y <- capture.output(dput(current_col))

    # In case the column contains a list, which we
    # do not currently support
    if (length(y)>1) {
      return(NULL)
    }

    # Create expect_equal text
    create_expect_equal(x, y, add_tolerance = is.numeric(current_col))

  })

  # Append name expectation
  name_expectation <- create_name_expectation(data, name)
  expectations <- c(expectations, name_expectation)

  null_indices <- get_null_indices(expectations)
  if (length(null_indices) > 0){

    # Warn about skipped elements
    skipped_cols <- colnames(data)[null_indices]
    plural_s <- ifelse(length(skipped_cols) > 1, "s", "")
    warning(paste0(
      "Skipped column", plural_s, " ",
      paste0(skipped_cols, collapse = ", "),
      "."
    ))

    # Remove NULLS
    expectations <- expectations[-null_indices]
  }

  expectations
}

# Only split into multiple tests when all elements are named
create_expectations_vector <- function(data, name = NULL) {

  if (!is.vector(data))
    stop("'data' must be a vector.")

  if (is.null(name)){
    name <- deparse(substitute(data))
  }

  # Get non-empty and non-NULL element names
  element_names <- get_element_names(data, remove_empty_names = TRUE)

  # If all elements have names
  # We can test each individually
  if (length(element_names) > 0 && length(data) == length(element_names)){

    # Create expect_equal expectations
    expectations <- plyr::llply(element_names, function(elem_name) {
      # Get current column
      current_elem <- data[[elem_name]]
      # Left side of expectation
      x <- paste0(name, "[[\"", elem_name, "\"]]")
      # Right side of expectation
      y <- capture.output(dput(current_elem))

      # In case the column contains a list, which we
      # do not currently support
      if (length(y)>1) {
        return(NULL)
      }

      # Create expect_equal text
      create_expect_equal(x, y, add_tolerance = is.numeric(current_elem))

    })

    # Append name expectation
    name_expectation <- create_name_expectation(data, name)
    expectations <- c(expectations, name_expectation)

  } else {

    x <- name
    y <- capture.output(dput(data))

    expectations <- list(
      create_expect_equal(
        x, y,
        add_tolerance = is.numeric(data)
        )
      )
  }

  # Note: as list(1,2,3)[-integer()] returns and empty list
  # We must check if there's a NULL first
  null_indices <- get_null_indices(expectations)
  if (length(null_indices) > 0){

    # Warn about skipped elements
    plural_s <- ifelse(length(null_indices) > 1, "s", "")
    warning(paste0(
      "Skipped element", plural_s, " ",
      paste0(null_indices, collapse = ", "),
      "."
    ))

    # Remove NULLS
    expectations <- expectations[-null_indices]
  }

  expectations
}

# returns: expect_equal(names(name), c("a","b"))
create_name_expectation <- function(data, name) {
  x <- paste("names(", name, ")")
  y <- capture.output(dput(names(data)))
  create_expect_equal(x = x,
                      y = y,
                      add_tolerance = FALSE)
}
