create_expectations_data_frame <- function(data, name = NULL) {
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = data, add = assert_collection)
  checkmate::assert_string(
    x = name, min.chars = 1, null.ok = TRUE,
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)

  if (is.null(name)) {
    name <- deparse(substitute(data))
  }

  # Create expect_equal expectations
  expectations <- plyr::llply(colnames(data), function(col_name) {
    # Get current column
    current_col <- data[[col_name]]
    if (is.list(current_col)) {
      return(NULL)
    }

    # Left side of expectation
    x <- paste0(name, "[[\"", col_name, "\"]]")
    # Right side of expectation
    y <- capture.output(dput(current_col))
    # In case dput spanned multiple lines
    # we collapse them to one string
    y <- collapse_strings(y)

    # Sanity check
    if (length(y) > 1) {
      return(NULL)
    }

    # Create expect_equal text
    create_expect_equal(x, y, add_tolerance = is.numeric(current_col))
  })

  # Append name expectation
  name_expectation <- create_name_expectation(data, name)
  expectations <- c(expectations, name_expectation)

  null_indices <- get_null_indices(expectations)
  if (length(null_indices) > 0) {

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
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_vector(x = data, add = assert_collection)
  checkmate::assert_string(
    x = name, min.chars = 1, null.ok = TRUE,
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)

  if (is.null(name)) {
    name <- deparse(substitute(data))
  }

  # Get non-empty and non-NULL element names
  element_names <- get_element_names(data, remove_empty_names = TRUE)

  # If all elements have names
  # We can test each individually
  if (length(element_names) > 0 &&
    length(data) == length(element_names)) {

    # Create expect_equal expectations
    expectations <- plyr::llply(element_names, function(elem_name) {
      # Get current column
      current_elem <- data[[elem_name]]
      # Left side of expectation
      x <- paste0(name, "[[\"", elem_name, "\"]]")
      # Right side of expectation
      y <- capture.output(dput(current_elem))
      # In case dput spanned multiple lines
      # we collapse them to one string
      y <- collapse_strings(y)

      # sanity check
      if (length(y) > 1) {
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
    # In case dput spanned multiple lines
    # we collapse them to one string
    y <- collapse_strings(y)

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
  if (length(null_indices) > 0) {

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

create_expectations_side_effect <- function(side_effects, name = NULL) {
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_list(
    x = side_effects, all.missing = FALSE,
    len = 4, add = assert_collection
  )
  checkmate::assert_names(
    x = names(side_effects),
    identical.to = c(
      "error", "warnings",
      "messages", "has_side_effects"
    ),
    type = "named"
  )
  checkmate::assert_string(
    x = name, min.chars = 1, null.ok = TRUE,
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)

  if (is.null(name)) { # TODO Not sure this would work (not used currently)
    name <- deparse(substitute(side_effects))
  }

  expectations <- list()

  if (!is.null(side_effects$error)) {
    expectations <- c(expectations, list(
      create_expect_side_effect(
        name, side_effects$error,
        side_effect_type = "error"
      )
    ))
  } else {
    if (!is.null(side_effects$warnings)) {
      expectations <- c(
        expectations,
        plyr::llply(side_effects$warnings, function(w) {
          create_expect_side_effect(
            name, w,
            side_effect_type = "warning"
          )
        })
      )
    }
    if (!is.null(side_effects$messages)) {
      expectations <- c(
        expectations,
        plyr::llply(side_effects$messages, function(m) {
          create_expect_side_effect(
            name, m,
            side_effect_type = "message"
          )
        })
      )
    }
  }

  expectations
}

# returns: expect_equal(names(name), c("a","b"))
create_name_expectation <- function(data, name) {
  x <- paste("names(", name, ")")
  y <- capture.output(dput(names(data)))
  create_expect_equal(
    x = x,
    y = y,
    add_tolerance = FALSE
  )
}

create_expect_equal <- function(x, y,
                                add_tolerance = FALSE,
                                spaces = 2) {
  spaces_string <- create_space_string(n = spaces)
  if (isTRUE(add_tolerance)) {
    tolerance_string <- paste0(",\n", spaces_string, "tolerance = 1e-4")
  } else {
    tolerance_string <- ""
  }

  # In case a string has \n, \t, etc.
  y <- escape_metacharacters(y)

  paste0(
    "expect_equal(\n",
    spaces_string,
    x,
    ",\n",
    spaces_string,
    y,
    tolerance_string,
    ")"
  )
}

create_expect_side_effect <- function(x, y,
                                      side_effect_type = "error",
                                      spaces = 2) {
  checkmate::assert_choice(
    x = side_effect_type,
    choices = c("error", "warning", "message")
  )

  spaces_string <- create_space_string(n = spaces)

  expect_fn <- dplyr::case_when(
    side_effect_type == "error" ~ "expect_error",
    side_effect_type == "warning" ~ "expect_warning",
    side_effect_type == "message" ~ "expect_message",
    TRUE ~ "" # Won't get here anyway
  )

  y <- escape_metacharacters(y)
  y <- split_to_paste0(y, spaces = spaces)

  paste0(
    expect_fn, "(\n",
    spaces_string,
    x,
    ",\n",
    spaces_string,
    y,
    ",\n",
    spaces_string,
    "fixed = TRUE",
    ")"
  )
}
