library(rtilities2)
context("create_tmp_name()")

test_that("create_tmp_name() works", {

  #### Create data ####
  nll <- NULL
  df <- data.frame("a" = c(1, 2, 3, 4, 5), "b" = c(2, 3, 4, 5, 6))
  nl <- list("a" = c(1, 2, 3, 4, 5), "b" = c(2, 3, 4, 5, 6))

  expect_equal(create_tmp_name(df, "a"), "a_")
  expect_equal(create_tmp_name(df, "c"), "c")

  expect_equal(create_tmp_name(nl, "a"), "a_")
  expect_equal(create_tmp_name(nl, "c"), "c")

  expect_error(create_tmp_name(nl, 3),
    "1 assertions failed:\n * Variable 'name': Must be of type 'string', not 'double'.",
    fixed = TRUE
  )
  expect_error(create_tmp_name(nl, ""),
    "1 assertions failed:\n * Variable 'name': Must have at least 1 characters.",
    fixed = TRUE
  )
  # TODO NA and NULL test
  # expect_error(create_tmp_name(nl, NA),
  #              "'name' must be of type character.",
  #              fixed = TRUE)

  # Using it within a function
  # in order not to overwrite a user's column
  foo <- function(data) {
    # Create unique temporary name
    tmp_colname <- create_tmp_name(data, ".tmp_index_")

    # Create index column with the name
    data[[tmp_colname]] <- seq_len(nrow(data))

    # Do something that reorders the data set
    data <- dplyr::sample_frac(data)

    # Order by the temporary index
    data <- dplyr::arrange_at(data, tmp_colname)

    # Remove the temporary index
    data[[tmp_colname]] <- NULL

    data
  }

  expect_identical(foo(df), df)
})
