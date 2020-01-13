library(rtilities2)
context("is_between()")

test_that("is_between() works", {
  expect_true(is_between(0, -3, 3))
  expect_equal(
    is_between(c(-4, -3, -2, -1, 0, 1, 2, 3, 4), -3, 3),
    c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  )
  expect_equal(
    is_between(c(-4, -3, -2, -1, 0, 1, 2, 3, 4), -3, 3, include_limits = TRUE),
    c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
  )
  expect_equal(is_between(NA, -3, 3), NA)
  expect_equal(is_between(NaN, -3, 3), NA)
  expect_error(
    is_between(NaN, -3, 3, allow_na = FALSE),
    "1 assertions failed:\n * Variable 'x': Contains missing values (element 1).",
    fixed = TRUE
  )
  expect_error(
    is_between(NA, -3, 3, allow_na = FALSE),
    "1 assertions failed:\n * Variable 'x': Contains missing values (element 1).",
    fixed = TRUE
  )
  expect_error(is_between(3, NA, 3),
    "1 assertions failed:\n * Variable 'lower': May not be NA.",
    fixed = TRUE
  )
  expect_error(is_between(3, -3, NA),
    "1 assertions failed:\n * Variable 'upper': May not be NA.",
    fixed = TRUE
  )
  expect_error(
    is_between(3, -3, 4, NULL),
    # Different in test suite and here. Does checkmate wrap lines?
    "1 assertions failed:\n * Variable 'include_limits': Must be of type 'logical flag', not\n * 'NULL'.",
    fixed = TRUE
  )
})
