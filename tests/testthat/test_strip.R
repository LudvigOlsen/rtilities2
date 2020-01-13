library(rtilities2)
context("strip()")

test_that("strip() works",{

  strings <- c(
    "Hello! I am George.  \n\rDon't call me Frank!",
    "    \tAs that, is, not, my, name!",
    "", NA, ".!\"'"
  )

  expect_equal(strip(strings),
               c("Hello I am George Dont call me Frank", " As that is not my name", "", NA, ""))
  expect_equal(strip(strings, remove_spaces = TRUE),
               c("HelloIamGeorgeDontcallmeFrank", "Asthatisnotmyname", "", NA, ""))
  expect_error(strip(strings, allow_na = FALSE),
               "* Variable 'strings': Contains missing values (element 4).",
               fixed=TRUE)
  expect_error(strip(NA, allow_na = FALSE),
               " * Variable 'strings': Contains missing values (element 1).",
               fixed=TRUE)
  expect_error(strip(NULL),
               "* Variable 'strings': Must be of type 'character', not 'NULL'.",
               fixed=TRUE)
  expect_error(strip(23),
               "* Variable 'strings': Must be of type 'character', not 'double'.",
               fixed=TRUE)
  expect_equal(strip(strings, replacement = " "),
               c("Hello I am George Don t call me Frank ",
                 " As that is not my name ", "", NA, " "))

})
