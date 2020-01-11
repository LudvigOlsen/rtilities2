library(rtilities2)
context("multicheck()")

test_that("multicheck() works with check_args_str()",{

  #### Create data ####
  nll <- NULL
  char_vec <- c("a","b","c","d","e")
  float_vec <- (1:5)*1.05
  int_vec <- as.integer(1:5)
  fact_vec <- factor(char_vec)

  expect_error(multicheck(
    check_arg_str(arg = NULL),
    check_arg_str(arg = nll, allow_null = FALSE),
    check_arg_str(arg = char_vec, type_check_fn = is.numeric),
    check_arg_str(arg = float_vec, type_check_fn = is.character)
  ),
  "Found 3 errors:\n    Error 1: 'nll' was NULL.\n    Error 2: 'char_vec' did not have the right type, as checked with is.numeric().\n    Error 3: 'float_vec' did not have the right type, as checked with is.character().",
  fixed = TRUE)
  expect_error(multicheck(
    check_arg_str(arg = NULL),
    check_arg_str(arg = nll, allow_null = FALSE),
    check_arg_str(arg = char_vec, type_check_fn = is.numeric),
    check_arg_str(arg = float_vec, type_check_fn = is.character),
    context = "myFunction"
  ),
  "myFunction found 3 errors:\n    Error 1: 'nll' was NULL.\n    Error 2: 'char_vec' did not have the right type, as checked with is.numeric().\n    Error 3: 'float_vec' did not have the right type, as checked with is.character().",
  fixed = TRUE)

  expect_invisible(multicheck(
    check_arg_str(arg = NULL)))
  expect_invisible(multicheck(
    check_arg_str(arg = NULL), context = "myFunction"))

  # Named conds
  expect_error(multicheck(
    mate = check_arg_str(arg = NULL),
    dude = check_arg_str(arg = nll, allow_null = FALSE),
    type_numeric = check_arg_str(arg = char_vec, type_check_fn = is.numeric),
    type_character = check_arg_str(arg = float_vec, type_check_fn = is.character),
    context = "myFunction"
  ),
  "myFunction found 3 errors:\n    Error dude: 'nll' was NULL.\n    Error type_numeric: 'char_vec' did not have the right type, as checked with is.numeric().\n    Error type_character: 'float_vec' did not have the right type, as checked with is.character().",
  fixed = TRUE)

  # Only showing first 2 errors
  expect_error(multicheck(
    mate = check_arg_str(arg = NULL),
    dude = check_arg_str(arg = nll, allow_null = FALSE),
    type_numeric = check_arg_str(arg = char_vec, type_check_fn = is.numeric),
    type_character = check_arg_str(arg = float_vec, type_check_fn = is.character),
    context = "myFunction",
    first_n = 2
  ),
  "myFunction found 3 errors:\n    Error dude: 'nll' was NULL.\n    Error type_numeric: 'char_vec' did not have the right type, as checked with is.numeric().\n    ...",
  fixed = TRUE)

})
