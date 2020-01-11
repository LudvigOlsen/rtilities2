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

  # Test when using check_arg or other random functions and objects

  # Using check_arg by accident
  expect_error(multicheck(
    mate = check_arg(arg = NULL),
    dude = check_arg(arg = nll, allow_null = FALSE),
    type_numeric = check_arg(arg = char_vec, type_check_fn = is.numeric),
    type_character = check_arg(arg = float_vec, type_check_fn = is.character),
    context = "myFunction",
    first_n = 2
  ),
  "'nll' was NULL.",
  fixed = TRUE)

  # Different types of objects and functions
  expect_error(multicheck(
    sum_ = sum(2,3),
    string = "direct string",
    fn = check_arg_str,
    context = "myFunction",
    first_n = 5
  ),
  "myFunction found 3 errors:\n    Error sum_: 5\n    Error string: direct string\n    Error fn: function (..., message_fn = return) \n{\n    check_arg(..., message_fn = message_fn)\n}",
  fixed = TRUE)
})
