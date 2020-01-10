library(rtilities2)
context("check_arg()")

#### Outside functions ####

test_that("check_arg() works outside functions on scalars",{})

test_that("check_arg() works outside functions on vectors",{

  nll <- NULL
  char_vec <- c("a","b","c","d","e")
  float_vec <- (1:5)*1.05
  int_vec <- as.integer(1:5)
  fact_vec <- factor(char_vec)

  #### Type errors ####

  # NULL
  expect_invisible(check_arg(arg = NULL))
  expect_invisible(check_arg(arg = nll))
  expect_error(check_arg(arg = NULL,allow_null = FALSE),
               "'NULL' was NULL", fixed = TRUE)
  expect_error(check_arg(arg = nll, allow_null = FALSE),
               "'nll' was NULL", fixed = TRUE)

  # Character
  expect_invisible(check_arg(arg = char_vec, type_check_fn = is.character))
  expect_error(check_arg(arg = char_vec, type_check_fn = is.numeric),
               "'char_vec' did not have the right type, as checked with is.numeric().",
               fixed = TRUE)
  expect_error(check_arg(arg = char_vec, type_check_fn = function(x){is.numeric(x)}),
               "'char_vec' did not have the right type.",
               fixed = TRUE)
  expect_error(check_arg(arg = char_vec, type_check_fn = is.numeric, type_name = "numeric"),
               "'char_vec' must be of type numeric.",
               fixed = TRUE)

  # Numeric
  expect_invisible(check_arg(arg = float_vec, type_check_fn = is.numeric))
  expect_error(check_arg(arg = float_vec, type_check_fn = is.character),
               "'float_vec' did not have the right type, as checked with is.character().",
               fixed = TRUE)
  expect_error(check_arg(arg = float_vec, type_check_fn = function(x){is.character(x)}),
               "'float_vec' did not have the right type.",
               fixed = TRUE)
  expect_error(check_arg(arg = float_vec, type_check_fn = is.character, type_name = "character"),
               "'float_vec' must be of type character.",
               fixed = TRUE)
  expect_error(check_arg(arg = float_vec, type_check_fn = is.integer, type_name = "integer"),
               "'float_vec' must be of type integer",
               fixed = TRUE)

  # Integer
  expect_invisible(check_arg(arg = int_vec, type_check_fn = is.integer))
  expect_error(check_arg(arg = int_vec, type_check_fn = is.character),
               "'int_vec' did not have the right type, as checked with is.character().",
               fixed = TRUE)
  expect_error(check_arg(arg = int_vec, type_check_fn = function(x){is.character(x)}),
               "'int_vec' did not have the right type.",
               fixed = TRUE)
  expect_error(check_arg(arg = int_vec, type_check_fn = is.character, type_name = "character"),
               "'int_vec' must be of type character.",
               fixed = TRUE)
  expect_error(check_arg(arg = int_vec, type_check_fn = is.double),
               "'int_vec' did not have the right type, as checked with is.double().",
               fixed = TRUE)

  # Factor
  expect_invisible(check_arg(arg = fact_vec, type_check_fn = is.factor))
  expect_error(check_arg(arg = fact_vec, type_check_fn = is.character),
               "'fact_vec' did not have the right type, as checked with is.character().",
               fixed = TRUE)
  expect_error(check_arg(arg = fact_vec, type_check_fn = function(x){is.character(x)}),
               "'fact_vec' did not have the right type.",
               fixed = TRUE)
  expect_error(check_arg(arg = fact_vec, type_check_fn = is.character, type_name = "character"),
               "'fact_vec' must be of type character.",
               fixed = TRUE)
  expect_error(check_arg(arg = fact_vec, type_check_fn = is.double),
               "'fact_vec' did not have the right type, as checked with is.double().",
               fixed = TRUE)

  # Specify name
  expect_error(check_arg(arg = char_vec, type_check_fn = is.numeric, type_name = "numeric", name = "differentName"),
               "'differentName' must be of type numeric.",
               fixed = TRUE)

  #### Length errors ####

  #### Allowed values ####

  #### Naming ####



})

test_that("check_arg() works outside functions on lists",{})

test_that("check_arg() works outside functions on data frames",{})

test_that("check_arg() works outside functions on functions",{})


#### Inside functions ####

test_that("check_arg() works within functions on scalars",{})

test_that("check_arg() works within functions on vectors",{

  char_vec <- c("a","b","c","d","e")
  float_vec <- (1:5)*1.05
  int_vec <- as.integer(1:5)
  fact_vec <- factor(char_vec)

  # arg_checker <- function(){
  #
  # }



})

test_that("check_arg() works within functions on lists",{})

test_that("check_arg() works within functions on data frames",{})

test_that("check_arg() works within functions on functions",{})
