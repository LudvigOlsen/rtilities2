library(rtilities2)
context("check_arg()")

#### Outside functions ####

# TODO test_that("check_arg() works outside functions on scalars",{})

test_that("check_arg() works outside functions on vectors",{

  #### Create data ####
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

  # Specify arg_nam
  expect_error(check_arg(arg = char_vec, type_check_fn = is.numeric, type_name = "numeric", arg_nam = "differentName"),
               "'differentName' must be of type numeric.",
               fixed = TRUE)

  #### Length errors ####

  expect_invisible(check_arg(arg = char_vec, has_length = 5))
  expect_invisible(check_arg(arg = nll, has_length = 5))
  expect_error(check_arg(arg = char_vec, has_length = 4),
               "'char_vec' had length 5 but must have length 4.",
               fixed = TRUE)
  expect_error(check_arg(arg = char_vec, has_length = 4, arg_nam = "differentName"),
               "'differentName' had length 5 but must have length 4.",
               fixed = TRUE)

  expect_invisible(check_arg(arg = char_vec, not_length = 10))
  expect_invisible(check_arg(arg = nll, not_length = 5))
  expect_error(check_arg(arg = char_vec, not_length = 5),
               "'char_vec' cannot have length 5.",
               fixed = TRUE)
  expect_error(check_arg(arg = char_vec, not_length = 5, arg_nam = "differentName"),
               "'differentName' cannot have length 5.",
               fixed = TRUE)

  #### Allowed values ####

  # Characters
  expect_invisible(check_arg(arg = nll, allowed_values = c("a","b")))
  expect_invisible(check_arg(arg = char_vec, allowed_values = char_vec))
  expect_error(check_arg(arg = char_vec, allowed_values = c("a","b")),
               "'char_vec' contained 3 unique disallowed values: c, d, e.",
               fixed = TRUE)
  expect_error(check_arg(arg = char_vec, allowed_values = c("b")),
               "'char_vec' contained 4 unique disallowed values: a, c, d, ...",
               fixed = TRUE)
  expect_error(check_arg(arg = char_vec, allowed_values = c("b"), arg_nam = "differentName"),
               "'differentName' contained 4 unique disallowed values: a, c, d, ...",
               fixed = TRUE)

  # Numeric
  expect_invisible(check_arg(arg = float_vec, allowed_values = float_vec))
  expect_error(check_arg(arg = float_vec, allowed_values = c("a","b")),
               "'float_vec' contained 5 unique disallowed values: 1.05, 2.1, 3.15, ...",
               fixed = TRUE)
  expect_error(check_arg(arg = float_vec, allowed_values = c(1.05, 4.20)),
               "'float_vec' contained 3 unique disallowed values: 2.1, 3.15, 5.25.",
               fixed = TRUE)

  # Integer
  expect_invisible(check_arg(arg = int_vec, allowed_values = int_vec))
  expect_error(check_arg(arg = int_vec, allowed_values = c("a","b")),
               "'int_vec' contained 5 unique disallowed values: 1, 2, 3, ...",
               fixed = TRUE)
  expect_error(check_arg(arg = int_vec, allowed_values = c(1.0, 2.0)),
               "'int_vec' contained 3 unique disallowed values: 3, 4, 5.",
               fixed = TRUE)

  # Factor
  # Important that values, not level indices, are checked
  expect_invisible(check_arg(arg = fact_vec, allowed_values = fact_vec))
  expect_invisible(check_arg(arg = fact_vec, allowed_values = c("a","b","c","d","e","f","g"))) # extra allowed vals
  expect_error(check_arg(arg = fact_vec, allowed_values = c("a","b")),
               "'fact_vec' contained 3 unique disallowed values: c, d, e.",
               fixed = TRUE)
  expect_error(check_arg(arg = fact_vec, allowed_values = c(1, 2)),
               "'fact_vec' contained 5 unique disallowed values: a, b, c, ...",
               fixed = TRUE)

  #### In range ####

  # Numeric
  expect_invisible(check_arg(arg = float_vec, in_range = c(min(float_vec),max(float_vec))))
  expect_invisible(check_arg(arg = float_vec, in_range = c(min(float_vec)-1,max(float_vec)+1)))
  expect_invisible(check_arg(arg = float_vec, in_range = c(max(float_vec), min(float_vec))))
  expect_error(check_arg(arg = float_vec, in_range = c("a","b")),
               "Cannot check numeric range when 'in_range' is not numeric.",
               fixed = TRUE)
  expect_error(check_arg(arg = char_vec, in_range = c(1,2)),
               "Cannot check numeric range when 'arg' is not numeric.",
               fixed = TRUE)
  expect_error(check_arg(arg = float_vec, in_range = c(min(float_vec), max(float_vec)-1)),
               "'float_vec' contained element with value outside the allowed numeric range.",
               fixed = TRUE)
  expect_error(check_arg(arg = float_vec, in_range = c(min(float_vec)+1, max(float_vec))),
               "'float_vec' contained element with value outside the allowed numeric range.",
               fixed = TRUE)
  expect_error(check_arg(arg = float_vec, in_range = c(min(float_vec)+1, max(float_vec)-1)),
               "'float_vec' contained element with value outside the allowed numeric range.",
               fixed = TRUE)
  expect_error(check_arg(arg = float_vec, in_range = c(min(float_vec)+1, max(float_vec)-1), arg_nam = "differentName"),
               "'differentName' contained element with value outside the allowed numeric range.",
               fixed = TRUE)

  # Integer
  expect_invisible(check_arg(arg = int_vec, in_range = c(min(int_vec),max(int_vec))))
  expect_invisible(check_arg(arg = int_vec, in_range = c(min(int_vec)-1,max(int_vec)+1)))
  expect_error(check_arg(arg = int_vec, in_range = c("a","b")),
               "Cannot check numeric range when 'in_range' is not numeric.",
               fixed = TRUE)
  expect_error(check_arg(arg = int_vec, in_range = c(min(int_vec), max(int_vec)-1)),
               "'int_vec' contained element with value outside the allowed numeric range.",
               fixed = TRUE)
  expect_error(check_arg(arg = int_vec, in_range = c(min(int_vec)+1, max(int_vec))),
               "'int_vec' contained element with value outside the allowed numeric range.",
               fixed = TRUE)
  expect_error(check_arg(arg = int_vec, in_range = c(min(int_vec)+1, max(int_vec)-1)),
               "'int_vec' contained element with value outside the allowed numeric range.",
               fixed = TRUE)
  expect_error(check_arg(arg = int_vec, in_range = c(min(int_vec)+1, max(int_vec)-1), arg_nam = "differentName"),
               "'differentName' contained element with value outside the allowed numeric range.",
               fixed = TRUE)

  # Numeric factor
  num_fact_vec <- factor(c(10,20,30,40,50))
  expect_error(check_arg(arg = num_fact_vec, in_range = c(1,2)),
               "Cannot check numeric range when 'arg' is not numeric.",
               fixed = TRUE)
  expect_error(check_arg(arg = float_vec, in_range = factor(c(12,22))),
               "Cannot check numeric range when 'in_range' is not numeric.",
               fixed = TRUE)



  #### Naming ####

  # Not named
  expect_invisible(check_arg(arg = float_vec, check_not_named = TRUE))
  expect_invisible(check_arg(arg = float_vec, check_not_named = FALSE))
  expect_invisible(check_arg(arg = setNames(float_vec,as.character(float_vec)), check_not_named = FALSE))
  expect_error(check_arg(arg = setNames(float_vec,as.character(float_vec)), check_not_named = TRUE, arg_nam = "namedFloatVec"),
               "'namedFloatVec' must not contain named elements.",
               fixed = TRUE)
  expect_error(check_arg(arg = c(3,4,setNames(float_vec,as.character(float_vec))), check_not_named = TRUE, arg_nam = "namedFloatVec"),
               "'namedFloatVec' must not contain named elements.",
               fixed = TRUE)

  # All named
  expect_invisible(check_arg(arg = float_vec, check_all_named = FALSE))
  expect_invisible(check_arg(arg = setNames(float_vec,as.character(float_vec)), check_all_named = TRUE))
  expect_error(check_arg(arg = c(3,4,setNames(float_vec,as.character(float_vec))), check_all_named = TRUE, arg_nam = "namedFloatVec"),
               "'namedFloatVec' contained unnamed elements.",
               fixed = TRUE)

  # check_all_uniquely_named = FALSE,
  expect_invisible(check_arg(arg = float_vec, check_all_uniquely_named = FALSE))
  expect_invisible(check_arg(arg = setNames(float_vec,as.character(float_vec)), check_all_uniquely_named = TRUE))
  expect_error(check_arg(arg = c(3,4,setNames(float_vec,as.character(float_vec))), check_all_uniquely_named = TRUE, arg_nam = "namedFloatVec"),
               "'namedFloatVec' contained unnamed elements.",
               fixed = TRUE)
  expect_error(check_arg(arg = setNames(float_vec,c("a","a","c","d","e")), check_all_uniquely_named = TRUE, arg_nam = "namedFloatVec"),
               "'namedFloatVec' contained duplicate names.",
               fixed = TRUE)


})

# TODO test_that("check_arg() works outside functions on lists",{})

test_that("check_arg() works outside functions on data frames",{

  num_df <- data.frame("a" = c(1,2,3), "b" = c(4,5,6))
  fact_df <- data.frame("a" = factor(c(1,2,3)),
                        "b" = factor(c(4,5,6)))
  fact2_df <- data.frame("a" = factor(c(10,20,30)),
                        "b" = factor(c(40,50,60)))
  char_df <- data.frame("a" = c("a","b","c"),
                        "b" = c("d","e","f"), stringsAsFactors = FALSE)

  check_val_types_numeric <- function(x){
    is.numeric(unlist(x, recursive = TRUE, use.names = FALSE))}
  check_val_types_factor <- function(x){
    is.factor(unlist(x, recursive = TRUE, use.names = FALSE))}

  # Expected to work
  expect_invisible(check_arg(num_df, type_check_fn = is.data.frame))
  expect_invisible(check_arg(num_df, type_check_fn = is.list))
  expect_invisible(check_arg(num_df, type_check_fn = check_val_types_numeric))
  expect_invisible(check_arg(fact_df, type_check_fn = check_val_types_factor))
  expect_invisible(check_arg(num_df, has_length = 2)) # 2 columns
  expect_invisible(check_arg(num_df, not_length = 10))
  expect_invisible(check_arg(num_df, allowed_values = c(1,2,3,4,5,6)))
  expect_invisible(check_arg(fact_df, allowed_values = c(1,2,3,4,5,6)))
  expect_invisible(check_arg(fact2_df, allowed_values = c(10,20,30,40,50,60)))
  expect_invisible(check_arg(char_df, allowed_values = letters))
  expect_invisible(check_arg(num_df, in_range = c(1,6)))
  expect_invisible(check_arg(num_df, check_all_named = TRUE))
  expect_invisible(check_arg(num_df, check_all_uniquely_named = TRUE))

  # Type
  expect_error(check_arg(num_df, type_check_fn = is.numeric),
               "'num_df' did not have the right type, as checked with is.numeric().",
               fixed = TRUE)
  expect_error(check_arg(char_df, type_check_fn = check_val_types_numeric),
               "'char_df' did not have the right type, as checked with check_val_types_numeric().",
               fixed = TRUE)
  expect_error(check_arg(char_df, type_check_fn = check_val_types_factor),
               "'char_df' did not have the right type, as checked with check_val_types_factor().",
               fixed = TRUE)

  # length
  expect_error(check_arg(num_df, has_length = 10),
               "num_df' had length 2 but must have length 10.",
               fixed = TRUE)
  expect_error(check_arg(num_df, not_length = 2),
               "num_df' cannot have length 2.",
               fixed = TRUE)

  # allowed values
  expect_error(check_arg(num_df, allowed_values = c(10,20,30)),
               "num_df' contained 6 unique disallowed values: 1, 2, 3, ...",
               fixed = TRUE)
  expect_error(check_arg(num_df, allowed_values = c(1,2,4)),
               "num_df' contained 3 unique disallowed values: 3, 5, 6.",
               fixed = TRUE)
  expect_error(check_arg(num_df, allowed_values = c(1,2,4)),
               "num_df' contained 3 unique disallowed values: 3, 5, 6.",
               fixed = TRUE)
  expect_error(check_arg(char_df, allowed_values = c(1,2,4)),
               "char_df' contained 6 unique disallowed values: a, b, c, ...",
               fixed = TRUE)
  expect_error(check_arg(fact2_df, allowed_values = c(10,20,40)),
               "'fact2_df' contained 3 unique disallowed values: 30, 50, 60.",
               fixed = TRUE)

  # in range
  expect_error(check_arg(num_df, in_range = c(3,5)),
               "num_df' contained element with value outside the allowed numeric range.",
               fixed = TRUE)
  expect_error(check_arg(num_df, in_range = c(0,5)),
               "num_df' contained element with value outside the allowed numeric range.",
               fixed = TRUE)
  expect_error(check_arg(num_df, in_range = c(3,7)),
               "num_df' contained element with value outside the allowed numeric range.",
               fixed = TRUE)
  expect_error(check_arg(fact2_df, in_range = c(3,7)),
               "Cannot check numeric range when 'arg' is not numeric.",
               fixed = TRUE)
  expect_error(check_arg(char_df, in_range = c(3,7)),
               "Cannot check numeric range when 'arg' is not numeric.",
               fixed = TRUE)

  # names
  expect_error(check_arg(num_df, check_not_named = TRUE),
               "num_df' must not contain named elements.",
               fixed = TRUE)
  colnames(num_df) <- c("a","a")
  expect_error(check_arg(num_df, check_all_uniquely_named = TRUE),
               "num_df' contained duplicate names.",
               fixed = TRUE)

})
test_that("check_arg() works outside functions on tibbles",{

  num_df <- tibble::tibble("a" = c(1,2,3), "b" = c(4,5,6))
  fact_df <- tibble::tibble("a" = factor(c(1,2,3)),
                        "b" = factor(c(4,5,6)))
  fact2_df <- tibble::tibble("a" = factor(c(10,20,30)),
                         "b" = factor(c(40,50,60)))
  char_df <- tibble::tibble("a" = c("a","b","c"),
                        "b" = c("d","e","f"))
  nested_df <- tibble::tibble("a" = c("a","b","c"),
                              "b" = c("d","e","f"),
                              "n" = list(tibble::tibble("n1" = c(1,2,3), "n2" = c(3,4,5)),
                                         tibble::tibble("n1" = c("a","b","c"), "n2" = c("d","e","f")),
                                         tibble::tibble("n1" = factor(c(1,2,3)), "n2" = factor(c(3,4,5)))))

  check_val_types_numeric <- function(x){
    is.numeric(unlist(x, recursive = TRUE, use.names = FALSE))}
  check_val_types_character <- function(x){
    is.character(unlist(x, recursive = TRUE, use.names = FALSE))}
  check_val_types_factor <- function(x){
    is.factor(unlist(x, recursive = TRUE, use.names = FALSE))}

  # Expected to work
  expect_invisible(check_arg(num_df, type_check_fn = is.data.frame))
  expect_invisible(check_arg(nested_df, type_check_fn = is.data.frame))
  expect_invisible(check_arg(num_df, type_check_fn = is.list))
  expect_invisible(check_arg(num_df, type_check_fn = check_val_types_numeric))
  expect_invisible(check_arg(fact_df, type_check_fn = check_val_types_factor))
  expect_invisible(check_arg(nested_df[c(2),], type_check_fn = check_val_types_character))
  expect_invisible(check_arg(num_df, has_length = 2)) # 2 columns
  expect_invisible(check_arg(num_df, not_length = 10))
  expect_invisible(check_arg(num_df, allowed_values = c(1,2,3,4,5,6)))
  expect_invisible(check_arg(fact_df, allowed_values = c(1,2,3,4,5,6)))
  expect_invisible(check_arg(fact2_df, allowed_values = c(10,20,30,40,50,60)))
  expect_invisible(check_arg(char_df, allowed_values = letters))
  expect_invisible(check_arg(num_df, in_range = c(1,6)))
  expect_invisible(check_arg(num_df, check_all_named = TRUE))
  expect_invisible(check_arg(num_df, check_all_uniquely_named = TRUE))

  # Type
  expect_error(check_arg(num_df, type_check_fn = is.numeric),
               "'num_df' did not have the right type, as checked with is.numeric().",
               fixed = TRUE)
  expect_error(check_arg(char_df, type_check_fn = check_val_types_numeric),
               "'char_df' did not have the right type, as checked with check_val_types_numeric().",
               fixed = TRUE)
  expect_error(check_arg(char_df, type_check_fn = check_val_types_factor),
               "'char_df' did not have the right type, as checked with check_val_types_factor().",
               fixed = TRUE)
  expect_invisible(check_arg(nested_df, type_check_fn = check_val_types_character)) # Forces all to character when unlisting

  # length
  expect_error(check_arg(num_df, has_length = 10),
               "num_df' had length 2 but must have length 10.",
               fixed = TRUE)
  expect_error(check_arg(num_df, not_length = 2),
               "num_df' cannot have length 2.",
               fixed = TRUE)

  # allowed values
  expect_error(check_arg(num_df, allowed_values = c(10,20,30)),
               "num_df' contained 6 unique disallowed values: 1, 2, 3, ...",
               fixed = TRUE)
  expect_error(check_arg(num_df, allowed_values = c(1,2,4)),
               "num_df' contained 3 unique disallowed values: 3, 5, 6.",
               fixed = TRUE)
  expect_error(check_arg(num_df, allowed_values = c(1,2,4)),
               "num_df' contained 3 unique disallowed values: 3, 5, 6.",
               fixed = TRUE)
  expect_error(check_arg(char_df, allowed_values = c(1,2,4)),
               "char_df' contained 6 unique disallowed values: a, b, c, ...",
               fixed = TRUE)
  expect_error(check_arg(fact2_df, allowed_values = c(10,20,40)),
               "'fact2_df' contained 3 unique disallowed values: 30, 50, 60.",
               fixed = TRUE)

  # in range
  expect_error(check_arg(num_df, in_range = c(3,5)),
               "num_df' contained element with value outside the allowed numeric range.",
               fixed = TRUE)
  expect_error(check_arg(num_df, in_range = c(0,5)),
               "num_df' contained element with value outside the allowed numeric range.",
               fixed = TRUE)
  expect_error(check_arg(num_df, in_range = c(3,7)),
               "num_df' contained element with value outside the allowed numeric range.",
               fixed = TRUE)
  expect_error(check_arg(fact2_df, in_range = c(3,7)),
               "Cannot check numeric range when 'arg' is not numeric.",
               fixed = TRUE)
  expect_error(check_arg(char_df, in_range = c(3,7)),
               "Cannot check numeric range when 'arg' is not numeric.",
               fixed = TRUE)

  # names
  expect_error(check_arg(num_df, check_not_named = TRUE),
               "num_df' must not contain named elements.",
               fixed = TRUE)
  colnames(num_df) <- c("a","a")
  expect_error(check_arg(num_df, check_all_uniquely_named = TRUE),
               "num_df' contained duplicate names.",
               fixed = TRUE)

})
test_that("check_arg() works outside functions on data tables",{

  num_df <- data.table::data.table("a" = c(1,2,3), "b" = c(4,5,6))
  fact_df <- data.table::data.table("a" = factor(c(1,2,3)),
                            "b" = factor(c(4,5,6)))
  fact2_df <- data.table::data.table("a" = factor(c(10,20,30)),
                             "b" = factor(c(40,50,60)))
  char_df <- data.table::data.table("a" = c("a","b","c"),
                            "b" = c("d","e","f"))

  check_val_types_numeric <- function(x){
    is.numeric(unlist(x, recursive = TRUE, use.names = FALSE))}
  check_val_types_factor <- function(x){
    is.factor(unlist(x, recursive = TRUE, use.names = FALSE))}

  # Expected to work
  expect_invisible(check_arg(num_df, type_check_fn = is.data.frame))
  expect_invisible(check_arg(num_df, type_check_fn = is.list))
  expect_invisible(check_arg(num_df, type_check_fn = check_val_types_numeric))
  expect_invisible(check_arg(fact_df, type_check_fn = check_val_types_factor))
  expect_invisible(check_arg(num_df, has_length = 2)) # 2 columns
  expect_invisible(check_arg(num_df, not_length = 10))
  expect_invisible(check_arg(num_df, allowed_values = c(1,2,3,4,5,6)))
  expect_invisible(check_arg(fact_df, allowed_values = c(1,2,3,4,5,6)))
  expect_invisible(check_arg(fact2_df, allowed_values = c(10,20,30,40,50,60)))
  expect_invisible(check_arg(char_df, allowed_values = letters))
  expect_invisible(check_arg(num_df, in_range = c(1,6)))
  expect_invisible(check_arg(num_df, check_all_named = TRUE))
  expect_invisible(check_arg(num_df, check_all_uniquely_named = TRUE))

  # Type
  expect_error(check_arg(num_df, type_check_fn = is.numeric),
               "'num_df' did not have the right type, as checked with is.numeric().",
               fixed = TRUE)
  expect_error(check_arg(char_df, type_check_fn = check_val_types_numeric),
               "'char_df' did not have the right type, as checked with check_val_types_numeric().",
               fixed = TRUE)
  expect_error(check_arg(char_df, type_check_fn = check_val_types_factor),
               "'char_df' did not have the right type, as checked with check_val_types_factor().",
               fixed = TRUE)

  # length
  expect_error(check_arg(num_df, has_length = 10),
               "num_df' had length 2 but must have length 10.",
               fixed = TRUE)
  expect_error(check_arg(num_df, not_length = 2),
               "num_df' cannot have length 2.",
               fixed = TRUE)

  # allowed values
  expect_error(check_arg(num_df, allowed_values = c(10,20,30)),
               "num_df' contained 6 unique disallowed values: 1, 2, 3, ...",
               fixed = TRUE)
  expect_error(check_arg(num_df, allowed_values = c(1,2,4)),
               "num_df' contained 3 unique disallowed values: 3, 5, 6.",
               fixed = TRUE)
  expect_error(check_arg(num_df, allowed_values = c(1,2,4)),
               "num_df' contained 3 unique disallowed values: 3, 5, 6.",
               fixed = TRUE)
  expect_error(check_arg(char_df, allowed_values = c(1,2,4)),
               "char_df' contained 6 unique disallowed values: a, b, c, ...",
               fixed = TRUE)
  expect_error(check_arg(fact2_df, allowed_values = c(10,20,40)),
               "'fact2_df' contained 3 unique disallowed values: 30, 50, 60.",
               fixed = TRUE)

  # in range
  expect_error(check_arg(num_df, in_range = c(3,5)),
               "num_df' contained element with value outside the allowed numeric range.",
               fixed = TRUE)
  expect_error(check_arg(num_df, in_range = c(0,5)),
               "num_df' contained element with value outside the allowed numeric range.",
               fixed = TRUE)
  expect_error(check_arg(num_df, in_range = c(3,7)),
               "num_df' contained element with value outside the allowed numeric range.",
               fixed = TRUE)
  expect_error(check_arg(fact2_df, in_range = c(3,7)),
               "Cannot check numeric range when 'arg' is not numeric.",
               fixed = TRUE)
  expect_error(check_arg(char_df, in_range = c(3,7)),
               "Cannot check numeric range when 'arg' is not numeric.",
               fixed = TRUE)

  # names
  expect_error(check_arg(num_df, check_not_named = TRUE),
               "num_df' must not contain named elements.",
               fixed = TRUE)
  colnames(num_df) <- c("a","a")
  expect_error(check_arg(num_df, check_all_uniquely_named = TRUE),
               "num_df' contained duplicate names.",
               fixed = TRUE)

})

# TODO test_that("check_arg() works outside functions on functions",{})


#### Inside functions ####

# TODO test_that("check_arg() works within functions on scalars",{})

# TODO test_that("check_arg() works within functions on vectors",{})

# TODO test_that("check_arg() works within functions on lists",{})

# TODO test_that("check_arg() works within functions on data frames",{})

# TODO test_that("check_arg() works within functions on functions",{})
