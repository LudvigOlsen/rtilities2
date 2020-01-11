library(rtilities2)
context("check_arg_str()")

test_that("check_arg_str() works outside functions on vectors",{

  #### Create data ####
  nll <- NULL
  char_vec <- c("a","b","c","d","e")
  float_vec <- (1:5)*1.05
  int_vec <- as.integer(1:5)
  fact_vec <- factor(char_vec)

  #### Type errors ####

  # NULL
  expect_invisible(check_arg_str(arg = NULL))
  expect_invisible(check_arg_str(arg = nll))
  expect_equal(check_arg(arg = NULL,allow_null = FALSE,message_fn = return),
               "'NULL' was NULL.")
  expect_equal(check_arg_str(arg = nll, allow_null = FALSE),
               "'nll' was NULL.")

  # Character
  expect_invisible(check_arg_str(arg = char_vec, type_check_fn = is.character))
  expect_equal(check_arg_str(arg = char_vec, type_check_fn = is.numeric),
               "'char_vec' did not have the right type, as checked with is.numeric().")
  expect_equal(check_arg_str(arg = char_vec, type_check_fn = function(x){is.numeric(x)}),
               "'char_vec' did not have the right type.",
               )
  expect_equal(check_arg_str(arg = char_vec, type_check_fn = is.numeric, type_name = "numeric"),
               "'char_vec' must be of type numeric.",
               )

  # Numeric
  expect_invisible(check_arg_str(arg = float_vec, type_check_fn = is.numeric))
  expect_equal(check_arg_str(arg = float_vec, type_check_fn = is.character),
               "'float_vec' did not have the right type, as checked with is.character().",
               )
  expect_equal(check_arg_str(arg = float_vec, type_check_fn = function(x){is.character(x)}),
               "'float_vec' did not have the right type.",
               )
  expect_equal(check_arg_str(arg = float_vec, type_check_fn = is.character, type_name = "character"),
               "'float_vec' must be of type character.",
               )
  expect_equal(check_arg_str(arg = float_vec, type_check_fn = is.integer, type_name = "integer"),
               "'float_vec' must be of type integer.",
               )

  # Integer
  expect_invisible(check_arg_str(arg = int_vec, type_check_fn = is.integer))
  expect_equal(check_arg_str(arg = int_vec, type_check_fn = is.character),
               "'int_vec' did not have the right type, as checked with is.character().",
               )
  expect_equal(check_arg_str(arg = int_vec, type_check_fn = function(x){is.character(x)}),
               "'int_vec' did not have the right type.",
               )
  expect_equal(check_arg_str(arg = int_vec, type_check_fn = is.character, type_name = "character"),
               "'int_vec' must be of type character.",
               )
  expect_equal(check_arg_str(arg = int_vec, type_check_fn = is.double),
               "'int_vec' did not have the right type, as checked with is.double().",
               )

  # Factor
  expect_invisible(check_arg_str(arg = fact_vec, type_check_fn = is.factor))
  expect_equal(check_arg_str(arg = fact_vec, type_check_fn = is.character),
               "'fact_vec' did not have the right type, as checked with is.character().",
               )
  expect_equal(check_arg_str(arg = fact_vec, type_check_fn = function(x){is.character(x)}),
               "'fact_vec' did not have the right type.",
               )
  expect_equal(check_arg_str(arg = fact_vec, type_check_fn = is.character, type_name = "character"),
               "'fact_vec' must be of type character.",
               )
  expect_equal(check_arg_str(arg = fact_vec, type_check_fn = is.double),
               "'fact_vec' did not have the right type, as checked with is.double().",
               )

  # Specify arg_nam
  expect_equal(check_arg_str(arg = char_vec, type_check_fn = is.numeric, type_name = "numeric", arg_nam = "differentName"),
               "'differentName' must be of type numeric.",
               )

  #### Length errors ####

  expect_invisible(check_arg_str(arg = char_vec, has_length = 5))
  expect_invisible(check_arg_str(arg = nll, has_length = 5))
  expect_equal(check_arg_str(arg = char_vec, has_length = 4),
               "'char_vec' had a length of 5 but must have a length of 4.",
               )
  expect_equal(check_arg_str(arg = char_vec, has_length = 4, arg_nam = "differentName"),
               "'differentName' had a length of 5 but must have a length of 4.",
               )

  expect_invisible(check_arg_str(arg = char_vec, not_length = 10))
  expect_invisible(check_arg_str(arg = nll, not_length = 5))
  expect_equal(check_arg_str(arg = char_vec, not_length = 5),
               "'char_vec' cannot have a length of 5.",
               )
  expect_equal(check_arg_str(arg = char_vec, not_length = 5, arg_nam = "differentName"),
               "'differentName' cannot have a length of 5.",
               )

  #### Allowed values ####

  # Characters
  expect_invisible(check_arg_str(arg = nll, allowed_values = c("a","b")))
  expect_invisible(check_arg_str(arg = char_vec, allowed_values = char_vec))
  expect_equal(check_arg_str(arg = char_vec, allowed_values = c("a","b")),
               "'char_vec' contained 3 unique disallowed values: c, d, e.",
               )
  expect_equal(check_arg_str(arg = char_vec, allowed_values = c("b")),
               "'char_vec' contained 4 unique disallowed values: a, c, d, ...",
               )
  expect_equal(check_arg_str(arg = char_vec, allowed_values = c("b"), arg_nam = "differentName"),
               "'differentName' contained 4 unique disallowed values: a, c, d, ...",
               )

  # Numeric
  expect_invisible(check_arg_str(arg = float_vec, allowed_values = float_vec))
  expect_equal(check_arg_str(arg = float_vec, allowed_values = c("a","b")),
               "'float_vec' contained 5 unique disallowed values: 1.05, 2.1, 3.15, ...",
               )
  expect_equal(check_arg_str(arg = float_vec, allowed_values = c(1.05, 4.20)),
               "'float_vec' contained 3 unique disallowed values: 2.1, 3.15, 5.25.",
               )

  # Integer
  expect_invisible(check_arg_str(arg = int_vec, allowed_values = int_vec))
  expect_equal(check_arg_str(arg = int_vec, allowed_values = c("a","b")),
               "'int_vec' contained 5 unique disallowed values: 1, 2, 3, ...",
               )
  expect_equal(check_arg_str(arg = int_vec, allowed_values = c(1.0, 2.0)),
               "'int_vec' contained 3 unique disallowed values: 3, 4, 5.",
               )

  # Factor
  # Important that values, not level indices, are checked
  expect_invisible(check_arg_str(arg = fact_vec, allowed_values = fact_vec))
  expect_invisible(check_arg_str(arg = fact_vec, allowed_values = c("a","b","c","d","e","f","g"))) # extra allowed vals
  expect_equal(check_arg_str(arg = fact_vec, allowed_values = c("a","b")),
               "'fact_vec' contained 3 unique disallowed values: c, d, e.",
               )
  expect_equal(check_arg_str(arg = fact_vec, allowed_values = c(1, 2)),
               "'fact_vec' contained 5 unique disallowed values: a, b, c, ...",
               )

  #### In range ####

  # Numeric
  expect_invisible(check_arg_str(arg = float_vec, in_range = c(min(float_vec),max(float_vec))))
  expect_invisible(check_arg_str(arg = float_vec, in_range = c(min(float_vec)-1,max(float_vec)+1)))
  expect_invisible(check_arg_str(arg = float_vec, in_range = c(max(float_vec), min(float_vec))))
  expect_equal(check_arg_str(arg = float_vec, in_range = c("a","b")),
               "Cannot check numeric range when 'in_range' is not numeric.",
               )
  expect_equal(check_arg_str(arg = char_vec, in_range = c(1,2)),
               "Cannot check numeric range when 'arg' is not numeric.",
               )
  expect_equal(check_arg_str(arg = float_vec, in_range = c(min(float_vec), max(float_vec)-1)),
               "'float_vec' contained element with value outside the allowed numeric range.",
               )
  expect_equal(check_arg_str(arg = float_vec, in_range = c(min(float_vec)+1, max(float_vec))),
               "'float_vec' contained element with value outside the allowed numeric range.",
               )
  expect_equal(check_arg_str(arg = float_vec, in_range = c(min(float_vec)+1, max(float_vec)-1)),
               "'float_vec' contained element with value outside the allowed numeric range.",
               )
  expect_equal(check_arg_str(arg = float_vec, in_range = c(min(float_vec)+1, max(float_vec)-1), arg_nam = "differentName"),
               "'differentName' contained element with value outside the allowed numeric range.",
               )

  # Integer
  expect_invisible(check_arg_str(arg = int_vec, in_range = c(min(int_vec),max(int_vec))))
  expect_invisible(check_arg_str(arg = int_vec, in_range = c(min(int_vec)-1,max(int_vec)+1)))
  expect_equal(check_arg_str(arg = int_vec, in_range = c("a","b")),
               "Cannot check numeric range when 'in_range' is not numeric.",
               )
  expect_equal(check_arg_str(arg = int_vec, in_range = c(min(int_vec), max(int_vec)-1)),
               "'int_vec' contained element with value outside the allowed numeric range.",
               )
  expect_equal(check_arg_str(arg = int_vec, in_range = c(min(int_vec)+1, max(int_vec))),
               "'int_vec' contained element with value outside the allowed numeric range.",
               )
  expect_equal(check_arg_str(arg = int_vec, in_range = c(min(int_vec)+1, max(int_vec)-1)),
               "'int_vec' contained element with value outside the allowed numeric range.",
               )
  expect_equal(check_arg_str(arg = int_vec, in_range = c(min(int_vec)+1, max(int_vec)-1), arg_nam = "differentName"),
               "'differentName' contained element with value outside the allowed numeric range.",
               )

  # Numeric factor
  num_fact_vec <- factor(c(10,20,30,40,50))
  expect_equal(check_arg_str(arg = num_fact_vec, in_range = c(1,2)),
               "Cannot check numeric range when 'arg' is not numeric.",
               )
  expect_equal(check_arg_str(arg = float_vec, in_range = factor(c(12,22))),
               "Cannot check numeric range when 'in_range' is not numeric.",
               )



  #### Naming ####

  # Not named
  expect_invisible(check_arg_str(arg = float_vec, check_not_named = TRUE))
  expect_invisible(check_arg_str(arg = float_vec, check_not_named = FALSE))
  expect_invisible(check_arg_str(arg = setNames(float_vec,as.character(float_vec)), check_not_named = FALSE))
  expect_equal(check_arg_str(arg = setNames(float_vec,as.character(float_vec)), check_not_named = TRUE, arg_nam = "namedFloatVec"),
               "'namedFloatVec' must not contain named elements.",
               )
  expect_equal(check_arg_str(arg = c(3,4,setNames(float_vec,as.character(float_vec))), check_not_named = TRUE, arg_nam = "namedFloatVec"),
               "'namedFloatVec' must not contain named elements.",
               )

  # All named
  expect_invisible(check_arg_str(arg = float_vec, check_all_named = FALSE))
  expect_invisible(check_arg_str(arg = setNames(float_vec,as.character(float_vec)), check_all_named = TRUE))
  expect_equal(check_arg_str(arg = c(3,4,setNames(float_vec,as.character(float_vec))), check_all_named = TRUE, arg_nam = "namedFloatVec"),
               "'namedFloatVec' contained unnamed elements.",
               )

  # check_all_uniquely_named = FALSE,
  expect_invisible(check_arg_str(arg = float_vec, check_all_uniquely_named = FALSE))
  expect_invisible(check_arg_str(arg = setNames(float_vec,as.character(float_vec)), check_all_uniquely_named = TRUE))
  expect_equal(check_arg_str(arg = c(3,4,setNames(float_vec,as.character(float_vec))), check_all_uniquely_named = TRUE, arg_nam = "namedFloatVec"),
               "'namedFloatVec' contained unnamed elements.",
               )
  expect_equal(check_arg_str(arg = setNames(float_vec,c("a","a","c","d","e")), check_all_uniquely_named = TRUE, arg_nam = "namedFloatVec"),
               "'namedFloatVec' contained duplicate names.",
               )

})

