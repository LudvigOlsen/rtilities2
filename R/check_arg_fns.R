check_arg_type <- function(arg, type_check_fn, type_check_fn_name, type_name, current_frame, arg_name, message_fn) {
  # Check type
  if (!is.null(type_check_fn) &&
      !type_check_fn(arg)) {
    if (is.null(type_name)) {
      type_check_message <- ifelse(
        !is.null(type_check_fn_name),
        paste0(", as checked with ", type_check_fn_name, "()"),
        ""
      )
      message_fn(paste0(arg_name,
                  " did not have the right type",
                  type_check_message, "."))

    } else {
      if (!is.character(type_name) || length(type_name) != 1) {
        message_fn("'type_name' must be a character vector of length 1.")
      }
      message_fn(paste0(arg_name, " must be of type ", type_name, "."))
    }
  }
}

check_arg_values_allowed <- function(arg, allowed_values,
                                     arg_name, message_fn,
                                     first_n = 3) {
  # Check if any values are disallowed
  if (!is.null(allowed_values)) {
    included_vals <- unlist(arg, recursive = TRUE, use.names = FALSE)
    disallowed_values <- setdiff(included_vals, allowed_values)
    num_disallowed_values <- length(disallowed_values)
    if (num_disallowed_values > 0) {
      message_fn(paste0( arg_name, " contained ",
                   num_disallowed_values,
                   " unique disallowed values: ",
                   paste0(head(disallowed_values, first_n), collapse = ", "),
                   ifelse(num_disallowed_values > first_n, ", ...", ".")
      ))
    }
  }
}

check_arg_in_range <- function(arg, in_range, arg_name, message_fn) {
  # Check that the values of arg is in numeric range
  if (!is.null(in_range)) {
    in_range <- sort(in_range)
    included_vals <- unlist(arg, recursive = TRUE, use.names = FALSE)
    if (!is.numeric(included_vals)) {
      message_fn("Cannot check numeric range when 'arg' is not numeric.")
    }
    if (!is.numeric(in_range)) {
      message_fn("Cannot check numeric range when 'in_range' is not numeric.")
    }
    arg_range <- range(included_vals)
    if (arg_range[[1]] < in_range[[1]] ||
        arg_range[[2]] > in_range[[2]]) {
      message_fn(paste0(
        arg_name,
        " contained element with value outside the allowed numeric range."
      ))
    }
  }
}

check_arg_not_null <- function(arg, arg_name, message_fn){
  # Error if arg is NULL
  if (is.null(arg)){
    message_fn(paste0(
      arg_name, " was NULL."
    ))
  }
}

check_arg_has_length <- function(arg, has_length, arg_name, message_fn){
  if (!is.null(has_length) && length(arg) != has_length){
    message_fn(paste0(arg_name, " had length ", length(arg),
                " but must have length ", has_length, "."))
  }
}

check_arg_not_length <- function(arg, not_length, arg_name, message_fn){
  if (!is.null(not_length) && length(arg) == not_length){
    message_fn(paste0(arg_name, " cannot have length ", not_length, "."))
  }
}

check_arg_not_named <- function(arg, check_not_named, arg_name, message_fn) {
  if (isTRUE(check_not_named) && !is.null(names(arg))) {
    message_fn(paste0(arg_name, " must not contain named elements."))
  }
}

non_empty_names <- function(x){
  names(x)[names(x) != ""]
}

check_arg_all_named <- function(arg, check_all_named, arg_name, message_fn) {
  # Check that all elements have names
  if (isTRUE(check_all_named) && length(non_empty_names(arg)) != length(arg)) {
    message_fn(paste0(arg_name, " contained unnamed elements."))
  }
}

check_arg_all_uniquely_named <- function(arg,
                                         check_all_uniquely_named,
                                         arg_name,
                                         message_fn) {
  # Check that all are named
  arg_all_named <- check_arg_all_named(
    arg = arg,
    check_all_named = check_all_uniquely_named,
    arg_name = arg_name,
    message_fn = message_fn)
  if(identical(message_fn, return) && !is.null(arg_all_named)) return(arg_all_named)

  # Check that all elements have a unique name
  if (isTRUE(check_all_uniquely_named) &&
      length(unique(non_empty_names(arg))) != length(arg)) {
    message_fn(paste0(arg_name, " contained duplicate names."))
  }
}

check_arg_allowed_names <- function(arg,
                                    allowed_names,
                                    arg_name,
                                    message_fn,
                                    first_n = 3) {

  # Check that all names are allowed
  if (!is.null(allowed_names)){
    disallowed_names <- setdiff(non_empty_names(arg), allowed_names)
    num_disallowed_names <- length(disallowed_names)
    if (num_disallowed_names > 0){
      message_fn(paste0(
        arg_name, " contained ",
        num_disallowed_names,
        " unique disallowed names: ",
        paste0(head(disallowed_names, first_n), collapse = ", "),
        ifelse(num_disallowed_names > first_n, ", ...", ".")
      ))
    }
  }

}

check_arg_required_names <- function(arg,
                                     required_names,
                                     arg_name,
                                     message_fn,
                                     first_n = 3) {

  # Check that all required names are in arg
  if (!is.null(required_names)){
    missing_names <- setdiff(required_names, non_empty_names(arg))
    num_missing_names <- length(missing_names)
    if (num_missing_names > 0){
      message_fn(paste0(
        arg_name, " lacked ",
        num_missing_names,
        " required names: ",
        paste0(head(missing_names, first_n), collapse = ", "),
        ifelse(num_missing_names > first_n, ", ...", ".")
      ))
    }
  }

}
