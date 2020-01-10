check_arg_type <- function(arg, type_check_fn, type_check_fn_name, type_name, current_frame, name) {
  # Check type
  if (!is.null(type_check_fn) &&
      !type_check_fn(arg)) {
    if (is.null(type_name)) {
      type_check_message <- ifelse(
        !is.null(type_check_fn_name),
        paste0(", as checked with ", type_check_fn_name, "()"),
        ""
      )
      stop(paste0(name,
                  " did not have the right type",
                  type_check_message, "."))

    } else {
      if (!is.character(type_name) || length(type_name) != 1) {
        stop("'type_name' must be a character vector of length 1.")
      }
      stop(paste0(name, " must be of type ", type_name, "."))
    }
  }
}

check_arg_values_allowed <- function(arg, allowed_vals, name) {
  # Check if any values are disallowed
  if (!is.null(allowed_vals)) {
    included_vals <- unlist(arg, recursive = TRUE, use.names = FALSE)
    disallowed_vals <- setdiff(included_vals, allowed_vals)
    num_disallowed_vals <- length(disallowed_vals)
    if (num_disallowed_vals > 0) {
      stop(paste0( name, " contained ",
                   num_disallowed_vals,
                   " unique disallowed values: ",
                   paste0(head(disallowed_vals, 3), collapse = ", "),
                   ifelse(num_disallowed_vals > 3, ", ...", ".")
      ))
    }
  }
}

check_arg_in_range <- function(arg, in_range, name) {
  # Check that the values of arg is in numeric range
  if (!is.null(in_range)) {
    if (!is.numeric(arg)) {
      stop("Cannot check numeric range when 'arg' is not numeric.")
    }
    arg_range <- range(arg)
    if (arg_range[[1]] < in_range[[1]] ||
        arg_range[[2]] > in_range[[2]]) {
      stop(paste0(
        name,
        " contained element outside the allowed numeric range."
      ))
    }
  }
}

check_arg_not_null <- function(arg, name){
  # Error if arg is NULL
  if (is.null(arg)){
    stop(paste0(
      name, " was NULL."
    ))
  }
}

check_arg_is_length <- function(arg, is_length, name){
  if (!is.null(is_length) && length(arg) != is_length){
    stop(paste0(name, " had length ", length(arg),
                " but must be of length ", is_length, "."))
  }
}

check_arg_not_length <- function(arg, not_length, name){
  if (!is.null(not_length) && length(arg) == not_length){
    stop(paste0(name, " cannot have length ", not_length, "."))
  }
}

check_arg_not_named <- function(arg, check_not_named, name) {
  if (isTRUE(check_not_named) && !is.null(names(arg))) {
    stop(paste0(name, " must only have unnamed elements."))
  }
}

check_arg_all_named <- function(arg, check_all_named, name) {
  # Check that all elements have names
  if (isTRUE(check_all_named) && length(names(arg)) != length(arg)) {
    stop(paste0(name, " contained unnamed elements."))
  }
}

check_arg_all_uniquely_named <- function(arg,
                                         check_all_uniquely_named,
                                         name) {
  # Check that all elements have a unique name
  if (isTRUE(check_all_uniquely_named) &&
      length(unique(names(arg))) != length(arg)) {
    stop(paste0(name, " contained unnamed elements."))
  }
}

