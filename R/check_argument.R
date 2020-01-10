check_arg <- function(arg,
                      type_check_fn = NULL,
                      is_length = NULL,
                      not_length = NULL,
                      allowed_vals = NULL,
                      in_range = NULL,
                      check_not_named = FALSE,
                      check_all_named = FALSE,
                      check_all_uniquely_named = FALSE,
                      allow_null = TRUE,
                      name = NULL,
                      type_name = NULL){

  # Get current frame for extracting argument names
  current_frame <- sys.frame(which = sys.nframe())

  # Get name from parent environment
  if (is.null(name)){
    name <- tryCatch( # Doesn't really work to do this in subfunction
      deparse(substitute(expr = arg, env = current_frame)),
      error = function(e) {
        return(NULL)
      },
      warning = function(w) {
        return(NULL)
      }
    )
    if (is.null(name)){
      stop("Could not extract name from 'arg'. Please specify it as a character string.")
    }
  }
  if (!is.character(name) || length(name) != 1){
    stop("'name' must be a character string or NULL.")
  }

  # Add '' around name
  name <- paste0("'", name, "'")

  if (!(isTRUE(allow_null) && is.null(arg))){

    # Check if arg is NULL
    check_arg_not_null(arg = arg, name = name)

    # Check length of arg
    check_arg_is_length(arg = arg,
                        is_length = is_length,
                        name = name)
    check_arg_not_length(arg = arg,
                         not_length = not_length,
                         name = name)

    # Check type
    # TODO Perhaps we want to pass the name of the parent function to the error message?
    type_check_fn_name <- tryCatch({
      tcfn_name <- deparse(substitute(expr = type_check_fn, env = current_frame))
      if (nchar(tcfn_name)>9 &&
          substr(tcfn_name, 1, 9) == "function("){
        tcfn_name <- NULL
      }
      tcfn_name
      }, error = function(e) {
        return(NULL)
      }, warning = function(w) {
        return(NULL)
      }
    )

    check_arg_type(
      arg = arg,
      type_check_fn = type_check_fn,
      type_check_fn_name = type_check_fn_name,
      type_name = type_name,
      current_frame = current_frame,
      name = name
    )

    # Check if any values are disallowed
    check_arg_values_allowed(arg = arg,
                             allowed_vals = allowed_vals,
                             name = name)

    # Check that the values of arg is in numeric range
    check_arg_in_range(arg = arg,
                       in_range = in_range,
                       name = name)

    ## Names
    # TODO Check up on cases where colnames()
    # returns strings but not names()

    # Check that arg has no name
    check_arg_not_named(arg = arg,
                        check_not_named = check_not_named,
                        name = name)
    # Check that all elements have names
    check_arg_all_named(arg = arg,
                        check_all_named = check_all_named,
                        name = name)
    # Check that all elements have a unique name
    check_arg_all_uniquely_named(arg = arg,
                                 check_all_uniquely_named = check_all_uniquely_named,
                                 name = name)

  }



}

