
## base wrappers

# Col should be col name
position_first <- function(data, col) {
  if (is.numeric(col)) stop("col must be name")
  subset(data, select = c(col, setdiff(names(data), col)))
}

position_last <- function(data, col) {
  if (is.numeric(col)) stop("col must be name")
  subset(data, select = c(setdiff(names(data), col), col))
}

rename_col <- function(data, before, after,
                       warn_at_overwrite = FALSE) {

  #
  # Replaces name of column in data frame
  #

  # Check names
  if (!is.character(before) || !is.character(after)) {
    stop("'before' and 'after' must both be of type character.")
  }
  if (length(before) != 1 || length(before) != 1) {
    stop("'before' and 'after' must both have length 1.")
  }

  if (before == after) {
    message("'before' and 'after' were identical.")
    return(data)
  }
  # If after is already a column in data
  # remove it, so we don't have duplicate column names
  if (after %in% colnames(data)) {
    if (isTRUE(warn_at_overwrite)) {
      warning("'after' already existed in 'data' and will be replaced.")
    }
    data[[after]] <- NULL
  }
  colnames(data)[names(data) == before] <- after
  return(data)
}
