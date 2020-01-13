
## --- string utils --- ##

# Collapse list of strings once
collapse_strings <- function(strings){
  if (length(strings)>1)
    strings <- paste0(strings, collapse = "")
  strings
}

# Add extra \ before the
# special metacharacters: \n \r \t \v \f
escape_metacharacters <- function(string){
  # TODO must be a way to do it in one gsub with groups?
  string <- gsub("\n", "\\n", string, fixed = TRUE)
  string <- gsub("\r", "\\r", string, fixed = TRUE)
  string <- gsub("\t", "\\t", string, fixed = TRUE)
  string <- gsub("\v", "\\v", string, fixed = TRUE)
  string <- gsub("\f", "\\f", string, fixed = TRUE)
  string
}

# Wrap text every n characters.
# Rude but useful for long error messages.
split_string_every <- function(string, per = 60){
  # https://stackoverflow.com/a/26497700/11832955
  n <- seq(1, nc <- nchar(string), by = per)
  substring(string, n, c(n[-1]-1, nc))
}

# Split long string into elements in paste0
split_to_paste0 <- function(string, per = 60, tolerance = 10, spaces = 2){

  if (nchar(string) > per + tolerance)
    splits <- split_string_every(paste0(string))
  else {
    return(paste0("\"", string, "\""))
  }

  spaces_string <- create_space_string(n = spaces + 7)
  paste0(
    "paste0(\"",
    paste0(splits, collapse = paste0("\",\n", spaces_string, "\"")), "\")"
  )
}

# Create string with n spaces
create_space_string <- function(n = 2){
  paste0(rep(" ", n), collapse = "")
}
