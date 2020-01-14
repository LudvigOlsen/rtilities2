

#   __________________ #< 236b56c37017ff9827cc04096429dc23 ># __________________
#   Not in                                                                  ####


#' @title Negated value matching
#' @description Negated \code{\link[base:match]{\%in\%}}.
#' @param x Vector with values to be matched.
#' @param table Vector with values to be matched against.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @aliases not_in
#' @export
#' @details The negation of \code{\link[base:match]{\%in\%}}.
#'
#'  Implementated as: \code{!(x %in% table)}
#' @return A logical vector, indicating if no match
#'  was located for each element of \code{x}.
#'  Values are \code{TRUE} or \code{FALSE} and never \code{NA}.
`%ni%` <- function(x, table) {
  !(x %in% table)
}
