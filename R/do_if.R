
#' @title Simple do-if functions
#' @name do_if
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  If the \code{condition} is \code{TRUE}, do something with the supplied object.
#' @param condition The condition to check. (Logical)
#' @param message Message. (Character)
#' @param x Any object.
#' @param fn Any function.
#'
#'  E.g. \code{function(){3+5}}
#' @param ... Arguments for \code{fn}.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @details
#' When \code{condition} is \code{FALSE}, they return \code{NULL} invisibly.
#'
#' When \code{condition} is \code{TRUE}:
#'
#' \subsection{stop_if()}{
#' Throw error with the supplied message.
#' }
#' \subsection{warn_if()}{
#' Throw warning with the supplied message.
#' }
#' \subsection{message_if()}{
#' Generate message with the supplied message.
#' }
#' \subsection{return_if()}{
#' Return \code{x}.
#' }
#' \subsection{do_if()}{
#' Call \code{fn} with the arguments in \code{...} and return the result.
#' }
#' @examples
#' # Attach packages
#' library(rtilities2)
#' \donttest{
#' a <- 0
#' stop_if(a == 0, "'a' cannot be 0.")
#' warn_if(a == 0, "'a' was 0.")
#' message_if(a == 0, "'a' was so kind to be 0.")
#' return_if(a == 0, a)
#' do_if(a == 0, function(x, y){x + y}, x = 2, y = 10)
#' }
NULL

# NOTE Aliases only work when building the package
# so use do_if to see docs

#' @rdname do_if
#' @export
stop_if <- function(condition, message){
  if (condition){
    stop(simpleError(message, call = if (p <- sys.parent(1L)) sys.call(p)))
  }
  invisible(NULL)
}

#' @rdname do_if
#' @export
warn_if <- function(condition, message){
  if (condition){
    warning(simpleWarning(message, call = if (p <- sys.parent(1L)) sys.call(p)))
  }
  invisible(NULL)
}

#' @rdname do_if
#' @export
message_if <- function(condition, message){
  if (condition){
    message(simpleMessage(message, call = if (p <- sys.parent(1L)) sys.call(p)))
  }
  invisible(NULL)
}

#' @rdname do_if
#' @export
return_if <- function(condition, x){
  if (condition){
    return(x)
  }
  invisible(NULL)
}

#' @rdname do_if
#' @export
do_if <- function(condition, fn, ...){
  if (condition){
    return(fn(...))
  }
  invisible(NULL)
}


# extract_error_msg()
