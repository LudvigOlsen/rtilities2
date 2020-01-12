
#' @title Simple do-if functions
#' @name do_if
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  If the \code{condition} is \code{TRUE}, do something with the supplied object.
#' @param condition The condition to check. (Logical)
#' @param message Message. (Character)
#'
#'  Note: If \code{NULL}, the \code{condition} will be used as message.
#' @param x Any object.
#' @param fn Any function.
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

add_condition_prefix <- function(m){
  paste0("This was TRUE: ", m)
}

#' @rdname do_if
#' @export
stop_if <- function(condition, message = NULL){

  # If message is NULL, get condition
  if (is.null(message)){
    message <- tryCatch( # Doesn't really work to do this in subfunction
      deparse(
        substitute(expr = condition,
                   env = sys.frame(which = sys.nframe()))),
      error = function(e) {
        stop("Cannot use 'condition' as message. Please provide a message.")
      },
      warning = function(w) {
        stop("Cannot use 'condition' as message. Please provide a message.")
      }
    )
    # Add "This was TRUE: "
    message <- add_condition_prefix(message)
  }

  if (condition){
    stop(simpleError(message, call = if (p <- sys.parent(1L)) sys.call(p)))
  }
  invisible(NULL)
}

#' @rdname do_if
#' @export
warn_if <- function(condition, message = NULL){

  # If message is NULL, get condition
  if (is.null(message)){
    message <- tryCatch( # Doesn't really work to do this in subfunction
      deparse(
        substitute(expr = condition,
                   env = sys.frame(which = sys.nframe()))),
      error = function(e) {
        stop("Cannot use 'condition' as message. Please provide a message.")
      },
      warning = function(w) {
        stop("Cannot use 'condition' as message. Please provide a message.")
      }
    )
    # Add "This was TRUE: "
    message <- add_condition_prefix(message)
  }

  if (condition){
    warning(simpleWarning(message, call = if (p <- sys.parent(1L)) sys.call(p)))
  }
  invisible(NULL)
}

#' @rdname do_if
#' @export
message_if <- function(condition, message = NULL){

  # If message is NULL, get condition
  if (is.null(message)){
    message <- tryCatch( # Doesn't really work to do this in subfunction
      deparse(
        substitute(expr = condition,
                   env = sys.frame(which = sys.nframe()))),
      error = function(e) {
        stop("Cannot use 'condition' as message. Please provide a message.")
      },
      warning = function(w) {
        stop("Cannot use 'condition' as message. Please provide a message.")
      }
    )
    # Add "This was TRUE: "
    message <- add_condition_prefix(message)
  }

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
  stop_if(!is.function(fn), "'fn' must be a function.")
  if (condition){
    return(fn(...))
  }
  invisible(NULL)
}


# extract_error_msg()
