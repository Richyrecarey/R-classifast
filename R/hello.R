#' Add together two numbers.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export

hello <- function(x) {
  y <- hello1(x)
  y
}

#' Diferent function
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.

# We will not export this one, it wont be loaded with the package
# But we can use it with computations I think
hello1 <- function(x) {
y <- x^2
y
}

