#' Gives the sum of all natural numbers below \code{x} that are multiples of 3 or 5.
#'
#' @param x A natural number.
#' @return The sum of all natural numbers below \code{x} that are multiples of 3 or 5.


multiples <- function(x) {
  if (!is.numeric(x)) stop("x needs to be numeric")
  tmp <- 1:(x - 1)
  sum(tmp[tmp %% 3 == 0 | tmp %% 5 == 0])
}
