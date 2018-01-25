#' binomial coefficent
#' @export
#' @description a function that calculates the binomial coefficent
#' @seealso \url{https://en.wikipedia.org/wiki/Binomial_coefficient}
#' @details
#' the binomial coefficent is defined as \deqn{n!/(k! * (n-k)!)} where
#' ! denotes the factorial operator. n and k must be positive integers satisfying
#' \eqn{n >= k}.
#' @param n integer; must be larger or equal to k.
#' @param k integer; must be larger or equal to 0.
binomial_coefficient <- function(n, k) {

  # test input
  if (!(is.numeric(n) && is.numeric(k) && isTRUE(all.equal(n, as.integer(n))) &&
      isTRUE(all.equal(k, as.integer(k))))) {
    stop("n and k must be integers")
  }

  if(any(k < 0)) {
    stop("k must be larger or equal to 0")
  }

  if (any(n < k)) {
    stop("n must be larger or equal to k")
  }

  return(factorial(n)/(factorial(k) * factorial(n - k)))

}
