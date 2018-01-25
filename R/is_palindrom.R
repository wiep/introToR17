#' checks if a string is a palindrom
#' @export
#' @param str string; length of the vector must be 1
#' @seealso \url{https://en.wikipedia.org/wiki/Palindrome}
is_palindrom <- function(str) {
  if (!is.character(str) || length(str) != 1) {
    stop("str must be a single character string")
  }
  n <- nchar(str)
  if (n <= 1) {
    return(TRUE)
  }

  for (i in seq(1, ceiling(n/2))) {
    if (substr(str, i, i) != substr(str, n-i+1, n-i+1)) {
      return(FALSE)
    }
  }
  return(TRUE)
}
