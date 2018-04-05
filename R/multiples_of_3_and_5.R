multiples <- function(x) {
  if (!is.numeric(x)) stop("x needs to be numeric")
  tmp <- 1:(x - 1)
  sum(tmp[tmp %% 3 == 0 | tmp %% 5 == 0])
}
