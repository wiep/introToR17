multiples <- function(x = 10) {
  tmp <- 1:(x - 1)
  sum(tmp[tmp %% 3 == 0 | tmp %% 5 == 0])
}
