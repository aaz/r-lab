# Generate a logistic map function
lm.func <- function(r) {
  lm <- function(x) {
    r * x * (1 - x)
  }
  return(lm)
}

# Generate a series using the logistic map
lm.series <- function(n, r, x0) {

  f <- lm.func(r)

  lm.iterator <- function(count, x, v) {
    if (count == 0) {
      return(append(v, x))
    } else {
      lm.iterator(count - 1, f(x), append(v, x))
    }
  }
  lm.iterator(n, x0, vector())
}
