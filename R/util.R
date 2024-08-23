`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


array_safe <- function(x, dim, name = deparse(substitute(x))) {
  if (length(x) != prod(dim)) {
    stop(sprintf("Unexpected length %d for '%s', expected %d",
                 length(x), name, prod(dim)))
  }
  array(x, dim)
}
