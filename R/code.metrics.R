code.metrics.internal <- function(expr, level = 1L, envir = NULL) {
  structure(lapply(expr, function(part) {
    if(is.atomic(part) || is.symbol(part)) part
    else if(is.language(part)) {
      envir$max.level <- max(envir$max.level, level + 1L)
      code.metrics.internal(part, level + 1L, envir)
    }
    else part
  }),level = level)
}

code.metrics <- function(expr, level = 1L) {
  envir <- new.env()
  envir$max.level <- level
  tree <- code.metrics.internal(expr, level, envir)
  envir$max.level
}
