code.tree <- function(expr) {
  lapply(expr, function(part) {
    if(is.atomic(part) || is.symbol(part)) part
    else if(is.language(part)) code.tree(part)
    else part
  })
}
