#' @export
as.stash <- function(x, ..., simplify = TRUE) {
  x <- as.flat_list(x)
  res <- llply(x, as.stash_, ... = ...)
  res <- as.flat_list(res)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  }
  return(res)
}

#' @export
as.stash_ <- function(x, ...) {
  UseMethod('as.stash_')
}

#' @export
as.stash_.character <- function(x, ...) {
  args <- list(...)
  args$x <- x
  do.call(as.local_stash, args)
}

#' @export
as.stash_.local_stash <- function(x, ...) {
  new.args <- list(...)
  new.args$x <- x
  do.call(as.local_stash, new.args)
}

#' @export
as.stash_.ftp_stash <- function(x, ...) {
  new.args <- list(...)
  new.args$x <- x
  do.call(as.ftp_stash, new.args)
}

#' @export
as.stash_.s3_stash <- function(x, ...) {
  new.args <- list(...)
  new.args$x <- x
  do.call(as.s3_stash, new.args)
}
