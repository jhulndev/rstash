#' @export
as.s3_stash <- function(x, ..., simplify = TRUE) {
  x <- as.flat_list(x)
  res <- llply(x, as.s3_stash_, ... = ...)
  res <- as.flat_list(res)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  }
  return(res)
}

#' @export
as.s3_stash_ <- function(x, ...) {
  UseMethod('as.s3_stash_')
}

#' @export
as.s3_stash_.character <- function(x, ...) {
  args <- list(...)
  args$path <- x
  do.call(s3_stash, args)
}

#' @export
as.s3_stash_.local_stash <- function(x, ...) {
  new.args <- update_arguments(get_arguments(x), list(...), kS3StashArgs)
  do.call(s3_stash, new.args)
}

#' @export
as.s3_stash_.s3_stash <- function(x, ...) {
  new.args <- update_arguments(get_arguments(x), list(...), kS3StashArgs)
  do.call(s3_stash, new.args)
}

#' @export
as.s3_stash_.ftp_stash <- function(x, ...) {
  new.args <- update_arguments(get_arguments(x), list(...), kS3StashArgs)
  do.call(s3_stash, new.args)
}
