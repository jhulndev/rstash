#' @export
validate_stash <- function(x, simplify = TRUE) {
  x <- as.flat_list(x)
  res <- llply(x, validate_stash_)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  }
  return(res)
}

#' @export
validate_stash_ <- function(x) {
  UseMethod('validate_stash_')
}

#' @export
validate_stash_.local_stash <- function(x) {
  if (file.exists(x)) {
    x <- set_messages(x, 'message', 'File exists')
  } else {
    x <- set_messages(x, 'warning', 'File does not exist')
  }
  return(x)
}

#' @export
validate_stash_.s3_stash <- function(x) {
  if (file.exists.s3(x, bucket = attr(x, 'bucket'))) {
    x <- set_messages(x, 'message', 'File exists')
  } else {
    x <- set_messages(x, 'warning', 'File does not exist')
  }
  return(x)
}