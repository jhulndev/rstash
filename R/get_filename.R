#' @export
get_filename <- function(x, simplify = TRUE, missing = NULL) {
  x <- as.flat_list(x)
  res <- llply(x, get_filename_)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  } else if (simplify && length(res) > 1) {
    res[laply(res, is.null)] <- missing
    unlist(res)
  }
  return(res)
}

get_filename_ <- function(x) {
  UseMethod('get_filename_')
}

#' @export
get_filename_.local_stash <- function(x) {
  attr(x, 'file')
}

#' @export
get_filename_.s3_stash <- function(x) {
  attr(x, 'file')
}

#' @export
get_filename_.ftp_stash <- function(x) {
  attr(x, 'file')
}
