#' @export
get_compression <- function(x, simplify = TRUE, missing = NULL) {
  x <- as.flat_list(x)
  res <- llply(x, get_compression_)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  } else if (simplify && length(res) > 1) {
    res[laply(res, is.null)] <- missing
    unlist(res)
  }
  return(res)
}

get_compression_ <- function(x) {
  UseMethod('get_compression_')
}

#' @export
get_compression_.local_stash <- function(x) {
  attr(x, 'compression')
}

#' @export
get_compression_.s3_stash <- function(x) {
  attr(x, 'compression')
}

#' @export
get_compression_.ftp_stash <- function(x) {
  attr(x, 'compression')
}

