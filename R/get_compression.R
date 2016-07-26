#' @export
get_compression <- function(x) {
  x <- as.flat_list(x)
  laply(x, get_compression_)
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

