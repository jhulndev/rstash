#' @export
get_timestamp <- function(x) {
  x <- as.flat_list(x)
  unlist(llply(x, get_timestamp_))
}

get_timestamp_ <- function(x) {
  UseMethod('get_timestamp_')
}

#' @export
get_timestamp_.local_stash <- function(x) {
  attr(x, 'time.stamp')
}

#' @export
get_timestamp_.s3_stash <- function(x) {
  attr(x, 'time.stamp')
}

#' @export
get_timestamp_.ftp_stash <- function(x) {
  attr(x, 'time.stamp')
}
