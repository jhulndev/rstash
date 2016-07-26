#' @export
get_filename <- function(x) {
  x <- as.flat_list(x)
  unlist(llply(x, get_filename_))
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
