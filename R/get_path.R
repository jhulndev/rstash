#' @export
get_path <- function(x) {
  x <- as.flat_list(x)
  unlist(llply(x, get_path_))
}

get_path_ <- function(x) {
  UseMethod('get_path_')
}

#' @export
get_path_.local_stash <- function(x) {
  attr(x, 'path')
}

#' @export
get_path_.s3_stash <- function(x) {
  attr(x, 'path')
}

#' @export
get_path_.ftp_stash <- function(x) {
  attr(x, 'path')
}
