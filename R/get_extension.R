#' @export
get_extension <- function(x) {
  x <- as.flat_list(x)
  unlist(llply(x, get_extension_))
}

get_extension_ <- function(x) {
  UseMethod('get_extension_')
}

#' @export
get_extension_.local_stash <- function(x) {
  attr(x, 'extension')
}

#' @export
get_extension_.s3_stash <- function(x) {
  attr(x, 'extension')
}

#' @export
get_extension_.ftp_stash <- function(x) {
  attr(x, 'extension')
}
