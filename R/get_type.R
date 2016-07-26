#' @export
get_type <- function(x) {
  x <- as.flat_list(x)
  unlist(llply(x, get_type_))
}

get_type_ <- function(x) {
  UseMethod('get_type_')
}

#' @export
get_type_.local_stash <- function(x) {
  attr(x, 'type')
}

#' @export
get_type_.s3_stash <- function(x) {
  attr(x, 'type')
}

#' @export
get_type_.ftp_stash <- function(x) {
  attr(x, 'type')
}
