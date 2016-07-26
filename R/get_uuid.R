#' @export
get_uuid <- function(x) {
  x <- as.flat_list(x)
  unlist(llply(x, get_uuid_))
}

get_uuid_ <- function(x) {
  UseMethod('get_uuid_')
}

#' @export
get_uuid_.local_stash <- function(x) {
  attr(x, 'uuid')
}

#' @export
get_uuid_.s3_stash <- function(x) {
  attr(x, 'uuid')
}

#' @export
get_uuid_.ftp_stash <- function(x) {
  attr(x, 'uuid')
}
