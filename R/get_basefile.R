#' @export
get_basefile <- function(x) {
  x <- as.flat_list(x)
  unlist(llply(x, get_basefile_))
}

get_basefile_ <- function(x) {
  UseMethod('get_basefile_')
}

#' @export
get_basefile_.local_stash <- function(x) {
  attr(x, 'base.file')
}

#' @export
get_basefile_.s3_stash <- function(x) {
  attr(x, 'base.file')
}

#' @export
get_basefile_.ftp_stash <- function(x) {
  attr(x, 'base.file')
}
