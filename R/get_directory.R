#' @export
get_directory <- function(x) {
  x <- as.flat_list(x)
  unlist(llply(x, get_directory_))
}

get_directory_ <- function(x) {
  UseMethod('get_directory_')
}

#' @export
get_directory_.local_stash <- function(x) {
  paste0(attr(x, 'directory'), collapse = .Platform$file.sep)
}

#' @export
get_directory_.ftp_stash <- function(x) {
  paste0(attr(x, 'directory'), collapse = '/')
}

#' @export
get_directory_.s3_stash <- function(x) {
  paste0(attr(x, 'directory'), collapse = '/')
}
