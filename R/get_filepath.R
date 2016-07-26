#' @export
get_filepath <- function(x) {
  x <- as.flat_list(x)
  unlist(llply(x, get_filepath_))
}

get_filepath_ <- function(x) {
  UseMethod('get_filepath_')
}

#' @export
get_filepath_.local_stash <- function(x) {
  paste0(c(attr(x, 'directory'), attr(x, 'file')), collapse = .Platform$file.sep)
}

#' @export
get_filepath_.s3_stash <- function(x) {
  paste0(c(attr(x, 'directory'), attr(x, 'file')), collapse = '/')
}

#' @export
get_filepath_.ftp_stash <- function(x) {
  paste0(c(attr(x, 'directory'), attr(x, 'file')), collapse = '/')
}
