#' @export
get_directory <- function(x, simplify = TRUE, missing = NULL) {
  x <- as.flat_list(x)
  res <- llply(x, get_directory_)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  } else if (simplify && length(res) > 1) {
    res[laply(res, is.null)] <- missing
    unlist(res)
  }
  return(res)
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
