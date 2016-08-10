#' @export
get_filepath <- function(x, simplify = TRUE, missing = NULL) {
  x <- as.flat_list(x)
  res <- llply(x, get_filepath_)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  } else if (simplify && length(res) > 1) {
    res[laply(res, is.null)] <- missing
    unlist(res)
  }
  return(res)
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
