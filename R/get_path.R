#' @export
get_path <- function(x, simplify = TRUE, missing = NULL) {
  x <- as.flat_list(x)
  res <- llply(x, get_path_)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  } else if (simplify && length(res) > 1) {
    res[laply(res, is.null)] <- missing
    unlist(res)
  }
  return(res)
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
