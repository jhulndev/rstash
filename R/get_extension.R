#' @export
get_extension <- function(x, simplify = TRUE, missing = NULL) {
  x <- as.flat_list(x)
  res <- llply(x, get_extension_)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  } else if (simplify && length(res) > 1) {
    res[laply(res, is.null)] <- missing
    unlist(res)
  }
  return(res)
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
