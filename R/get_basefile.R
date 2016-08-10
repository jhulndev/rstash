#' @export
get_basefile <- function(x, simplify = TRUE, missing = NULL) {
  x <- as.flat_list(x)
  res <- llply(x, get_basefile_)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  } else if (simplify && length(res) > 1) {
    res[laply(res, is.null)] <- missing
    unlist(res)
  }
  return(res)
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
