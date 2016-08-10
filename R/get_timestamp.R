#' @export
get_timestamp <- function(x, simplify = TRUE, missing = NULL) {
  x <- as.flat_list(x)
  res <- llply(x, get_timestamp_)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  } else if (simplify && length(res) > 1) {
    res[laply(res, is.null)] <- missing
    unlist(res)
  }
  return(res)
}

get_timestamp_ <- function(x) {
  UseMethod('get_timestamp_')
}

#' @export
get_timestamp_.local_stash <- function(x) {
  attr(x, 'time.stamp')
}

#' @export
get_timestamp_.s3_stash <- function(x) {
  attr(x, 'time.stamp')
}

#' @export
get_timestamp_.ftp_stash <- function(x) {
  attr(x, 'time.stamp')
}
