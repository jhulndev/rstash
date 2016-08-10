#' @export
get_type <- function(x, simplify = TRUE, missing = NULL) {
  x <- as.flat_list(x)
  res <- llply(x, get_type_)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  } else if (simplify && length(res) > 1) {
    res[laply(res, is.null)] <- missing
    unlist(res)
  }
  return(res)
}

get_type_ <- function(x) {
  UseMethod('get_type_')
}

#' @export
get_type_.local_stash <- function(x) {
  attr(x, 'type')
}

#' @export
get_type_.s3_stash <- function(x) {
  attr(x, 'type')
}

#' @export
get_type_.ftp_stash <- function(x) {
  attr(x, 'type')
}
