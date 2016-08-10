#' @export
get_uuid <- function(x, simplify = TRUE, missing = NULL) {
  x <- as.flat_list(x)
  res <- llply(x, get_uuid_)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  } else if (simplify && length(res) > 1) {
    res[laply(res, is.null)] <- missing
    unlist(res)
  }
  return(res)
}

get_uuid_ <- function(x) {
  UseMethod('get_uuid_')
}

#' @export
get_uuid_.local_stash <- function(x) {
  attr(x, 'uuid')
}

#' @export
get_uuid_.s3_stash <- function(x) {
  attr(x, 'uuid')
}

#' @export
get_uuid_.ftp_stash <- function(x) {
  attr(x, 'uuid')
}
