#' @export
get_container <- function(x, simplify = TRUE, missing = NULL) {
  x <- as.flat_list(x)
  res <- llply(x, get_container_)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  } else if (simplify && length(res) > 1) {
    res[laply(res, is.null)] <- missing
    unlist(res)
  }
  return(res)
}

get_container_ <- function(x) {
  UseMethod('get_container_')
}

#' @export
get_container_.s3_stash <- function(x) {
  attr(x, 'bucket')
}
