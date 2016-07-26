#' @export
get_container <- function(x) {
  x <- as.flat_list(x)
  unlist(llply(x, get_container_))
}

get_container_ <- function(x) {
  UseMethod('get_container_')
}

#' @export
get_container_.s3_stash <- function(x) {
  attr(x, 'bucket')
}
