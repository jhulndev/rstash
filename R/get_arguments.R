#' @export
get_arguments <- function(x, simplify = TRUE) {
  x <- as.flat_list(x)
  res <- llply(x, get_arguments_)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  }
  return(res)
}

get_arguments_ <- function(x) {
  UseMethod('get_arguments_')
}

#' @export
get_arguments_.local_stash <- function(x) {
  attr(x, 'args')
}

#' @export
get_arguments_.s3_stash <- function(x) {
  attr(x, 'args')
}

#' @export
get_arguments_.ftp_stash <- function(x) {
  attr(x, 'args')
}
