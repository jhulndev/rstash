#' @export
get_attributes <- function(x, simplify = TRUE) {
  x <- as.flat_list(x)
  res <- llply(x, get_attributes_)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  }
  return(res)
}

get_attributes_ <- function(x) {
  UseMethod('get_attributes_')
}

#' @export
get_attributes_.local_stash <- function(x) {
  attr(x, 'args')
  attributes(x)[names(attributes(x)) != 'args']
}

#' @export
get_attributes_.s3_stash <- function(x) {
  attributes(x)[names(attributes(x)) != 'args']
}

#' @export
get_attributes_.ftp_stash <- function(x) {
  attributes(x)[names(attributes(x)) != 'args']
}
