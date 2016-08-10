#' @export
get_filedata <- function(x, simplify = TRUE) {
  x <- as.flat_list(x)
  res <- llply(x, get_filedata_)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  }
  return(res)
}


get_filedata_ <- function(x) {
  UseMethod('get_filedata_')
}

#' @export
get_filedata_.local_stash <- function(x) {
  attrs <- attributes(x)
  c(attrs['base.file'], attrs['time.stamp'], attrs['uuid'], attrs['extension'],
      attrs['compression'])
}

#' @export
get_filedata_.s3_stash <- function(x) {
  attrs <- attributes(x)
  c(attrs['base.file'], attrs['time.stamp'], attrs['uuid'], attrs['extension'],
      attrs['compression'])
}

#' @export
get_filedata_.ftp_stash <- function(x) {
  attrs <- attributes(x)
  c(attrs['base.file'], attrs['time.stamp'], attrs['uuid'], attrs['extension'],
      attrs['compression'])
}
