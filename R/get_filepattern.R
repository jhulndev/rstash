#' @export
get_filepattern <- function(x, simplify = TRUE, missing = NULL) {
  x <- as.flat_list(x)
  res <- llply(x, get_filepattern_)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  } else if (simplify && length(res) > 1) {
    res[laply(res, is.null)] <- missing
    unlist(res)
  }
  return(res)
}

get_filepattern_ <- function(x) {
  if (attr(x, 'type') != 'file') {
    return(NULL)
  }

  gen_filename_regx(file.name = attr(x, 'base.file'),
      time.stamp = attr(x, 'time.stamp'), uuid = attr(x, 'uuid'),
      extension = attr(x, 'extension'), compression = attr(x, 'compression'))
}
