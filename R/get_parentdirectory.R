#' @export
get_parentdirectory <- function(x, simplify = TRUE, missing = NULL) {
  x <- as.flat_list(x)
  res <- llply(x, get_parentdirectory_)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  } else if (simplify && length(res) > 1) {
    res[laply(res, is.null)] <- missing
    unlist(res)
  }
  return(res)
}

get_parentdirectory_ <- function(x) {
  UseMethod('get_parentdirectory_')
}

#' @export
get_parentdirectory_.local_stash <- function(x) {
  directory <- attr(x, 'directory')
  directory <- directory[1:length(directory) - 1]
  if (length(directory) == 0) {
    return('')
  }
  paste0(directory, collapse = .Platform$file.sep)
}

#' @export
get_parentdirectory_.ftp_stash <- function(x) {
  directory <- attr(x, 'directory')
  directory <- directory[1:length(directory) - 1]
  if (length(directory) == 0) {
    return('')
  }
  paste0(directory, collapse = '/')
}

#' @export
get_parentdirectory_.s3_stash <- function(x) {
  directory <- attr(x, 'directory')
  directory <- directory[1:length(directory) - 1]
  if (length(directory) == 0) {
    return('')
  }
  paste0(directory, collapse = '/')
}
