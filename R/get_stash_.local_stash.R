#' Get/Load a stashed object from a local stash
#'
#' @inheritParams get_stash
#'
#' @return List of loaded stash objects.
#' @export
get_stash_.local_stash <- function(file.name, from, time.stamp, uuid, extension,
    compression, checksum, read.fn, ...) {

  if (!is.character(from)) {
    stop('"from" directory is invalid')
  }

  from <- validate_directory(from, create = FALSE, recursive = FALSE)
  file.pattern <- generate_filepattern(file.name, time.stamp, uuid, extension,
      compression)
  file.matches <- list.files(from, pattern = file.pattern, full.names = TRUE)

  if (length(file.matches) == 0) {
    message('No files found')
    return(NA)
  }

  lapply(file.matches, get_stash_read_local, read.fn = read.fn, ... = ...)
}

get_stash_read_local <- function(file.path, read.fn, ...) {
  message('Reading... ', file.path)
  read.args <- list(quote(file.path), ...)
  do.call(read.fn, read.args)
}