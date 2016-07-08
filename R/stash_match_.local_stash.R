#' List files/objects that match parameters from a local stash
#'
#' @inheritParams stash_match
#'
#' @return List of stash objects that match parameters.
#' @export
stash_match_.local_stash <- function(file.name, from, time.stamp, uuid,
    extension, compression) {

  if (!is.character(from)) {
    stop('"from" directory is invalid')
  }

  from <- gsub('^$', '\\.', from)
  file.pattern <- generate_filepattern(file.name, time.stamp, uuid, extension,
      compression)
  file.matches <- list.files(from, pattern = file.pattern, full.names = TRUE)

  if (length(file.matches) == 0) {
    return(file.matches)
  }

  lapply(file.matches, local_stash)
}
