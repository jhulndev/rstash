#' List all files/objects in a stash
#'
#' @param from Path for where the object resides. Accepts a character
#'    string or *_stash object.
#' @param recursive Logical. Should the contents recurse into directories?
#'
#' @return List of stash objects residing in the \code{from} stash
#' @export
stash_content <- function(from = '', recursive) {

  from <- as.stash(from)
  stash_match_(file.name = '.*', from, time.stamp = FALSE, uuid = FALSE,
      extension = NULL, compression = NULL, recursive = recursive)
}
