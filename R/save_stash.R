#' Save an object
#'
#' @param x Object to save
#' @param file.name Base file name for the saved object
#' @param to Path for where the object will be saved. Accepts a character
#'    string or *_stash object.
#' @param time.stamp TRUE or FALSE. When TRUE, a time stamp will be appended to
#'    the \code{file.name}. It is recommended to use time.stamp OR uuid, not
#'    both. Using both can cause the file name to become very long.
#' @param uuid TRUE or FALSE. When TRUE, a uuid will be appended to
#'    the \code{file.name}. It is recommended to use time.stamp OR uuid, not
#'    both. Using both can cause the file name to become very long.
#' @param extension Extension to append to the file.
#' @param compression Accepts NULL for no compression, or 'gz' for gzip.
#' @param checksum Not being used right now.
#' @param save.fn Any function where the first argument is the object and the
#'    second is the file path.
#' @param ... Arguments to pass to \code{save.fn}.
#'
#' @return List of stash objects that were successfully saved.
#' @export
save_stash <- function(x, file.name, to = '', time.stamp = FALSE, uuid = FALSE,
    extension = NULL, compression = NULL, checksum = FALSE, save.fn = write.csv,
    ...) {

  to <- as.stash(to)
  save_stash_(x, file.name, to, time.stamp, uuid, extension, compression,
      checksum, save.fn, ...)
}


save_stash_ <- function(x, file.name, to, time.stamp, uuid, extension,
    compression, checksum, save.fn, ...) {

  UseMethod('save_stash_', to)
}