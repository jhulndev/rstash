#' Save an object to a local stash
#'
#' @inheritParams save_stash
#'
#' @return List of stash objects that were successfully saved.
#' @export
save_stash_.local_stash <- function(x, file.name, to, time.stamp, uuid,
    extension, compression, checksum, save.fn, ...) {

  if (!is.character(to)) {
    stop('"to" directory is invalid')
  }

  to <- validate_directory(to, create = TRUE, recursive = TRUE)
  file.name <- generate_filename(file.name, time.stamp, uuid, extension,
      compression)
  save.path <- file.path(to, file.name)

  message('Saving locally...')

  if (!is.null(compression)) {
    save.con <- gzfile(save.path, 'w')
  } else {
    save.con <- file(save.path, 'w')
  }

  save.args <- list(quote(x), quote(save.con), ...)
  do.call(save.fn, save.args)
  close(save.con)

  save.path <- local_stash(save.path)
  validate_files(save.path, 'Successfully saved: ',
      'Failed to save: ')

}

