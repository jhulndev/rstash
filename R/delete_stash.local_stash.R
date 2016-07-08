#' Delete a file/object from a local stash
#'
#' @inheritParams delete_stash
#'
#' @return List of stash objects that were successfully deleted.
#' @export
delete_stash_.local_stash <- function(file.name, from, time.stamp, uuid,
    extension, compression, checksum, no.prompt, clean.up) {

  if (!is.character(from)) {
    stop('"from" directory is invalid')
  }

  from <- gsub('^$', '\\.', from)
  file.pattern <- generate_filepattern(file.name, time.stamp, uuid, extension,
      compression)
  file.matches <- list.files(from, pattern = file.pattern, full.names = TRUE)

  if (length(file.matches) == 0) {
    message('No files found to delete.')
    return(NA)
  }

  if (!isTRUE(no.prompt)) {
    message('Ready to delete the following files from "', from, '":\n  - ',
        paste(basename(file.matches), collapse = '\n  - '))
    confirmation <- readline(prompt = 'Are you sure you want to delete?: (y/n) ')
    if (!tolower(confirmation) %in% c('yes', 'y')) {
      stop('Deletion has been cancelled.')
    }
  }

  delete.results <- unlist(lapply(file.matches, delete_local_file))

  if (isTRUE(clean.up)) {
    clean_empty_dir(from)
  }

  return(delete.results)
}

delete_local_file <- function(x) {
  message('Deleting... ', x)
  unlink(x, recursive = TRUE)
  validate_files(local_stash(x), 'Failed to delete: ',
      'Successfully deleted: ')
}

