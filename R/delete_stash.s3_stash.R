#' Delete a file/object from an AWS S3 stash
#'
#' @inheritParams delete_stash
#'
#' @return List of stash objects that were successfully deleted.
#' @export
delete_stash_.s3_stash <- function(file.name, from, time.stamp, uuid, extension,
    compression, checksum, no.prompt, clean.up) {

  if (class(from) != 's3_stash') {
    stop('"from" directory needs to be an "s3_stash"')
  }

  bucket <- attr(from, 'bucket')

  file.pattern <- generate_filepattern(file.name, time.stamp, uuid, extension,
      compression)
  file.matches <- list.files.s3(bucket, from, pattern = file.pattern,
      full.names = TRUE)

  if (length(file.matches) == 0) {
    message('No files found to delete.')
    return(NA)
  }

  if (!isTRUE(no.prompt)) {
    message('Ready to delete the following files from "', from, '" in ',
        'S3 bucket "', bucket, '":\n  - ',
        paste(basename(file.matches), collapse = '\n  - '))
    confirmation <- readline(prompt = 'Are you sure you want to delete?: (y/n) ')
    if (!tolower(confirmation) %in% c('yes', 'y')) {
      stop('Deletion has been cancelled.')
    }
  }

  delete.results <- unlist(lapply(file.matches, delete_s3_object,
      bucket = bucket))

  return(delete.results)
}

delete_s3_object <- function(x, bucket) {
  message('Deleting... ', x)
  delete_object(x, bucket)
  validate_files(s3_stash(bucket, x), 'Failed to delete: ',
      'Successfully deleted: ')
}
