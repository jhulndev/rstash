#' Get/Load a stashed object from a AWS S3 stash
#'
#' @inheritParams get_stash
#'
#' @return List of loaded stash objects.
#' @export
get_stash_.s3_stash <- function(file.name, from, time.stamp, uuid, extension,
    compression, checksum, read.fn, ...) {

  if (class(from) != 's3_stash') {
    stop('"from" directory needs to be an "s3_stash"')
  }

  bucket <- attr(from, 'bucket')

  file.pattern <- generate_filepattern(file.name, time.stamp, uuid, extension,
      compression)
  file.matches <- list.files.s3(bucket, from, pattern = file.pattern,
      full.names = TRUE)

  if (length(file.matches) == 0) {
    message('No files found')
    return(NA)
  }

  ## Need to pull the files locally
  message('Moving temp file locally...')
  ## Need to save the object to a temp file in order to load into s3
  temp.stash <- local_stash(tempfile())
  temp.file <- suppressMessages(
    copy_stash(file.name, from = from, to = temp.stash, time.stamp, uuid,
        extension, compression, checksum, keep.from = TRUE))

  message('Reading from temp file...')
  results <- suppressMessages(
    lapply(temp.file, get_stash_read_local, read.fn = read.fn, ... = ...))

  ## Remove the temp files
  unlink(temp.stash, recursive = TRUE)

  return(results)
}
