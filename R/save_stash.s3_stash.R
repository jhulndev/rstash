#' Save an object to a AWS S3 stash
#'
#' @inheritParams save_stash
#'
#' @return List of stash objects that were successfully saved.
#' @export
save_stash_.s3_stash <- function(x, file.name, to, time.stamp, uuid, extension,
    compression, checksum, save.fn, ...) {

  if (class(to) != 's3_stash') {
    stop('"to" directory needs to be an "s3_stash"')
  }

  bucket <- attr(to, 'bucket')

  message('Saving temp file locally...')
  ## Need to save the object to a temp file in order to load into s3
  temp.stash <- tempfile()
  temp.file <- suppressMessages(
    save_stash(x, file.name, to = temp.stash, time.stamp, uuid, extension,
        compression, checksum, save.fn, ...)[[1]])
  temp.file <- normalizePath(temp.file)

  ## Send file to s3
  message('Pushing file to S3...')
  file.name <- generate_filename(file.name, time.stamp, uuid, extension,
      compression)
  save.path <- file.path.s3(to, file.name)
  put.response <- put_object_wrapper(temp.file, save.path, bucket)

  ## Clean up the temp folder that was created.
  unlink(temp.stash, recursive = TRUE)

  save.path <- s3_stash(bucket, save.path)
  validate_files(save.path, 'Successfully saved to S3: ',
      'Failed to save to S3: ')

}
