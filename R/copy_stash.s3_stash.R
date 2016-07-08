copy_stash_.s3_stash.local_stash <- function(file.name, from, to, time.stamp,
    uuid, extension, compression, checksum, keep.from, clean.up) {

  if (class(from) != 's3_stash') {
    stop('"from" directory needs to be an "s3_stash"')
  }
  if (!is.character(to)) {
    stop('"to" directory is invalid')
  }

  bucket <- attr(from, 'bucket')

  file.pattern <- generate_filepattern(file.name, time.stamp, uuid, extension,
      compression)
  from.files <- list.files.s3(bucket, from, pattern = file.pattern,
      full.names = TRUE, recursive = FALSE)
  file.names <- basename(from.files)

  if (length(from.files) == 0) {
    message('No files found to copy')
    return(NA)
  }

  to <- validate_directory(to, create = TRUE, recursive = TRUE)
  to.files <- file.path(to, file.names)

  results <- mapply(save_object, object = from.files, file = to.files,
      bucket = bucket, parse_response = FALSE)

  if (!isTRUE(keep.from)) {
    mapply(delete_object, object = from.files, bucket = bucket)
  }

  to.files <- lapply(to.files, local_stash)
  validate_files(to.files, 'Successfully copied: ',
      'Failed to copy: ')
}



copy_stash_.s3_stash.s3_stash <- function(file.name, from, to, time.stamp,
    uuid, extension, compression, checksum, keep.from, clean.up) {

  if (class(from) != 's3_stash') {
    stop('"from" directory needs to be an "s3_stash"')
  }
  if (class(to) != 's3_stash') {
    stop('"to" directory needs to be an "s3_stash"')
  }

  from.bucket <- attr(from, 'bucket')
  to.bucket <- attr(to, 'bucket')

  if (identical(from.bucket, to.bucket)) {
    message('Moving temp file locally...')
    ## Need to save the object to a temp file in order to load into s3
    temp.stash <- local_stash(tempfile())
    temp.file <- suppressMessages(
      copy_stash(file.name, from = from, to = temp.stash, time.stamp, uuid,
          extension, compression, checksum, keep.from = keep.from))

    if (any(is.na(temp.file))) {
      message('No files to copy...')
      return(NA)
    }

    results <- copy_stash(file.name, from = temp.stash, to = to, time.stamp,
        uuid, extension, compression, checksum, keep.from = FALSE,
        clean.up = TRUE)

    return(results)
  }

  file.pattern <- generate_filepattern(file.name, time.stamp, uuid, extension,
      compression)
  from.files <- list.files.s3(bucket, from, pattern = file.pattern,
      full.names = TRUE)
  file.names <- basename(from.files)

  to.files <- sapply(file.names, function(x) file.path.s3(to, x))

  results <- mapply(copy_object, from_object = from.files, to_object = to.files,
      from_bucket = from.bucket, to_bucket = to.bucket)

  if (!isTRUE(keep.from)) {
    mapply(delete_object, object = from.files, bucket = from.bucket)
  }

  to.files <- lapply(to.files, s3_stash, bucket = to.bucket)
  validate_files(to.files, 'Successfully copied: ',
      'Failed to copy: ')
}






