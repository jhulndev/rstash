copy_stash_.local_stash.local_stash <- function(file.name, from, to, time.stamp,
    uuid, extension, compression, checksum, keep.from, clean.up) {

  if (!is.character(from)) {
    stop('"from" directory is invalid')
  }
  if (!is.character(to)) {
    stop('"to" directory is invalid')
  }

  from <- validate_directory(from, create = FALSE, recursive = FALSE)
  file.pattern <- generate_filepattern(file.name, time.stamp, uuid, extension,
      compression)
  from.files <- list.files(from, pattern = file.pattern, full.names = TRUE)
  file.names <- basename(from.files)

  if (length(from.files) == 0) {
    message('No files found to copy')
    return(NA)
  }

  to <- validate_directory(to, create = TRUE, recursive = TRUE)
  to.files <- file.path(to, file.names)

  if (isTRUE(keep.from)) {
    results <- file.copy(from.files, to.files)
  } else {
    results <- file.rename(from.files, to.files)
    if (isTRUE(clean.up)) {
      clean_empty_dir(from)
    }
  }

  to.files <- lapply(to.files, local_stash)
  validate_files(to.files, 'Successfully copied: ',
      'Failed to copy: ')
}



copy_stash_.local_stash.s3_stash <- function(file.name, from, to, time.stamp,
    uuid, extension, compression, checksum, keep.from, clean.up) {

  if (!is.character(from)) {
    stop('"from" directory is invalid')
  }
  if (class(to) != 's3_stash') {
    stop('"to" directory needs to be an "s3_stash"')
  }

  bucket <- attr(to, 'bucket')

  from <- validate_directory(from, create = FALSE, recursive = FALSE)
  file.pattern <- generate_filepattern(file.name, time.stamp, uuid, extension,
      compression)
  from.files <- list.files(from, pattern = file.pattern, full.names = TRUE)
  file.names <- basename(from.files)

  if (length(from.files) == 0) {
    message('No files found to copy')
    return(NA)
  }

  to.files <- sapply(file.names, function(x) file.path.s3(to, x))

  results <- mapply(put_object_wrapper, file = from.files, object = to.files,
      bucket = bucket)

  if (!isTRUE(keep.from)) {
    unlink(from.files)
    if (isTRUE(clean.up)) {
      clean_empty_dir(from)
    }
  }

  to.files <- lapply(to.files, s3_stash, bucket = bucket)
  validate_files(to.files, 'Successfully copied: ',
      'Failed to copy: ')
}