#' List files/objects that match parameters from an AWS S3 stash
#'
#' @inheritParams stash_match
#'
#' @return List of stash objects that match parameters.
#' @export
stash_match_.s3_stash <- function(file.name, from, time.stamp, uuid,
    extension, compression, recursive) {

  if (class(from) != 's3_stash') {
    stop('"from" directory needs to be an "s3_stash"')
  }

  bucket <- attr(from, 'bucket')

  file.pattern <- generate_filepattern(file.name, time.stamp, uuid, extension,
      compression)
  file.matches <- list.files.s3(bucket, from, pattern = file.pattern,
      full.names = TRUE, recursive = recursive)

  if (length(file.matches) == 0) {
    return(file.matches)
  }

  lapply(file.matches, s3_stash)
}
