kS3StashArgs <- c('path', 'time.stamp', 'uuid', 'extension', 'compression',
    'base.file', 'is.file', 'bucket', 'access.key.id', 'secret.access.key',
    'region', 'url.style', '.file.name', '.directory')

#' Object for representing an AWS S3 file or directory
#'
#' @param bucket AWS S3 bucket
#' @param path A "file path" to a AWS S3 directory or file. S3 doesn't
#'    technically have folders/directories, but the object keys can be
#'    interpreted that way.
#' @param url.style 'virtual-hosted' or 'path'. see:
#'    https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingBucket.html
#'    Not really important for rstash operations, but may be useful for other APIs
#'
#' @return A s3_stash object.
#' @export
s3_stash <- function(bucket, path = '', time.stamp = 'auto', uuid = 'auto',
    extension = 'auto', compression = 'auto', base.file = NULL, is.file = FALSE,
    access.key.id = NULL, secret.access.key = NULL, region = NULL,
    url.style = 'virtual-hosted', ..., simplify = TRUE) {

  dots <- list(...)
  path <- as.flat_list(path)

  res <- llply(path, s3_stash_, bucket = bucket, time.stamp = time.stamp, uuid = uuid,
      extension = extension, compression = compression, base.file = base.file,
      is.file = is.file, access.key.id = access.key.id,
      secret.access.key = secret.access.key, region = region,
      url.style = url.style, .file.name = dots$.file.name,
      .directory = dots$.directory)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  }
  return(res)
}


s3_stash_ <- function(bucket, path, time.stamp, uuid, extension, compression,
    base.file, is.file, access.key.id, secret.access.key, region, url.style,
    .file.name, .directory) {

  original.args <- list(path = path, time.stamp = time.stamp, uuid = uuid,
      extension = extension, compression = compression, base.file = base.file,
      is.file = is.file, bucket = bucket, access.key.id = access.key.id,
      secret.access.key = secret.access.key, region = region,
      url.style = url.style)

  type <- gen_stash_type(base.file = base.file, is.file = is.file,
      .file.name = .file.name)

  dir.path <- gen_stash_dirpath(base.file = base.file, is.file = is.file,
      path = path, .directory = .directory)

  file.name <- gen_stash_filename(base.file = base.file, is.file = is.file,
      path = path, .file.name = .file.name)

  proc.filename <- process_stash_filename(file.name, time.stamp, uuid,
      extension, compression)

  if (is.null(base.file)) {
    base.file <- proc.filename$file.name
  }

  auth <- FALSE
  if (!is.null(access.key.id) && !is.null(secret.access.key)) {
    auth <- TRUE
  }

  dir.path <- split_path(dir.path)

  temp.str <- structure('', class = 's3_stash', bucket = bucket,
    directory = dir.path, file = file.name, region = region,
    url.style = url.style)

  structure(get_displaypath(temp.str), class = 's3_stash', args = original.args,
    type = type, directory = dir.path, file = file.name, base.file = base.file,
    time.stamp = proc.filename$time.stamp, uuid = proc.filename$uuid,
    extension = proc.filename$extension, compression = proc.filename$compression,
    bucket = bucket, auth = auth, access.key.id = access.key.id,
    secret.access.key = secret.access.key, region = region,
    url.style = url.style)

}


#' @export
print.s3_stash <- function(x) {

  cat('Stash:\t\t', 'aws s3', attr(x, 'type'), '\n')
  cat('Authentication:\t', attr(x, 'auth'), '\n')
  cat('Bucket:\t\t', attr(x, 'bucket'), '\n')
  cat('Region:\t\t', attr(x, 'region'), '\n')
  cat('URL Style:\t', attr(x, 'url.style'), '\n')
  cat('Directory:\t', paste0(attr(x, 'directory'), collapse = '/'), '\n')

  if (!is.null(attr(x, 'file'))) {
    cat('File:\t\t', attr(x, 'file'), '\n')
  }

  if (!is.null(attr(x, 'base.file'))) {
    cat('Base File:\t', attr(x, 'base.file'), '\n')
  }

  cat('Timestamp:\t', attr(x, 'time.stamp'), '\n')
  cat('UUID:\t\t', attr(x, 'uuid'), '\n')
  cat('Extension:\t', attr(x, 'extension') %||% 'NULL', '\n')
  cat('Compression:\t', attr(x, 'compression') %||% 'NULL', '\n')

  any.message <- !is.null(attr(x, 'critical')) ||
      !is.null(attr(x, 'warning')) || !is.null(attr(x, 'message'))

  if (any.message) {
    cat('-----------------------------------------------------------\n')
  }
  if (!is.null(attr(x, 'critical'))) {
    cat('CRITICAL:\t', attr(x, 'critical'), '\n')
  }
  if (!is.null(attr(x, 'warning'))) {
    cat('WARNING:\t', attr(x, 'warning'), '\n')
  }
  if (!is.null(attr(x, 'message'))) {
    cat('MESSAGE:\t', attr(x, 'message'), '\n')
  }
  if (any.message) {
    cat('-----------------------------------------------------------\n')
  }
  cat('\n')

  print(x[1])
  invisible(x)

}







