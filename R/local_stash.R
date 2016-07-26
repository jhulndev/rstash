kLocalStashArgs <- c('path', 'time.stamp', 'uuid', 'extension', 'compression',
    'base.file', 'is.file', '.file.name', '.directory')

#' Object for representing a local file or directory
#'
#' @param path A file path to a local directory or file.
#'
#' @return A local_stash object.
#' @export
local_stash <- function(path = '', time.stamp = 'auto', uuid = 'auto',
    extension = 'auto', compression = 'auto', base.file = NULL,
    is.file = FALSE, ..., simplify = TRUE) {

  dots <- list(...)
  path <- as.flat_list(path)

  res <- llply(path, local_stash_, time.stamp = time.stamp, uuid = uuid,
      extension = extension, compression = compression, base.file = base.file,
      is.file = is.file, .file.name = dots$.file.name,
      .directory = dots$.directory)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  }
  return(res)
}


local_stash_ <- function(path, time.stamp, uuid, extension, compression,
    base.file, is.file, .file.name, .directory) {

  original.args <- list(path = path, time.stamp = time.stamp, uuid = uuid,
      extension = extension, compression = compression, base.file = base.file,
      is.file = is.file)

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

  dir.path <- split_path(dir.path)

  temp.str <- structure('', class = 'local_stash', directory = dir.path,
      file = file.name)

  structure(get_displaypath(temp.str), class = 'local_stash', args = original.args,
    type = type, directory = dir.path, file = file.name, base.file = base.file,
    time.stamp = proc.filename$time.stamp, uuid = proc.filename$uuid,
    extension = proc.filename$extension, compression = proc.filename$compression)

}

#' @export
print.local_stash <- function(x) {

  cat('Stash:\t\t', 'local', attr(x, 'type'), '\n')
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


## TODO:
## file.path.local_stash
## file.exists.local_stash