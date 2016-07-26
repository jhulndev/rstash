kFtpStashArgs <- c('path', 'time.stamp', 'uuid', 'extension', 'compression',
    'base.file', 'is.file', 'server', 'user', 'password', '.file.name',
    '.directory')

#' Object for representing a ftp file or directory
#'
#' For extension and compression, default to 'auto'. Check if available compression
#' is applied and extract it. If you do not want any compression to be identified
#' then set to NULL If the compression extension is named differently then was
#' rstash would create, pass a named vector, the name being the rstash designation
#' and the value being the compression extension of the existing file. For
#' example compression = c(gz = 'gzip') or c(gz = NA) if the compression is gzip
#' but there is no extension on the file.
#'
#' If this is not a file then nothing is processed, for 'auto', the
#'
#' @param path A file path to a local directory or file.
#' @param compression Default to auto, if the file ends it '.gz' it will identify gzip.
#'
#' @return A ftp_stash object.
#' @export
ftp_stash <- function(server, path = '', time.stamp = 'auto', uuid = 'auto',
    extension = 'auto', compression = 'auto', base.file = NULL, is.file = FALSE,
    user = NULL, password = NULL, ..., simplify = TRUE) {

  dots <- list(...)
  path <- as.flat_list(path)

  res <- llply(path, ftp_stash_, server = server, time.stamp = time.stamp,
      uuid = uuid, extension = extension, compression = compression,
      base.file = base.file, is.file = is.file, user = user, password = password,
      .file.name = dots$.file.name, .directory = dots$.directory)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  }
  return(res)
}


ftp_stash_ <- function(server, path, time.stamp, uuid, extension, compression,
    base.file, is.file, user, password, .file.name, .directory) {

  original.args <- list(path = path, time.stamp = time.stamp, uuid = uuid,
      extension = extension, compression = compression, base.file = base.file,
      is.file = is.file, server = server, user = user, password = password)

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
  if (!is.null(user) && !is.null(password)) {
    auth <- TRUE
  }

  server <- gsub('^ftp:/+|/$', '', server)
  dir.path <- split_path(dir.path)

  temp.str <- structure('', class = 'ftp_stash', server = server,
    directory = dir.path, file = file.name)

  structure(get_displaypath(temp.str), class = 'ftp_stash', args = original.args,
    type = type, directory = dir.path, file = file.name, base.file = base.file,
    time.stamp = proc.filename$time.stamp, uuid = proc.filename$uuid,
    extension = proc.filename$extension, compression = proc.filename$compression,
    server = server, auth = auth, user = user, password = password)

}


#' @export
print.ftp_stash <- function(x) {

  cat('Stash:\t\t', 'ftp', attr(x, 'type'), '\n')
  cat('Authentication:\t', attr(x, 'auth'), '\n')
  cat('Server:\t\t', attr(x, 'server'), '\n')
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
