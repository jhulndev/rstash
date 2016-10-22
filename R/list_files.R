#' List the Files in a Stash or Directory/Folder
#'
#' When \code{x} is a \code{\link[base]{character}} vector or a
#' \code{\link{local_stash}}, then \code{list_files} behaves as an alias for
#' \code{\link[base]{list.files}} where \code{x} is passed to the \code{path}
#' argument of \code{\link[base]{list.files}} and the remaining matching
#' arguments are passed as is with the default values of \code{list_files} where
#' applicable. (If you do not define an argument, the default value for the
#' argument as defined in \code{list_files} will be used and not the default as
#' defined in \code{\link[base]{list.files}}).
#'
#' @param x A \code{character} vector or a \code{\link{stash}}.
#' @param pattern Optional \code{\link[base]{regex}} (regular expression).
#'    Only file names which match the regular expression will be returned.
#' @param all.files Logical. If \code{FALSE}, only the names of visible files
#'    are returned. If TRUE, all file names will be returned. \strong{Not
#'    applicable when \code{x} is a \code{\link{ftp_stash}} or
#'    \code{\link{s3_stash}}.}
#' @param full.names Logical. If \code{FALSE} and \code{recursive = FALSE}, only
#'    the file names are returned. If \code{FALSE} and \code{recursive = TRUE},
#'    sub-directory names will be prepended to the file names. If \code{TRUE}
#'    and \code{x} is a \code{character} vector or a
#'    \code{\link{local_stash}}, the full directory path is prepended to the
#'    file names to give a relative file path. If \code{TRUE} and \code{x} is a
#'    \code{\link{ftp_stash}} or \code{\link{s3_stash}}, then a fully resolvable
#'    URL will be returned. \strong{Not applicable when \code{as.stash = TRUE}.}
#' @param recursive Logical. Should the listing recurse into directories?
#' @param ignore.case Logical. Should pattern-matching be case-insensitive?
#' @param include.dirs Logical. If \code{TRUE}, then sub-directory
#'    names be included when \code{recursive = TRUE}, no effect when
#'    \code{recursive = FALSE} (sub-directory names will always be included).
#' @param no.. Logical. Should both "." and ".." be excluded from non-recursive
#'    listings? \strong{Not applicable when \code{x} is a \code{\link{ftp_stash}}
#'    or \code{\link{s3_stash}}.}
#' @param match.file Logical. If \code{TRUE} and \code{x} is a file stash, then
#'    the file name pattern will be extracted and used for \code{pattern}. Not
#'    applicable is \code{x} is not a stash object or is a directory.
#' @param as.stash Logical. If \code{TRUE}, a list of stash objects will be
#'    returned. If \code{FALSE}, a \code{character} vector is returned.
#'
#' @return If \code{as.stash = TRUE}, a list of \code{\link{stash}} objects
#'    will be returned, matching the stash type of \code{x}. If \code{x} is a
#'    \code{character} vector then a list of \code{\link{local_stash}} objects
#'    are returned. If \code{as.stash = FALSE}, a \code{character} vector is
#'    returned. If there are no matching files, the list or character vector
#'    will be of \code{length == 0}.
#' @export
list_files <- function(x = ".", pattern = NULL, all.files = FALSE,
    full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
    include.dirs = !recursive, no.. = FALSE, match.file = TRUE,
    as.stash = TRUE, simplify = TRUE) {

  x <- as.flat_list(x)
  res <- llply(x, list_files_, pattern = pattern, all.files = all.files,
      full.names = full.names, recursive = recursive,
      ignore.case = ignore.case, include.dirs = include.dirs, no.. = no..,
      match.file = match.file, as.stash = as.stash)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  }
  return(res)
}



list_files_ <- function(x, pattern, all.files, full.names, recursive,
    ignore.case, include.dirs, no.., match.file, as.stash) {

  UseMethod('list_files_')
}



#' @export
list_files_.character <- function(x, pattern, all.files, full.names, recursive,
    ignore.case, include.dirs, no.., match.file, as.stash) {

  if (as.stash) {
    full.names <- TRUE
  }

  files <- list.files(path = x, pattern = pattern, all.files = all.files,
      full.names = full.names, recursive = recursive, ignore.case = ignore.case,
      include.dirs = include.dirs, no.. = no..)

  if (!as.stash) {
    return(files)
  }
  llply(files, local_stash, is.file = TRUE)
}



#' @export
list_files_.local_stash <- function(x, pattern, all.files, full.names, recursive,
    ignore.case, include.dirs, no.., match.file, as.stash) {

  if (attr(x, 'type') != 'directory') {
    message('x is a file. The directory of x will be used instead.')
  }

  if (as.stash) {
    full.names <- TRUE
  }

  if (match.file && is.null(pattern)) {
    pattern <- get_filepattern(x)
  }

  files <- list.files(path = get_directory(x), pattern = pattern, all.files = all.files,
      full.names = full.names, recursive = recursive, ignore.case = ignore.case,
      include.dirs = include.dirs, no.. = no..)

  if (!as.stash) {
    return(files)
  }
  llply(files, local_stash, is.file = TRUE)
}



#' @export
list_files_.s3_stash <- function(x, pattern, all.files, full.names, recursive,
    ignore.case, include.dirs, no.., match.file, as.stash) {

  ## TODO: Verify applicability of all.files (show hidden files if TRUE)
  ## TODO: Verify applicability of no..

  if (attr(x, 'type') != 'directory') {
    message('x is a file. The directory of x will be used instead.')
  }

  ## Build argument list. Using a vector so that NULL arguments are not passed.
  ## Logical values pased after conversion to list, otherwise they are coersed
  ## to character vectors
  args <- c(
    bucket = attr(x, 'bucket'),
    key = attr(x, 'access.key.id'),
    secret = attr(x, 'secret.access.key'),
    region = attr(x, 'region')
  )
  prefix <- c(prefix = get_directory(x))
  if (prefix != '') {
    args <- c(args, prefix)
  }

  args <- as.list(args)
  args$parse_response <- FALSE

  ## Get bucket contents
  get.response <- do.call(get_bucket, args)
  ## Read the content and strip namespaces
  response.content <- xml_ns_strip(read_xml(get.response$content))
  ## Extract full files
  is.dir <- xml_text(xml_find_all(response.content, '//Contents/Size')) == 0
  dir.contents <- xml_text(xml_find_all(response.content, '//Key'))
  dir.contents <- gsub('/$', '', dir.contents)
  files <- dir.contents[!is.dir]

  ## Extract directories that were not explicitly created on s3
  dirs <- unlist(llply(files, path_levels, base.path = get_directory(x)))
  dirs <- unique(dirs)
  dirs <- dirs[!dirs %in% files]
  dirs <- unique(c(dir.contents[is.dir], dirs))
  dirs <- dirs[dirs != get_directory(x)]

  ## Filter out recursive values
  if (!recursive) {
    recursive.prefix <- gsub('/$', '', args$prefix)
    dirs <- dirs[recursive.prefix == dirname(dirs)]
    files <- files[recursive.prefix == dirname(files)]
  }

  dir.stashes <- file.stashes <- NULL
  if (length(dirs) > 0) {
    dir.stashes <- as.s3_stash(as.stash_dir(x), path = dirs,
        time.stamp = 'auto', uuid = 'auto', extension = 'auto',
        compression = 'auto')
  }
  if (length(files) > 0) {
    file.stashes <- as.s3_stash(as.stash_dir(x), path = files, is.file = TRUE,
        time.stamp = 'auto', uuid = 'auto', extension = 'auto',
        compression = 'auto')
  }

  if (include.dirs) {
    all.stashes <- list(dir.stashes, file.stashes)
  } else {
    all.stashes <- file.stashes
  }

  all.stashes <- flatten_list(all.stashes)

  if (match.file && is.null(pattern)) {
    pattern <- get_filepattern(x)
  }

  if (!is.null(pattern) && !is.null(all.stashes)) {
    matches <- grepl(pattern = pattern, basename(as.character(all.stashes)),
        ignore.case = ignore.case)
    all.stashes <- all.stashes[matches]
  }

  if (is.null(all.stashes)) {
    return(NULL)
  }

  if (as.stash) {
    return(all.stashes)
  }

  if (full.names) {
    return(as.character(all.stashes))
  }

  return(gsub(paste0('^', get_directory(x)), '', get_filepath(all.stashes)))

}



#' @export
list_files_.ftp_stash <- function(x, pattern, all.files, full.names, recursive,
    ignore.case, include.dirs, no.., match.file, as.stash) {

  ## TODO: Verify applicability of all.files (show hidden files if TRUE)
  ## TODO: Verify applicability of no..

  if (attr(x, 'type') != 'directory') {
    message('x is a file. The directory of x will be used instead.')
  }

  ## NOTE: Expecting the following structure in the reponse for a line
  ## "drwxr-x---  2 user System            0 Jul 09  2015 File Name"
  response.regx <- paste0(
    '([a-z\\-]{10})\\s+',  # Permissions
    '([0-9])\\s+',         # Type, 1 = file, 2 = dir
    '([_a-zA-Z0-9]+)\\s+', # user?
    '([a-zA-Z]+)\\s+',     # ?
    '([0-9]+)\\s+',        # Size
    '([A-Z][a-z]{2})\\s+', # Month
    '([0-3][0-9])\\s+',    # Day
    '([0-9:]+)\\s+',       # Year, or time?
    '(.+)')                # File name

  auth <- httr::authenticate(user = attr(x, 'user'),
      password = attr(x, 'password'))
  res.content <- httr::content(httr::GET(x, auth), as = 'text')
  parsed.res <- strsplit(res.content, split = '\n')[[1]]

  is.dir <- gsub(response.regx, '\\2', parsed.res) == 2
  dir.contents <- gsub(response.regx, '\\9', parsed.res)

  dirs <- dir.contents[is.dir]
  dirs <- dirs[!grepl('^\\.+$', dirs)]
  files <- dir.contents[!is.dir]

  dir.stashes <- file.stashes <- NULL
  if (length(dirs) > 0) {
    dir.stashes <- as.ftp_stash(as.stash_dir(x),
        path = paste(get_directory(x), dirs, sep = '/'), time.stamp = 'auto',
        uuid = 'auto', extension = 'auto', compression = 'auto')
  }
  if (length(files) > 0) {
    file.stashes <- as.ftp_stash(as.stash_dir(x),
        path = paste(get_directory(x), files, sep = '/'), is.file = TRUE,
        time.stamp = 'auto', uuid = 'auto', extension = 'auto',
        compression = 'auto')
  }

  if (recursive && !is.null(dir.stashes)) {
    recursed.files <- list_files(dir.stashes, pattern = pattern,
        all.files = all.files, full.names = full.names, recursive = recursive,
        ignore.case = ignore.case, include.dirs = include.dirs, no.. = no..,
        as.stash = TRUE)
    file.stashes <- list(file.stashes, recursed.files)
  }

  if (include.dirs) {
    all.stashes <- list(dir.stashes, file.stashes)
  } else {
    all.stashes <- file.stashes
  }

  all.stashes <- flatten_list(all.stashes)

  if (match.file && is.null(pattern)) {
    pattern <- get_filepattern(x)
  }

  if (!is.null(pattern) && !is.null(all.stashes)) {
    matches <- grepl(pattern = pattern, basename(as.character(all.stashes)),
        ignore.case = ignore.case)
    all.stashes <- all.stashes[matches]
  }

  if (is.null(all.stashes)) {
    return(NULL)
  }

  if (as.stash) {
    return(all.stashes)
  }

  if (full.names) {
    return(as.character(all.stashes))
  }

  return(gsub(paste0('^', get_directory(x)), '', get_filepath(all.stashes)))

}

