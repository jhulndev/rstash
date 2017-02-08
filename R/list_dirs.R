#' List the Directory in a Stash or Directory/Folder
#'
#' @export
list_dirs <- function(x = '.', full.names = TRUE, recursive = TRUE,
    as.stash = TRUE, simplify = TRUE) {

  x <- as.flat_list(x)
  res <- llply(x, list_dirs_, full.names = full.names, recursive = recursive,
      as.stash = as.stash)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  }
  return(res)
}



list_dirs_ <- function(x, full.names, recursive, as.stash) {
  UseMethod('list_dirs_')
}



#' @export
list_dirs_.character <- function(x, full.names, recursive, as.stash) {

  if (as.stash) {
    full.names <- TRUE
  }

  dirs <- list.dirs(path = x, full.names = full.names, recursive = recursive)

  if (!as.stash) {
    return(dirs)
  }
  llply(dirs, local_stash, is.file = FALSE)
}



#' @export
list_dirs_.local_stash <- function(x, full.names, recursive, as.stash) {

  if (attr(x, 'type') != 'directory') {
    message('x is a file. The directory of x will be used instead.')
  }

  if (as.stash) {
    full.names <- TRUE
  }

  dirs <- list.dirs(path = x, full.names = full.names, recursive = recursive)

  if (!as.stash) {
    return(dirs)
  }
  llply(dirs, local_stash, is.file = FALSE)
}



#' @export
list_dirs_.s3_stash <- function(x, full.names, recursive, as.stash) {

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
  }

  dir.stashes <- NULL
  if (length(dirs) > 0) {
    dir.stashes <- as.s3_stash(x, path = dirs)
  }

  dir.stashes <- flatten_list(dir.stashes)

  if (is.null(dir.stashes)) {
    return(NULL)
  }

  if (as.stash) {
    return(dir.stashes)
  }

  if (full.names) {
    return(as.character(dir.stashes))
  }

  return(gsub(paste0('^', get_directory(x), '/'), '', get_filepath(dir.stashes)))
}



#' @export
list_dirs_.ftp_stash <- function(x, full.names, recursive, as.stash) {

  ## TODO: Verify applicability of all.files (show hidden files if TRUE)
  ## TODO: Verify applicability of no..

  if (attr(x, 'type') != 'directory') {
    message('x is a file. The directory of x will be used instead.')
  }

  ## NOTE: Expecting the following structure in the reponse for a line
  ## "drwxr-x---  2 user System            0 Jul 09  2015 File Name"
  resonse.regx <- paste0(
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
  parsed.res <- strsplit(res.content, split = '\r?\n')[[1]]

  is.dir <- gsub(resonse.regx, '\\2', parsed.res) == 2
  dir.contents <- gsub(resonse.regx, '\\9', parsed.res)

  dirs <- dir.contents[is.dir]
  dirs <- dirs[!grepl('^\\.+$', dirs)]

  dir.stashes <- NULL
  if (length(dirs) > 0) {
    dir.stashes <- as.ftp_stash(x, path = paste0(get_directory(x), dirs))
  }

  if (recursive && !is.null(dir.stashes)) {
    recursed.dirs <- list_dirs(dir.stashes, full.names = full.names,
        recursive = recursive, as.stash = TRUE)
    dir.stashes <- list(dir.stashes, recursed.dirs)
  }

  dir.stashes <- flatten_list(dir.stashes)

  if (is.null(dir.stashes)) {
    return(NULL)
  }

  if (as.stash) {
    return(dir.stashes)
  }

  if (full.names) {
    return(as.character(dir.stashes))
  }

  return(gsub(paste0('^', get_directory(x), '/'), '', get_filepath(dir.stashes)))

}

