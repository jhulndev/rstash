#' @export
dir_exists <- function(...) {
  x <- as.flat_list(...)
  laply(x, dir_exists_)
}

dir_exists_ <- function(x) {
  UseMethod('dir_exists_')
}


#' @export
dir_exists_.character <- function(x) {
  dir.exists(x)
}


#' @export
dir_exists_.local_stash <- function(x) {
  if (get_type(x) != 'directory') {
    message('x is a file. The directory of x will be used instead.')
  }
  dir.exists(get_directory(x))
}


#' @export
dir_exists_.ftp_stash <- function(x) {

  if (get_type(x) != 'directory') {
    message('x is a file. The directory of x will be used instead.')
    x <- as.stash_dir(x)
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
  res.content <- tryCatch(httr::content(httr::GET(x, auth), as = 'text'),
      error = function(cond) NA)

  ## TODO: Needs to be more robust with access denied and errors
  if (is.na(res.content)) {
    return(FALSE)
  }
  parsed.res <- strsplit(res.content, split = '\n')[[1]]

  is.dir <- gsub(resonse.regx, '\\2', parsed.res) == 2
  dir.contents <- gsub(resonse.regx, '\\9', parsed.res)

  dirs <- dir.contents[is.dir]
  dirs <- dirs[!grepl('^\\.+$', dirs)]

  return(get_filepath(x) %in% dirs ||
      (get_directory(x) == get_filepath(x) && length(dir.contents) > 0))
}


#' @export
dir_exists_.s3_stash <- function(x) {

  if (get_type(x) != 'directory') {
    message('x is a file. The directory of x will be used instead.')
    x <- as.stash_dir(x)
  }

  args <- c(
    bucket = attr(x, 'bucket'),
    key = attr(x, 'access.key.id'),
    secret = attr(x, 'secret.access.key'),
    region = attr(x, 'region')
  )
  prefix <- c(prefix = get_filepath(x))
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
  dirs <- unlist(llply(files, path_levels, base.path = get_directory(x),
      base.inclusive = TRUE))
  dirs <- unique(dirs)
  dirs <- dirs[!dirs %in% files]
  dirs <- unique(c(dir.contents[is.dir], dirs))

  return(get_directory(x) %in% dirs)
}




