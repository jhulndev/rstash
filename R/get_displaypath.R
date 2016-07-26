#' @export
get_displaypath <- function(x) {
  x <- as.flat_list(x)
  unlist(llply(x, get_displaypath_))
}

get_displaypath_ <- function(x) {
  UseMethod('get_displaypath_')
}


#' @export
get_displaypath_.local_stash <- function(x) {
  directory <- get_directory(x)
  file <- get_filename(x)

  if (length(directory) == 0 || directory == '') {
    directory <- NULL
  }
  return(paste0(c(directory, file), collapse = .Platform$file.sep))
}


#' @export
get_displaypath_.s3_stash <- function(x) {
  url.style <- attr(x, 'url.style')
  bucket <- get_container(x)
  region <- attr(x, 'region')
  directory <- get_directory(x)
  file <- get_filename(x)

  region.domain <- ifelse(is.null(region), 's3', paste0('s3-', region))
  main.url <- paste0(region.domain, '.amazonaws.com')

  if (url.style == 'virtual-hosted') {
    main.url <- paste(main.url, bucket, sep = '/')
  } else {
    main.url <- paste(bucket, main.url, sep = '.')
  }

  main.url <- paste0('http://', main.url)
  if (directory == '') {
    directory <- NULL
  }
  return(paste0(c(main.url, directory, file), collapse = '/'))
}


#' @export
get_displaypath_.ftp_stash <- function(x) {
  server <- attr(x, 'server')
  directory <- get_directory(x)
  file <- get_filename(x)

  if (directory == '') {
    directory <- NULL
  }
  return(paste0(c('ftp:/', server, directory, file), collapse = '/'))
}
