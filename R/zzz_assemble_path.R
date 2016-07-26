#' @export
assemble_path <- function(stash) {
  UseMethod('assemble_path')
}


#' @export
assemble_path.local_stash <- function(stash) {
  directory <- do.call(file.path, as.list(attr(stash, 'directory')))
  file <- attr(stash, 'file') %||% ''

  if (length(directory) == 0 || directory == '') {
    return(file)
  } else {
    return(file.path(directory, file))
  }
}


#' @export
assemble_path.s3_stash <- function(stash) {
  url.style <- attr(stash, 'url.style')
  bucket <- attr(stash, 'bucket')
  region <- attr(stash, 'region')
  directory <- paste(attr(stash, 'directory'), collapse = '/')
  file <- attr(stash, 'file')

  region.domain <- ifelse(is.null(region), 's3', paste0('s3-', region))
  main.url <- paste0(region.domain, '.amazonaws.com')

  if (url.style == 'virtual-hosted') {
    main.url <- paste(main.url, bucket, sep = '/')
  } else {
    main.url <- paste(bucket, main.url, sep = '.')
  }

  main.url <- paste0('http://', main.url)

  if (directory == '') {
    return(paste(main.url, file, sep = '/'))
  } else {
    return(paste(main.url, directory, file, sep = '/'))
  }
}


#' @export
assemble_path.ftp_stash <- function(stash) {
  server <- attr(stash, 'server')
  directory <- paste(attr(stash, 'directory'), collapse = '/')
  file <- attr(stash, 'file')

  if (directory == '') {
    return(paste('ftp:/', server, file, sep = '/'))
  } else {
    return(paste('ftp:/', server, directory, file, sep = '/'))
  }
}
