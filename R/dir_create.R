#' @export
dir_create <- function(x, show.warnings = TRUE, recursive = FALSE, mode = '0777') {
  x <- as.flat_list(x)
  laply(x, dir_create_, show.warnings = show.warnings, recursive = recursive,
      mode = mode)
}

dir_create_ <- function(x, show.warnings, recursive, mode) {
  UseMethod('dir_create_')
}

#' @export
dir_create_.character <- function(x, show.warnings, recursive, mode) {
  dir.create(path = x, showWarnings = show.warnings, recursive = recursive,
      mode = mode)
}

#' @export
dir_create_.local_stash <- function(x, show.warnings, recursive, mode) {
  x <- get_directory(x)
  dir_create_(x, show.warnings = show.warnings, recursive = recursive,
      mode = mode)
}

#' @export
dir_create_.s3_stash <- function(x, show.warnings, recursive, mode) {
  return(TRUE)
}

#' #' @export
#' dir_create_.ftp_stash <- function(x, show.warnings, recursive, mode) {
#'
#' }
