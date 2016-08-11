#' @export
as.ftp_stash <- function(x, ..., simplify = TRUE) {
  x <- as.flat_list(x)
  res <- llply(x, as.ftp_stash_, ... = ...)
  res <- as.flat_list(res)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  }
  return(res)
}

#' @export
as.ftp_stash_ <- function(x, ...) {
  UseMethod('as.ftp_stash_')
}

#' @export
as.ftp_stash_.character <- function(x, ...) {
  args <- list(...)
  args$path <- x
  do.call(ftp_stash, args)
}

#' @export
as.ftp_stash_.local_stash <- function(x, ...) {
  .dots <- list(...)
  new.args <- update_arguments(get_arguments(x), get_attributes(x), kFtpStashArgs)
  new.args <- update_arguments(new.args, .dots, kFtpStashArgs)

  if (is.null(.dots$path)) {
    if (is.null(new.args$.file.name)) {
      new.args$.file.name <- get_filename(x)
    }
    if (is.null(new.args$.directory)) {
      new.args$.directory <- get_directory(x)
    }
  }

  do.call(ftp_stash, new.args)
}

#' @export
as.ftp_stash_.s3_stash <- function(x, ...) {
  .dots <- list(...)
  new.args <- update_arguments(get_arguments(x), get_attributes(x), kFtpStashArgs)
  new.args <- update_arguments(new.args, .dots, kFtpStashArgs)

  if (is.null(.dots$path)) {
    if (is.null(new.args$.file.name)) {
      new.args$.file.name <- get_filename(x)
    }
    if (is.null(new.args$.directory)) {
      new.args$.directory <- get_directory(x)
    }
  }

  do.call(ftp_stash, new.args)
}

#' @export
as.ftp_stash_.ftp_stash <- function(x, ...) {
  .dots <- list(...)
  new.args <- update_arguments(get_arguments(x), get_attributes(x), kFtpStashArgs)
  new.args <- update_arguments(new.args, .dots, kFtpStashArgs)

  if (is.null(.dots$path)) {
    if (is.null(new.args$.file.name)) {
      new.args$.file.name <- get_filename(x)
    }
    if (is.null(new.args$.directory)) {
      new.args$.directory <- get_directory(x)
    }
  }

  do.call(ftp_stash, new.args)
}
