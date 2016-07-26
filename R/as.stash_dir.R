#' @export
as.stash_dir <- function(x, ..., simplify = TRUE) {
  x <- as.flat_list(x)
  res <- llply(x, as.stash_dir_, ... = ...)
  res <- as.flat_list(res)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  }
  return(res)
}

as.stash_dir_ <- function(x, ...) {
  UseMethod('as.stash_dir_')
}

#' @export
as.stash_dir_.character <- function(x, ...) {
  args <- list(...)
  args$is.file <- FALSE
  args$base.file <- NULL
  args$x <- x
  do.call(local_stash, args)
}

#' @export
as.stash_dir_.local_stash <- function(x, ...) {
  args <- list(...)

  if (!is.null(get_filename(x)) || !is.null(get_basefile(x))) {
    arg.overwrite <- list(path = get_directory(x), is.file = FALSE,
        base.file = NULL)
    args <- update_arguments(args, arg.overwrite, kLocalStashArgs)
  }

  new.args <- update_arguments(get_attributes(x), args, kLocalStashArgs)
  new.args$x <- x
  return(do.call(as.stash, new.args))
}


#' @export
as.stash_dir_.ftp_stash <- function(x, ...) {
  args <- list(...)

  if (!is.null(get_filename(x)) || !is.null(get_basefile(x))) {
    arg.overwrite <- list(path = get_directory(x), is.file = FALSE,
        base.file = NULL)
    args <- update_arguments(args, arg.overwrite, kLocalStashArgs)
  }

  new.args <- update_arguments(get_arguments(x), args, kFtpStashArgs)
  new.args$x <- x
  return(do.call(as.stash, new.args))
}


#' @export
as.stash_dir_.s3_stash <- function(x, ...) {
  args <- list(...)

  if (!is.null(get_filename(x)) || !is.null(get_basefile(x))) {
    arg.overwrite <- list(path = get_directory(x), is.file = FALSE,
        base.file = NULL)
    args <- update_arguments(args, arg.overwrite, kLocalStashArgs)
  }

  new.args <- update_arguments(get_arguments(x), args, kS3StashArgs)
  new.args$x <- x
  return(do.call(as.stash, new.args))
}
