#' @export
as.local_stash <- function(x, ..., simplify = TRUE) {
  x <- as.flat_list(x)
  res <- llply(x, as.local_stash_, ... = ...)
  res <- as.flat_list(res)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  }
  return(res)
}

#' @export
as.local_stash_ <- function(x, ...) {
  UseMethod('as.local_stash_')
}

#' @export
as.local_stash_.character <- function(x, ...) {
  args <- list(...)
  args$path <- x

  if (!'is.file' %in% names(args)) {
    args$is.file <- file.exists(x) && !dir.exists(x)
  }

  do.call(local_stash, args)
}

#' @export
as.local_stash_.local_stash <- function(x, ...) {
  .dots <- list(...)
  new.args <- update_arguments(get_arguments(x), get_attributes(x), kLocalStashArgs)
  new.args <- update_arguments(new.args, .dots, kLocalStashArgs)

  if (is.null(.dots$path)) {
    if (is.null(new.args$.file.name)) {
      new.args$.file.name <- get_filename(x)
    }
    if (is.null(new.args$.directory)) {
      new.args$.directory <- get_directory(x)
    }
  }

  do.call(local_stash, new.args)
}

#' @export
as.local_stash_.s3_stash <- function(x, ...) {
  .dots <- list(...)
  new.args <- update_arguments(get_arguments(x), get_attributes(x), kLocalStashArgs)
  new.args <- update_arguments(new.args, .dots, kLocalStashArgs)

  if (is.null(.dots$path)) {
    if (is.null(new.args$.file.name)) {
      new.args$.file.name <- get_filename(x)
    }
    if (is.null(new.args$.directory)) {
      new.args$.directory <- get_directory(x)
    }
  }

  do.call(local_stash, new.args)
}

#' @export
as.local_stash_.ftp_stash <- function(x, ...) {
  .dots <- list(...)
  new.args <- update_arguments(get_arguments(x), get_attributes(x), kLocalStashArgs)
  new.args <- update_arguments(new.args, .dots, kLocalStashArgs)

  if (is.null(.dots$path)) {
    if (is.null(new.args$.file.name)) {
      new.args$.file.name <- get_filename(x)
    }
    if (is.null(new.args$.directory)) {
      new.args$.directory <- get_directory(x)
    }
  }

  do.call(local_stash, new.args)
}
