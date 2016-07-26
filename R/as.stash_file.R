## TODO: These do no take into account most of the variables if they are already files

#' @export
as.stash_file <- function(x, regen.file = FALSE, cache.time.stamp = FALSE,
    cache.uuid = FALSE, ..., simplify = TRUE) {

  time.stamp.cache <- uuid.cache <- NULL
  if (cache.time.stamp) {
    time.stamp.cache <- gen_timestamp(kDefaultDigitsSecs)
  }
  if (cache.uuid) {
    uuid.cache <- gen_uuid()
  }

  x <- as.flat_list(x)
  res <- llply(x, as.stash_file_, regen.file = regen.file,
      .time.stamp.cache = time.stamp.cache, .uuid.cache = uuid.cache, ... = ...)
  res <- as.flat_list(res)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  }
  return(res)
}

as.stash_file_ <- function(x, regen.file = FALSE, .time.stamp.cache = NULL,
    .uuid.cache = NULL, ...) {

  UseMethod('as.stash_file_')
}

## If you pass in a character string, we assume it is a local file
#' @export
as.stash_file_.character <- function(x, regen.file = FALSE,
    .time.stamp.cache = NULL, .uuid.cache = NULL, ...) {

  x <- as.local_stash(x = x, ... = ...)
  as.stash_file_(x = x, regen.file = FALSE,
      .time.stamp.cache = .time.stamp.cache, .uuid.cache = .uuid.cache,
      ... = ...)
}


#' @export
as.stash_file_.local_stash <- function(x, regen.file = FALSE,
    .time.stamp.cache = NULL, .uuid.cache = NULL, ...) {

  if (regen.file || is.null(get_filename(x))) {
    args <- list(...)
    if (is.null(args$base.file) && is.null(get_basefile(x))) {
      stop('Please provide a base.file name as an argument or in the stash object')
    }
    args <- update_arguments(get_attributes(x), args, kLocalStashArgs)

    args$.file.name <- gen_filename(base.file = args$base.file,
        time.stamp = args$time.stamp, uuid = args$uuid,
        extension = args$extension, compression = args$compression,
        .time.stamp.cache = .time.stamp.cache, .uuid.cache = .uuid.cache)

    args$.directory <- get_directory(x)

    new.args <- update_arguments(get_arguments(x), args, kLocalStashArgs)
    return(do.call(local_stash, new.args))
  }
  return(x)

}


#' @export
as.stash_file_.ftp_stash <- function(x, regen.file = FALSE,
    .time.stamp.cache = NULL, .uuid.cache = NULL, ...) {

  if (regen.file || is.null(get_filename(x))) {
    args <- list(...)
    if (is.null(args$base.file) && is.null(get_basefile(x))) {
      stop('Please provide a base.file name as an argument or in the stash object')
    }
    args <- update_arguments(get_attributes(x), args, kFtpStashArgs)

    args$.file.name <- gen_filename(base.file = args$base.file,
        time.stamp = args$time.stamp, uuid = args$uuid,
        extension = args$extension, compression = args$compression,
        .time.stamp.cache = .time.stamp.cache, .uuid.cache = .uuid.cache)

    args$.directory <- get_directory(x)

    new.args <- update_arguments(get_arguments(x), args, kFtpStashArgs)
    return(do.call(ftp_stash, new.args))
  }
  return(x)
}


#' @export
as.stash_file_.s3_stash <- function(x, regen.file = FALSE,
    .time.stamp.cache = NULL, .uuid.cache = NULL, ...) {

  args <- list(...)
  if (regen.file || is.null(get_filename(x))) {
    if (is.null(args$base.file) && is.null(get_basefile(x))) {
      stop('Please provide a base.file name as an argument or in the stash object')
    }
    args <- update_arguments(get_attributes(x), args, kS3StashArgs)

    args$.file.name <- gen_filename(base.file = args$base.file,
        time.stamp = args$time.stamp, uuid = args$uuid,
        extension = args$extension, compression = args$compression,
        .time.stamp.cache = .time.stamp.cache, .uuid.cache = .uuid.cache)

    args$.directory <- get_directory(x)

    new.args <- update_arguments(get_arguments(x), args, kS3StashArgs)
    return(do.call(s3_stash, new.args))
  }
  return(x)
}
