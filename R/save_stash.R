
## Split out the generation of the file stash object from the actual action
## This will allow the same file name to be used if desired. So if it is already
## of type = file, then it will go straight to the save action, if it is a
## directory, it will generate file objects based on the action and then save.
## If you want to force the file data to be regenerated, then.
##
## If you pass in a file object do you want the filename to be reprocessed to
## generate new timestamps? Or do you want it to be persisted?
##
## 2 seperate things, the regen is at the object level, if a file comes in, it
## will be rebuilt from the base file
##
## OR - if a new file name needs to be created, should the timestamp and uuid
## used across a list of 'to' objects be the same
##
## When regen.file = TRUE, then stashes of type 'file' with file attributes
## will have the file attr regenerated based on the base.file and other attrs.
## When regen.file = FALSE, then stashes of type 'file' will be used as is.
##
## When cache.time.stamp = TRUE and cache.uuid = TRUE then the same values will
## be used to generate the file names for all 'to' objects provided. This is only
## applicable for files that are regenerated, so behavior will be effected by
## regen.file.
##
## If a base.file is provided and regen.file = TRUE then the base.file will
## overwrite any base.file that may exist on the passed 'to' object.
##
## In general, all the args passed into save_stash that also exist on the _stash
## object will only be used when regen.file = TRUE
##

#' Save an object
#'
#' @param x Object to save
#' @param file.name Base file name for the saved object
#' @param to Path for where the object will be saved. Accepts a character
#'    string or *_stash object.
#' @param time.stamp TRUE or FALSE. When TRUE, a time stamp will be appended to
#'    the \code{file.name}. It is recommended to use time.stamp OR uuid, not
#'    both. Using both can cause the file name to become very long.
#' @param uuid TRUE or FALSE. When TRUE, a uuid will be appended to
#'    the \code{file.name}. It is recommended to use time.stamp OR uuid, not
#'    both. Using both can cause the file name to become very long.
#' @param extension Extension to append to the file.
#' @param compression Accepts NULL for no compression, or 'gz' for gzip.
#' @param checksum Not being used right now.
#' @param save.fn Any function where the first argument is the object and the
#'    second is the file path.
#' @param ... Arguments to pass to \code{save.fn}.
#'
#' @return List of stash objects that were successfully saved.
#' @export
save_stash <- function(x, to, save.fn = write.csv, ..., regen.file = TRUE,
    cache.time.stamp = TRUE, cache.uuid = TRUE, simplify = TRUE) {

  to <- as.stash_file(x = to, regen.file = regen.file,
      cache.time.stamp = cache.time.stamp, cache.uuid = cache.uuid,
      simplify = FALSE)

  res <- llply(to, save_stash_, x = x, save.fn = save.fn, ... = ...)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  }
  return(res)
}


save_stash_ <- function(x, to, save.fn, ...) {
  UseMethod('save_stash_', to)
}


#' @export
save_stash_.local_stash <- function(x, to, save.fn, ...) {

  if (!dir_exists(to)) {
    dir_create(to, recursive = TRUE)
  }

  message('Saving locally...')
  conn <- open_file_conn(to)
  args <- list(quote(x), quote(conn), ...)
  do.call(save.fn, args)
  close(conn)

  return(set_validation_message(to))
}

#' @export
save_stash_.s3_stash <- function(x, to, save.fn, ...) {

  message('Saving temp file locally...')
  temp.stash <- as.local_stash(to, path = tempfile())
  temp.file <- suppressMessages(
      save_stash(x, to = temp.stash, regen.file = FALSE, save.fn, ...))

  message('Pushing file to S3...')
  put.response <- put_object_wrapper(temp.file, to)

  unlink(temp.stash, recursive = TRUE)
  return(set_validation_message(to))
}


