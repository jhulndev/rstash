validate_files <- function(files, success.msg = NULL, fail.msg = NULL) {
  files <- as.flat_list(files)
  res <- lapply(files, validate_file_, success.msg = success.msg,
      fail.msg = fail.msg)

  if (simplify && length(res) == 1) {
    return(res[[1]])
  }
  return(res)
}

#' @export
validate_file_ <- function(file, success.msg, fail.msg) {
  UseMethod('validate_file_', file)
}

#' @export
validate_file_.local_stash <- function(file, success.msg, fail.msg) {

  success <- file.exists(file)
  if (!is.null(success.msg)) {
    plyr::l_ply(file[success], function(x) message(success.msg, x))
  }

  if (!is.null(fail.msg)) {
    plyr::l_ply(file[!success], function(x) message(fail.msg, x))
  }

  file[!success] <- NA
  return(file)

}

#' @export
validate_file_.s3_stash <- function(file, success.msg, fail.msg) {

  success <- file.exists.s3(file, bucket = attr(file, 'bucket'))
  if (!is.null(success.msg)) {
    plyr::l_ply(file[success], function(x) message(success.msg, x))
  }

  if (!is.null(fail.msg)) {
    plyr::l_ply(file[!success], function(x) message(fail.msg, x))
  }

  file[!success] <- NA
  return(file)

}
