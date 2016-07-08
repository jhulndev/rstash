# This will generate a timestamp that is safe to use in file names
generate_timestamp <- function(digits.secs = 0, with.date = TRUE,
    with.time = TRUE) {

  date.format <- 'D%Y%m%d'
  time.format <- 'T%H%M%OS'

  if (!with.date && !with.time) {
    stop('Both with.date and with.time are FALSE. ',
        'Please set one to TRUE.')
  }

  if (digits.secs > 6) {
    digits.secs <- 6
    message('Max digits.secs of 6 will be used')
  }

  if (!with.date) {
    date.format <- NULL
  }

  if (!with.time) {
    time.format <- NULL
  } else {
    time.format <- paste0(time.format, digits.secs)
  }

  date.time.format <- paste(c(date.format, time.format), collapse = '')
  time.stamp <- format(Sys.time(), format = date.time.format, tz = "UTC")
  time.stamp <- gsub('[-:\\.]', '', time.stamp)

  return(time.stamp)
}


generate_timestamp_pattern <- function(digits.secs = 0, with.date = TRUE,
    with.time = TRUE) {

  date.format <- 'D[0-9]{8}'
  time.format <- paste0('T[0-9]{', 6 + digits.secs, '}')

  if (!with.date && !with.time) {
    stop('Both with.date and with.time are FALSE. ',
        'Please set one to TRUE.')
  }

  if (digits.secs > 6) {
    digits.secs <- 6
    message('Max digits.secs of 6 will be used')
  }

  if (!with.date) {
    date.format <- NULL
  }

  if (!with.time) {
    time.format <- NULL
  }

  date.time.format <- paste(c(date.format, time.format), collapse = '')
  return(date.time.format)
}


generate_filename <- function(file.name, time.stamp = FALSE, uuid = FALSE,
    extension = 'txt', compression = NULL) {

  compression <- validate_compression(compression)

  if (isTRUE(time.stamp)) {
    file.name <- paste(c(file.name, generate_timestamp(2)), collapse = '_')
  }
  if (isTRUE(uuid)) {
    file.name <- paste(c(file.name, uuid::UUIDgenerate(FALSE)), collapse = '_')
  }

  file.name <- paste(c(file.name, extension, compression), collapse = '.')
  return(file.name)
}


generate_filepattern <- function(file.name, time.stamp = FALSE, uuid = FALSE,
    extension = NULL, compression = NULL) {

  compression <- validate_compression(compression)

  if (isTRUE(time.stamp)) {
    file.name <- paste(c(file.name, generate_timestamp_pattern(2)),
        collapse = '\\_')
  }
  if (isTRUE(uuid)) {
    file.name <- paste(c(file.name, generate_uuid_pattern(FALSE)),
        collapse = '\\_')
  }

  file.name <- paste(c(file.name, extension, compression), collapse = '\\.')
  file.name <- paste0('^', file.name, '$')
  return(file.name)

}


generate_uuid_pattern <- function(use.time = FALSE) {
  '[0-9a-z]{8}\\-[0-9a-z]{4}\\-[0-9a-z]{4}\\-[0-9a-z]{4}\\-[0-9a-z]{12}'
}


ifnull <- function(test, yes, no) {
  ifelse(is.null(test), yes, no)
}


validate_compression <- function(compression, allowed = c('gz')) {

  if (is.null(compression)) {
    return(compression)
  }

  if (is.character(compression) && compression %in% allowed) {
    return(compression)
  }

  stop('Compression type of ', compression, ' is not supported. Options ',
      'include ', paste0('"', allowed, '"', collapse = ', '), '.')

}


validate_directory <- function(dir, create = FALSE, recursive = FALSE) {

  dir <- gsub('^$', '\\.', dir)

  if (!dir.exists(dir) && create) {
    suppressWarnings(
      dir.create(dir, recursive = recursive))
  }

  if (dir.exists(dir)) {
    return(dir)

  } else {
    if (create && !recursive) {
      stop('The directory "', dir, '" could not be created, please check ',
          ' permissions or try setting recursive = TRUE.')
    } else if (create && recursive) {
      stop('The directory "', dir, '" could not be created, please check ',
          ' permissions.')
    } else if (!create) {
      stop('The directory "', dir, '" does not exist.')
    }
    return(NA)

  }
}


file.path.s3 <- function(...) {
  args <- list(...)
  args <- args[!args %in% c('', '.')]
  paste0(args, collapse = '/')
}


file.exists.s3 <- function(object.key, bucket) {
  get.response <- get_bucket(bucket, prefix = object.key, max = 1)
  length(get.response) > 0
}


as.stash <- function(x) {
  ## Check if the to path is of a stash type and convert if it is not.
  if (!grepl('.+_stash$', class(x))) {
    x <- local_stash(as.character(x))
  }
  return(x)
}


list.files.s3 <- function(bucket, dir = '', pattern = NULL, full.names = FALSE,
    recursive = FALSE) {
  ## Get bucket contents
  get.response <- get_bucket(bucket, prefix = dir, parse_response = FALSE)
  ## Read the content and strip namespaces
  response.content <- xml_ns_strip(read_xml(get.response$content))
  ## Extract keys
  dir.contents <- xml_text(xml_find_all(response.content, '//Key'))

  if (!recursive) {
    is.recursive.dir <- dir != dirname(dir.contents)
    dir.contents <- dir.contents[!is.recursive.dir]
  }

  dir.files <- basename(dir.contents)

  if (!is.null(pattern)) {
    file.matches <- grepl(pattern, dir.files)
  } else {
    file.matches <- TRUE
  }

  if (isTRUE(full.names)) {
    return(dir.contents[file.matches])
  } else {
    return(dir.files[file.matches])
  }
}


clean_empty_dir <- function(dir) {
  remaining.files <- list.files(dir)
  if (length(remaining.files) == 0) {
    unlink(dir, recursive = TRUE)
  }
}


`%||%` <- function(a, b) {
  if (is.null(a)) {
    b
  } else {
    a
  }
}


validate_files <- function(files, success.msg = NULL, fail.msg = NULL) {
  if (length(files) == 1 && !is.list(files)) {
    files <- list(files)
  }
  lapply(files, validate_file_, success.msg = success.msg, fail.msg = fail.msg)

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


put_object_wrapper <- function(file, object, bucket, headers = list(), ...) {

  ## put_object is not closing it's connection to the file. Need to find
  ## the connection and manually close it in order to prevent warning messages
  file <- normalizePath(file)
  put.result <- put_object(file, object, bucket, headers, ...)

  all.open.cons <- data.frame(showConnections())
  bad.open.con <- row.names(
      all.open.cons[all.open.cons$description == file, ])
  if (length(bad.open.con) > 0) {
    close(getConnection(bad.open.con))
  }

  return(put.result)
}
