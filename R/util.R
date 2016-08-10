## General Utility Functions ===================================================
ifnull <- function(test, yes, no) {
  ifelse(is.null(test), yes, no)
}

`%||%` <- function(a, b) {
  if (is.null(a)) {
    b
  } else {
    a
  }
}

split_path <- function(path, sep = '/|\\\\') {
  strsplit(path, sep)[[1]]
}

clean_empty_dir <- function(dir) {
  remaining.files <- list.files(dir)
  if (length(remaining.files) == 0) {
    unlink(dir, recursive = TRUE)
  }
}

update_arguments <- function(old.args, new.args, allowed.args = NULL) {
  old.args <- old.args[!names(old.args) %in% names(new.args)]
  all.args <- c(old.args, new.args)

  if (!is.null(allowed.args)) {
    all.args <- all.args[names(all.args) %in% allowed.args]
  }
  return(all.args)
}

open_file_conn <- function(x) {
  if (!is.null(get_compression(x))) {
    return(gzfile(x, 'w'))
  }
  return(file(x, 'w'))
}

set_messages <- function(x, level = 'message', ...) {
  if (!level %in% c('message', 'warning', 'critical')) {
    stop('level must be "message", "warning", or "critical"')
  }
  attr(x, level) <- paste(...)
  return(x)
}

set_validation_message <- function(x) {
  valid <- file_exists(x)
  x[valid] <- set_messages(x[valid], 'message', 'File exists')
  x[!valid] <- set_messages(x[!valid], 'warning', 'File does not exist')
  return(x)
}

clear_messages <- function(x, ...) {
  levels <- list(...)
  if (length(levels) == 0) {
    levels <- NULL
  }

  allowed.messages <- c('message', 'warning', 'critical')
  if (!is.null(levels) && !all(levels %in% allowed.messages)) {
    stop('level must be NULL, "message", "warning", or "critical"')
  }
  if (is.null(levels)) {
    levels <- allowed.messages
  }
  for (level in levels) {
    attr(x, level) <- NULL
  }
  return(x)
}

as.flat_list <- function(x) {
  if (length(x) > 1) {
    x <- as.list(x)
  } else {
    x <- list(x)
  }
  flatten_list(x)
}

flatten_list <- function(x, i = 1, clean.items = list()) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.list(x)) {
    clean.items[[i]] <- x
    return(clean.items)
  }
  for (item in x) {
    if (is.list(item)) {
      clean.items <- flatten_list(item, i, clean.items)
      i <- length(clean.items) + 1
    } else if (!is.null(item)) {
      clean.items[[i]] <- item
      i <- i + 1
    }
  }
  return(clean.items)
}

path_levels <- function(x, base.path = NULL, base.inclusive = FALSE) {
  sep <- gsub('.*(/|\\\\).*', '\\1', x)
  path <- split_path(x, sep)
  seq_ids <- llply(seq_along(path), function(x) seq(from = 1, to = x))
  all.paths <- laply(seq_ids, function(x) paste0(path[x], collapse = sep))

  if (is.null(base.path) || base.path == '') {
    return(all.paths)
  }
  filter.start <- which(base.path == all.paths)
  if (!base.inclusive) {
    filter.start <- filter.start + 1
  }
  return(all.paths[filter.start:length(all.paths)])
}

expand.arguments <- function(...) {
  args <- list(...)
  max.length <- max(sapply(args, length))
  args <- llply(args, function(x) {
    if (!is.list(x) && length(x) == 1) {
      list(x)
    } else if (!is.list(x) && length(x) > 1) {
      as.list(x)
    } else {
      x
    }})
  lapply(args, rep, length = max.length)
}


arrange.arguments <- function(expanded.args) {
  n <- length(expanded.args[[1]])
  lapply(1:n, function(i) lapply(expanded.args, "[[", i))
}





## Timestamp Generation & Pattern ==============================================
gen_timestamp <- generate_timestamp <- function(digits.secs = 0, with.date = TRUE,
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

gen_timestamp_regx <- generate_timestamp_pattern <- function(digits.secs = 0, with.date = TRUE,
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


## Filename Generation & Pattern ===============================================
gen_filename <- generate_filename <- function(base.file, time.stamp = FALSE, uuid = FALSE,
    extension = 'txt', compression = NULL, .time.stamp.cache = NULL,
    .uuid.cache = NULL) {

  file.name <- base.file
  compression <- validate_compression(compression)

  if (isTRUE(time.stamp)) {
    time.stamp.cache <- .time.stamp.cache %||%
        generate_timestamp(kDefaultDigitsSecs)
    file.name <- paste(c(file.name, time.stamp.cache), collapse = '_')
  }
  if (isTRUE(uuid)) {
    uuid.cache <- .uuid.cache %||% gen_uuid(FALSE)
    file.name <- paste(c(file.name, uuid.cache), collapse = '_')
  }

  file.name <- paste(c(file.name, extension, compression), collapse = '.')
  return(file.name)
}


gen_filename_regx <- generate_filepattern <- function(file.name, time.stamp = FALSE, uuid = FALSE,
    extension = NULL, compression = NULL) {

  compression <- validate_compression(compression)

  if (isTRUE(time.stamp)) {
    file.name <- paste(c(file.name, generate_timestamp_pattern(kDefaultDigitsSecs)),
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


## UUID Pattern ================================================================
gen_uuid <- function(use.time = FALSE) {
  if (!requireNamespace('uuid', quietly = TRUE)) {
    stop('"uuid" needed for this function to work. Please install it.',
      call. = FALSE)
  }
  uuid::UUIDgenerate(use.time)
}

gen_uuid_regx <- generate_uuid_pattern <- function(use.time = FALSE) {
  '[0-9a-z]{8}\\-[0-9a-z]{4}\\-[0-9a-z]{4}\\-[0-9a-z]{4}\\-[0-9a-z]{12}'
}


## Stash Gen Helpers ===========================================================
gen_stash_type <- function(base.file, is.file, .file.name) {
  if (!is.null(base.file) || is.file || !is.null(.file.name)) {
    return('file')
  }
  return('directory')
}

gen_stash_dirpath <- function(base.file, is.file, path, .directory) {
  if (!is.null(.directory)) {
    return(.directory)
  }
  if (!is.null(base.file) || !is.file) {
    return(path)
  }
  if (length(split_path(path)) > 1) {
    return(dirname(path))
  }
  return('')
}

gen_stash_filename <- function(base.file, is.file, path, .file.name) {
  if (!is.null(.file.name)) {
    return(.file.name)
  }
  if (!is.null(base.file) || !is.file) {
    return(NULL)
  }
  return(basename(path))
}


## Validation - Compression & Directory ========================================
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

validate_directory <- function(dir, create = FALSE, recursive = FALSE,
    stop.on.fail = TRUE) {

  dir <- gsub('^$', '\\.', dir)

  if (!dir.exists(dir) && create) {
    suppressWarnings(
      dir.create(dir, recursive = recursive))
  }

  if (dir.exists(dir)) {
    return(TRUE)

  } else {
    if (create && !recursive) {
      warning('The directory "', dir, '" could not be created, please check ',
          ' permissions or try setting recursive = TRUE.')
    } else if (create && recursive) {
      warning('The directory "', dir, '" could not be created, please check ',
          ' permissions.')
    } else if (!create) {
      warning('The directory "', dir, '" does not exist.')
    }
    if (stop.on.fail) {
      stop('Directory invalid.')
    } else {
      return(FALSE)
    }

  }
}

## Process Stash Helpers =======================================================
process_stash_filename <- function(file.name = NULL, time.stamp = 'auto',
    uuid = 'auto', extension = 'auto', compression = 'auto') {

  proc.compression <- process_compression(file.name, compression)
  file.name <- proc.compression$file.name
  compression <- proc.compression$compression

  proc.extension <- process_extension(file.name, extension)
  file.name <- proc.extension$file.name
  extension <- proc.extension$extension

  proc.uuid <- process_uuid(file.name, uuid)
  file.name <- proc.uuid$file.name
  uuid <- proc.uuid$uuid

  proc.time.stamp <- process_timestamp(file.name, time.stamp)
  file.name <- proc.time.stamp$file.name
  time.stamp <- proc.time.stamp$time.stamp

  list(file.name = file.name, time.stamp = time.stamp, uuid = uuid,
      extension = extension, compression = compression)

}

process_compression <- function(file.name = NULL, compression = 'auto') {

  if (!is.null(names(compression))) {
    compression.ext <-  unname(compression)
    compression <- names(compression)
  } else {
    compression.ext <- compression
  }

  if (is.null(compression) || is.na(compression)) {
    return(list(file.name = file.name, compression = NULL))
  }

  if (is.null(file.name) && compression == 'auto') {
    return(list(file.name = file.name, compression = NULL))
  }

  if (compression == 'auto') {
    avail.comp <- paste(kAvailCompression, collapse = '|')
    pattern <- paste0('^.+\\.(', avail.comp, ')$')

    if (grepl(pattern, file.name)) {
      compression <- gsub(pattern, '\\1', file.name)
      compression.ext <- compression

    } else {
      compression <- NULL

    }
  }

  if (!is.null(file.name)) {
    file.name <- gsub(paste0('^(.+)\\.', compression.ext, '$'), '\\1', file.name)
  }

  list(file.name = file.name, compression = compression)
}

process_extension <- function(file.name = NULL, extension = 'auto') {

  if (!is.null(names(extension))) {
    extension.ext <-  unname(extension)
    extension <- names(extension)
  } else {
    extension.ext <- extension
  }

  if (is.null(extension) || is.na(extension)) {
    return(list(file.name = file.name, extension = NULL))
  }

  if (is.null(file.name) && extension == 'auto') {
    return(list(file.name = file.name, extension = NULL))
  }

  if (extension == 'auto') {
    pattern <- '^.+\\.([a-zA-Z0-9]+)$'
    if (grepl(pattern, file.name)) {
      extension <- gsub(pattern, '\\1', file.name)
      extension.ext <- extension

    } else {
      extension <- NULL

    }
  }

  if (!is.null(file.name)) {
    file.name <- gsub(paste0('^(.+)\\.', extension.ext, '$'), '\\1', file.name)
  }

  list(file.name = file.name, extension = extension)
}

process_uuid <- function(file.name = NULL, uuid = 'auto') {

  if (!(!is.na(uuid) || is.logical(uuid) || uuid == 'auto')) {
    stop('uuid must be TRUE, FALSE, or "auto"')
  }

  if (is.null(file.name)) {
    if (uuid == 'auto') {
      uuid <- FALSE
    }
    return(list(file.name = file.name, uuid = uuid))
  }

  uuid.pattern <- paste0('_?', gen_uuid_regx(FALSE), '_?')

  if (uuid == 'auto') {
    uuid <- grepl(uuid.pattern, file.name)
  }

  file.name <- gsub(uuid.pattern, '', file.name)

  list(file.name = file.name, uuid = uuid)
}

process_timestamp <- function(file.name = NULL, time.stamp = 'auto') {

  if (!(!is.na(time.stamp) || is.logical(time.stamp) || time.stamp == 'auto')) {
    stop('time.stamp must be TRUE, FALSE, or "auto"')
  }

  if (is.null(file.name)) {
    if (time.stamp == 'auto') {
      time.stamp <- FALSE
    }
    return(list(file.name = file.name, time.stamp = time.stamp))
  }

  timestamp.pattern <- paste0('_?',
      generate_timestamp_pattern(kDefaultDigitsSecs), '_?')

  if (time.stamp == 'auto') {
    time.stamp <- grepl(timestamp.pattern, file.name)
  }

  file.name <- gsub(timestamp.pattern, '', file.name)

  list(file.name = file.name, time.stamp = time.stamp)
}


## aws.s3 Helpers =======================================================
put_object_wrapper <- function(from, to, headers = list(), ...) {
  args <- list(...)
  new.args <- c(
    file = normalizePath(get_filepath(from)),
    object = get_filepath(to),
    bucket = get_container(to),
    key = attr(to, 'access.key.id'),
    secret = attr(to, 'secret.access.key'),
    region = attr(to, 'region'),
    headers = headers,
    args
  )
  new.args <- as.list(new.args)
  new.args$parse_response <- FALSE

  ## put_object is not closing it's connection to the file. Need to find
  ## the connection and manually close it in order to prevent warning messages
  put.result <- do.call(put_object, new.args)

  all.open.cons <- data.frame(showConnections())
  if (nrow(all.open.cons) > 0) {
    bad.open.con <- row.names(
        all.open.cons[all.open.cons$description == file, ])
    if (length(bad.open.con) > 0) {
      close(getConnection(bad.open.con))
    }
  }
  return(put.result)
}
