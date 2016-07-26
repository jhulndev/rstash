#' @export
file_path <- function(..., inherit.dir = NULL, inherit.file = TRUE,
    as.stash = TRUE, simplify = TRUE) {

  ex.args <- expand.arguments(...)
  arranged.args <- arrange.arguments(ex.args)

  exploded.paths <- llply(arranged.args, explode_path, with.file = inherit.file)
  assembled.paths <- llply(exploded.paths,
      function(x) do.call(file.path, as.list(x)))

  if (!as.stash) {
    return(unlist(assembled.paths))
  }

  if (!is.null(inherit.dir)) {
    res <- mapply(convert_stash, path = assembled.paths, args = arranged.args,
        inherit.dir = inherit.dir, inherit.file = inherit.file, SIMPLIFY = FALSE)
  } else {
    res <- mapply(convert_stash, path = assembled.paths, args = arranged.args,
        inherit.file = inherit.file, SIMPLIFY = FALSE)
  }

  res <- as.flat_list(res)
  if (simplify && length(res) == 1) {
    return(res[[1]])
  }
  return(res)
}

convert_stash <- function(path, args, inherit.dir = NULL, inherit.file = TRUE) {

  extract.class <- extract_stash_class(args, inherit.pos = inherit.dir)
  if (is.na(extract.class)) {
    return(as.local_stash(path))
  }

  extract.class.fn <- paste0('as.', names(extract.class))
  stash.args <- list(x = args[[extract.class]], path = path)
  stash.dir <- do.call(extract.class.fn, stash.args)

  if (!inherit.file) {
    return(stash.dir)
  }

  extract.file.class <- extract_stash_class(args, inherit.pos = length(args))
  if (is.na(extract.file.class)) {
    return(stash.dir)
  }

  attrs <- attributes(args[[extract.file.class]])
  if (attrs$type != 'file') {
    return(stash.dir)
  }

  file.args <- c(attrs$args['time.stamp'], attrs$args['uuid'],
    attrs$args['extension'], attrs$args['compression'], attrs$args['base.file'],
    attrs$args['is.file'])

  file.args$x <- stash.dir
  stash.file <- do.call(as.stash, file.args)

  return(stash.file)
}

extract_stash_class <- function(x, inherit.pos = NULL) {
  x.classes <- laply(x, class)

  if (!is.null(inherit.pos)) {
    inherit.pos <- as.integer(inherit.pos)
    if (is.na(inherit.pos) || inherit.pos < 1 || inherit.pos > length(x)) {
      warning('inherit.from will be ignored since it is not valid. ',
          'Must be an integer or NULL.')
    } else {
      output.stash.pos <- inherit.pos
    }
  } else {
    stash.found <- grepl('^.+_stash$', x.classes)
    if (any(stash.found)) {
      output.stash.pos <- which(stash.found)[[1]]
    } else {
      output.stash.pos <- 1
    }
  }

  x.class <- x.classes[[output.stash.pos]]
  if (!grepl('^.+_stash$', x.class)) {
    return(NA)
  }
  names(output.stash.pos) <- x.class
  return(output.stash.pos)

}


explode_path <- function(x, with.file = TRUE) {
  x.len <- length(x)
  y <- llply(x[-x.len], explode_path_, with.file = FALSE)
  z <- explode_path_(x[[x.len]], with.file = with.file)
  c(unlist(y), z)
}

explode_path_ <- function(x, with.file = FALSE) {
  UseMethod('explode_path_')
}

#' @export
explode_path_.character <- function(x, with.file) {
  return(x)
}

#' @export
explode_path_.numeric <- function(x, with.file) {
  return(as.character(x))
}

#' @export
explode_path_.local_stash <- function(x, with.file) {
  if (with.file) {
    file <- attr(x, 'file')
  } else {
    file <- NULL
  }
  c(attr(x, 'directory'), file)
}

#' @export
explode_path_.ftp_stash <- function(x, with.file) {
  if (with.file) {
    file <- attr(x, 'file')
  } else {
    file <- NULL
  }
  c(attr(x, 'directory'), file)
}

#' @export
explode_path_.s3_stash <- function(x, with.file) {
  if (with.file) {
    file <- attr(x, 'file')
  } else {
    file <- NULL
  }
  c(attr(x, 'directory'), file)
}
