#' rstash: A package for...
#'
#' The foo package provides three categories of important functions:
#' foo, bar and baz.
#'
#' @section Foo functions:
#' The foo functions ...
#'
#'
#' @import plyr
#' @import aws.s3
#' @import uuid
#'
#' @docType package
#' @name rstash
NULL




#' Title
#'
#' @param bucket s
#' @param path s
#'
#' @return a
#' @export
s3_stash <- function(bucket, path = '') {
  structure(path, class = 's3_stash', bucket = bucket)
}

#' Title
#'
#' @param path s
#'
#' @return s
#' @export
local_stash <- function(path = '') {
  structure(path, class = 'local_stash')
}
