#' rstash: A package for abstracting away where your data is being stored,
#'    giving a consistent means of accessing and moving your data whether it is
#'    on your local file system or in a cloud based object storage system.
#'
#' The rstash package provides two main categories of functions:
#' Data movement and Informational
#'
#' @section Data movement functions:
#' save_stash
#' get_stash
#' delete_stash
#' copy_stash
#' move_stash
#'
#' @section Informational functions:
#' stash_match
#' stash_content
#'
#' @import plyr
#' @import aws.s3
#' @import uuid
#' @import xml2
#'
#' @docType package
#' @name rstash
NULL


#' Object for representing an AWS S3 file or directory
#'
#' @param bucket AWS S3 bucket
#' @param path A "file path" to a AWS S3 directory or file. S3 doesn't
#'    technically have folders/directories, but the object keys can be
#'    interpreted that way.
#'
#' @return A s3_stash object.
#' @export
s3_stash <- function(bucket, path = '') {
  structure(path, class = 's3_stash', bucket = bucket)
}

#' Object for representing a local file or directory
#'
#' @param path A file path to a local directory or file.
#'
#' @return A local_stash object.
#' @export
local_stash <- function(path = '') {
  structure(path, class = 'local_stash')
}
