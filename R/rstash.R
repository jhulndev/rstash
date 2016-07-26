#' rstash: A package for abstracting away where your data is being stored,
#'    giving a consistent means of accessing and moving your data whether it is
#'    on your local file system or in a cloud based object storage system.
#'
#' A stash refers to both the storage location and the objects being stored.
#' When defining a stash, at least a location needs to be defined, which will
#' default to the working directory for a local_stash. Defining the object is
#' optional. If it is not defined, then this stash would only refer to the
#' location.
#'
#' When a stash is passed into an actor such as get_stash or save_stash, the
#' defined values in the *_stash function will be used, if not provided, then
#' the stash's values will be used, if not provided, then the default of the
#' *_stash will be used.
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
#' @import xml2
#' @import RCurl
#'
#' @docType package
#' @name rstash
NULL


kAvailCompression <- c('gz')
kDefaultDigitsSecs <- 2
