## ---- Archive0
#==============================================================================#
#                                 Archive0                                     #
#==============================================================================#
#' Archive0
#'
#' \code{Archive} Abstract class for archiving objects in the NLPStudio
#'
#' This abstract class defines the methods used by the concrete classes for
#' archiving objects in the NLPStudio
#'
#' @docType class
#' @examples
#' \dontrun{
#' a <- archive$new()
#' a$archive0("development")
#' }
#'
#' @section Method:
#' \describe{
#'  \item{\code{new()}}{Creates an object of class Archive0}
#'  \item{\code{archive(object)}}{Archives the object in the NLPStudio archive subdirectory }
#' }
#'
#' @section Parameters:
#' @param object
#' \describe{
#'  \item{object$name}{Character string with name of the object}
#'  \item{object$desc}{Character string with description of the object e.g. "development-environment-archive"}
#'  \item{object$path}{Character string containing the directory containing the object}
#' }
#'
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @export
Archive0 <- R6::R6Class(
  classname = "Archive0",

  public = list(
    archive = function(object) {

      archDir <- nlpStudio$studioDirs$archive

      archiveFile <- file.path(archDir, paste0(sub('\\..*', '',object$desc),
                            format(Sys.time(),'-%Y%m%d-%H%M%S'),".zip"))

      files <- list.files(path = file.path(object$path), all.files = TRUE,
                          recursive = TRUE, include.dirs = TRUE)
      zip(archiveFile, files)
    }
  )
)
