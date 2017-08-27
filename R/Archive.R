## ---- Archive
#==============================================================================#
#                                 Archive                                     #
#==============================================================================#
#' Archive
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
#'  \item{\code{new()}}{Creates an object of class Archive}
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
Archive <- R6::R6Class(
  classname = "Archive",

  public = list(
    archive = function(object) {

      c <- class(brown)[1]
      d <- getDocument(format = "list")

      dirs <- nlpStudio$getDirectories()

      archiveFile <- file.path(dirs$archives,
                               paste0(sub('\\..*', '',c), "-object-",
                                      "-archive-", d$name,
                            format(Sys.time(),'-%Y%m%d-%H%M%S')))

      files <- list.files(path = d$path, all.files = TRUE, full.names = TRUE,
                          recursive = TRUE, include.dirs = TRUE)
      zip(archiveFile, files)
    }
  )
)
