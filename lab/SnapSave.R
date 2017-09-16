#==============================================================================#
#                                   SnapSave                                   #
#==============================================================================#
#' SnapSave
#'
#' \code{SnapSave} Class for saving snapshots of objects
#'
#' \strong{SnapSave Class Overview:}
#'
#' The SnapSave class manages the process of archiving and restoring objects
#' in the NLPStudio.
#'
#' \strong{Snap Family of Classes Participants:}
#' \itemize{
#'  \item Snap0: This class which manages and reports the saving and restoration of snapshots.
#'  \item SnapSave: Class which saves the snapshot to disk.
#'  \item SnapRestore: Class which restores a snapshot from disk.
#'  }
#'
#'
#' \strong{SnapSave Methods:}
#' \itemize{
#'  \item{\code{save(object)}}{Method that saves the snapshot of an object. A directory is created containing two compressed files: (1), the serialized object, and (2), the files in the object's directory.}
#' }
#'
#' @param snapName Character string indicating the unique name for the snapshot.
#' @param object Object of the "Corpus", "Document", "DocumentCollection", or "Lab" class to be archived.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Snapshot classes
#' @export
SnapSave <- R6::R6Class(
  classname = "SnapSave",
  public = list(
    save = function(name, object, snapshotDirectory) {

      # Validate parameter
      if (missing(name)) {
        v <- Validate0$new()
        v$notify(class = "SnapSave", method = "save",
                 fieldName = "name", value = "", level = "Error",
                 msg = paste("Snapshot name is missing with no default.",
                             "See ?SnapSave for further assistance."),
                 expect = NULL)
        stop()
      }


      if (missing(object)) {
        v <- Validate0$new()
        v$notify(class = "SnapSave", method = "save",
                 fieldName = "object", value = "", level = "Error",
                 msg = paste("Object is missing with no default.",
                             "See ?SnapSave for further assistance."),
                 expect = NULL)
        stop()
      }

      if (missing(snapshotDirectory)) {
        v <- Validate0$new()
        v$notify(class = "SnapSave", method = "save",
                 fieldName = "snapshotDirectory", value = "", level = "Error",
                 msg = paste("Snapshot directory is missing with no default.",
                             "See ?SnapSave for further assistance."),
                 expect = NULL)
        stop()
      }

      if (!dir.exists(snapshotDirectory)) {
        v <- Validate0$new()
        v$notify(class = "SnapSave", method = "save",
                 fieldName = "snapshotDirectory", value = snapshotDirectory, level = "Error",
                 msg = paste("Snapshot directory does not exist.",
                             "See ?SnapSave for further assistance."),
                 expect = NULL)
        stop()
      }

      v <- ValidateExists$new()
      if (v$validate(class = "SnapSave", method = "save",
                     fieldName = "object", value = object, level = "Error",
                     msg = paste("Unable to take snapshot of object. Object does not exist.",
                                 "Please see ?SnapSave for further assistance."),
                     expect = TRUE) == FALSE) {
        stop()
      }

      # Create Directory for Snapshot files
      snapDir <- file.path(snapshotDirectory, name)
      dir.create(snapDir)

      # Save object
      snapFileName <- file.path(snapDir,paste0(name, "-object"))
      save(snapFileName, file = object)

      # Compress and save files
      o <- object$getObject()

      snapFilesName <- file.path(snapDir, paste0(name, "-files"))

      snapFiles <- list.files(path = o$path, all.files = TRUE, full.names = TRUE,
                          recursive = TRUE, include.dirs = TRUE)

      if (length(snapFiles) != 0) {
        zip(snapFilesName, snapFiles)
      }
    }
  )
)
