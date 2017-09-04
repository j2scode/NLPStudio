#==============================================================================#
#                                     Snap0                                    #
#==============================================================================#
#' Snap0
#'
#' \code{Snap0} Manages the process of saving and restoring snapshots of
#' NLPStudio objects.
#'
#' \strong{Snap0 Class Overview:}
#'
#' The Snap0 singleton class defines the interface for saving and restoring snapshots
#' of NLPStudio objects. Its single object delegates the actual save and restore
#' operations to the SnapSave and SnapRestore classes, respectively. Once the
#' request has been fulfilled by the subclasses, the request is logged.
#'
#' \strong{Snap0 Class Collaborators:}
#' The collaborators of the Snap0 class are:
#' \itemize{
#'  \item Lab: Class in which NLP takes place. The class contains objects of the DocumentCollection class.
#'  \item DocumentCollection: Composite class containing objects of the Document class.
#'  \item Document: Leaf class of the Document objects.
#'  }
#'
#' \strong{Snap Family of Classes Participants:}
#' \itemize{
#'  \item Snap0: This class which manages and reports the saving and restoration of snapshots.
#'  \item SnapSave: Class which saves the snapshot to disk.
#'  \item SnapRestore: Class which restores a snapshot from disk.
#'  }
#'
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{Instantiates the Snap0 object. This singleton object is instantiated by the NLPStudio object at package load time.}
#'  \item{\code{saveSnap(object)}}{Instantiates a SnapSave object to carry out the client's save request.}
#'  \item{\code{restoreSnap(snapName)}}{Instantiates a SnapRestore object to carry out the client's restore request. The snapshotName variable identifies uniquely the specific snapshot to restore.}
#'  \item{\code{getSnaps(object)}}{Returns the list of snapshots for an object, list, or data frame formats. The objectName variable identifies the object for which snapshots should be reported.}
#'  \item{\code{printSnaps(object)}}{Prints a data frame of snapshots for a named object to the console. The objectName variable identifies the object for which snapshots should be reported.}
#' }
#'
#' @param object An NLPStudio object to be saved
#' @param snapName A character string containing the unique name / identifier for a particular snapshot of an object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Snapshot classes
#' @export
Snap0 <- R6::R6Class(
  "SingletonContainer",
  portable = FALSE,
  inherit = Singleton,
  public = list(
    initialize = function(...) {
      Class <<- R6::R6Class(
        classname = "Snap0",
        private = list(
          ..name = "snap0",
          ..desc = "Snap0 NLPStudio Object Snapshot Manager",
          ..snaps = data.frame(),
          ..created = character(0),
          ..modified = character(0),

          getSeqNum = function(name) {
            seqNum <- 1
            if (nrow(subset(private$..snaps,
                            objectName  = name)) > 0) {
              seqNum <- private$..snaps %>% filter(objectName == name) %>%
                summarise(max(seqNum))
              seqNum <- seqNum + 1
            }
            return(seqNum)
          }
        ),

        public = list(

          initialize = function() {
            private$..seqNum <- 0
            private$..created <- Sys.time()
            private$..modified <- Sys.time()
            invisible(self)
          },

          getInstance = function() invisible(self),

          saveSnap = function(object) {

            # Confirm object parameter is not missing
            if (missing(object)) {
              v <- Validate0$new()
              v$notify(cls = "Snap0", method = "saveSnapshot",
                       fieldName = "object", value = "", level = "Error",
                       msg = paste("Object is missing with no default.",
                                   "See ?Snap0 for further assistance."),
                       expect = TRUE)
              stop()
            }

            # Format key variables
            objectName <- object$getName()
            seqNum <- private$getSeqNum(objectName)
            snapName <- paste0(objectName,"-", as.Date(Sys.time()),"-", seqNum)
            requested <- Sys.time()

            # Execute
            snapSave <- SnapSave$save(snapName, object)

            # Wrap up
            snap <- data.frame(objectName = objectName,
                               snapName = snapName,
                               request = "Save",
                               seqNum = seqNum,
                               requested = requested,
                               completed = Sys.time(),
                               stringsAsFactors = FALSE)
            private$..snaps <- rbind(private$..snaps, snap)
          }

        )
      )
      super$initialize(...)
    }
  ),lock_objects = FALSE
)#$new()
