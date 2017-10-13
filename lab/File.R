#==============================================================================#
#                                     File0                                    #
#==============================================================================#
#' File0
#'
#' \code{File0} Abstract class for the File Family of classes. This class defines
#' the interface for the FileCsv, FileRdata, and FileText classes.
#'
#' \strong{File Methods:}
#' There are three types of methods within the File classes and they are:
#' \itemize{
#'  \item{Core Methods: Core methods for obtaining object information, and moving, copying, and deleting files on disk.}
#'  \item{Structural Methods: Methods to maintain the relationship with objects of the Document class.}
#'  \item{Visitor Methods: Methods for implementation of and messaging with objects of the visitor classes.}
#' }
#'
#' \strong{File Core Methods:}
#'  \itemize{
#'   \item{\code{new(name, fileName, desc = NULL)}}{Base method for instantiating File objects.
#'   Specific behaviors implemented in the subclasses. }
#'   \item{\code{getObject()}}{Base method for obtaining file object information.
#'   Specific behaviors implemented in the subclasses. }
#'   \item{\code{moveFile()}}{Base method for moving a file. This is invoked by
#'   the Document class when there is a change in the composite hierarchy. }
#'   \item{\code{copyFile()}}{Base method for copying a file.
#'   Specific behaviors implemented in the subclasses. }
#'   \item{\code{deleteFile()}}{Base method for deleting a file from disk.
#'   Specific behaviors implemented in the subclasses. }
#' }
#'
#'#' \strong{File Structural Methods:}
#'  \itemize{
#'   \item{\code{getAncestor()}}{Base method for retrieving the Document class object to which the file is associated.
#'   Specific behaviors implemented in the subclasses. }
#'   \item{\code{setAncestor()}}{Base method for setting the Document class object to which the file is associated.
#'   Specific behaviors implemented in the subclasses. }
#' }
#'
#' \strong{File Visitor Methods:}
#'  \itemize{
#'   \item{\code{acceptVReader(visitor)}}{Method for accepting the VReader visitor method. Subclasses override these methods.}
#'   \item{\code{acceptVWriter(visitor)}}{Method for accepting the VWriter visitor method. Subclasses override these methods.}
#'   \item{\code{acceptVWriteState(visitor)}}{Method for accepting the VWriteState visitor method. Subclasses override these methods.}
#'   \item{\code{acceptVReadState(visitor)}}{Method for accepting the VReadState visitor method. Subclasses override these methods.}
#'  }
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
File <- R6::R6Class(
  classname = "File",
  lock_objects = FALSE,
  private = list(
    ..name = character(0),
    ..desc = character(0),
    ..parent = character(0),
    ..path = character(0),
    ..fileName = character(0),
    ..content = character(0),
    ..modified = "None",
    ..created = "None"
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                              Core Methods                               #
    #-------------------------------------------------------------------------#
    initialize = function(name, fileName, desc = NULL) stop("This method is not implemented for this class"),

    getObject = function() {
      file <- list(
        name = private$..name,
        desc = private$..desc,
        parent = private$..parent,
        path = private$..parent$getPath(),
        fileName = private$..fileName,
        content = private$..content,
        modified = private$..modified,
        created = private$..created
      )
      return(file)
    },

    moveFile = function() {
      currentPath <- file.path(private$..path, private$..fileName)
      newPath <- file.path(private$..parent$getPath(), private$..fileName)
      file.rename(currentPath, newPath)
      class <- class(self)[1]
      historian$addEvent(cls = class, objectName = private$..name,
                         method = "moveFile",
                         event = paste("Moved file from", currentPath, "to",
                                       newPath))

    },

    copyFile = function(path) {

      currentPath <- file.path(private$..path, private$..fileName)
      newPath <- file.path(path, private$..fileName)
      file.copy(currentPath, newPath)
      class <- class(self)[1]
      historian$addEvent(cls = class, objectName = private$..name,
                         method = "copyFile",
                         event = paste("Copied file at", currentPath, "to",
                                       newPath))
    },

    deleteFile = function() {
      stateManager$setState(self)
      base::unlink(file.path(private$..path, private$..fileName))
    },

    #-------------------------------------------------------------------------#
    #                              Structural Methods                         #
    #-------------------------------------------------------------------------#
    getAncestor = function() private$..parent,

    setAncestor = function(parent) {

      v <- ValidatorClass$new()
      if (v$validate(class = class(self)[1], method = "setAncestor", fieldName = "class(parent)",
                     level = "Error", value = class(parent)[1],
                     msg = paste("Unable to set parent.  Parent must be a",
                                 "Document object.",
                                 "See ?Document for assistance."),
                     expect = "Document") == FALSE) {
        stop()
      }

      # Get parent information
      p <- parent$getObject()

      # Set parent variable
      private$..parent <- parent

      # Move files
      self$moveFiles()

      historian$addEvent(cls = class(self)[1], objectName = private$..name,
                         method = "setAncestor",
                         event = paste("Set ancestor of,",
                                       private$..name, "to",
                                       p$name))
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    acceptVReader = function(visitor)  stop("This method is not implemented for this class"),
    acceptVWriter = function(visitor)  stop("This method is not implemented for this class"),
    acceptVWriteState = function(visitor)  stop("This method is not implemented for this class"),
    acceptVReadState = function(visitor)  stop("This method is not implemented for this class")
  )
)
