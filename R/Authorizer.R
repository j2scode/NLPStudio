#==============================================================================#
#                                  Authorizer                                  #
#==============================================================================#
#' Authorizer
#'
#' \code{Authorizer} Class which maintains the authentication and access
#' policy and authorizes activities.
#'
#' \strong{Authorizer Methods:}
#' \describe{
#'  \item{\code{new()}}{Instantiates the authorizer.}
#'  \item{\code{addClass(class, attributes)}}{Adds a class and its attributes to the list of valid classes and attributes}#'
#'  \item{\code{removeClass(class)}}{Removes a class from the Authorizer.}
#'  \item{\code{addAttribute(class, attribute)}}{Adds an attribute to a class.}
#'  \item{\code{removeAttribute(class)}}{Removes an attribute from a class .}
#'  \item{\code{addPolicy(subject, object, attribute, type)}}{Creates an authorization policy.}
#'  \item{\code{removePolicy(subject, object = null, attribute = null)}}{Removes a policy based upon the parameters presented. The parameters are validated from left to right. e.g. if attribute is sent, the object can not be null.}
#'  \item{\code{authorize(subject, object, attribute, type)}}{Returns true if the subject is authorized to perform the requested type of action on the attribute.}
#' }
#'
#' \strong{Authorizer Parameters:}
#' @param class Character string containing the name of valid class in the NLPStudio package.
#' @param attributes Vector containing the names of the attributes of the class.
#' @param subject Character string containing the name of the active class requesting access.
#' @param object Character string containing the name of the class to which access is being requested.
#' @param attribute Character string containing the name of the class's attribute to which access is being requested.
#' @param type Character representing the type of access being requested. Valid types are "r" for read and "w" for write access.
#'
#' @return A logical, TRUE if authorization is granted, FALSE otherwise.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @docType class
#' @family Validation Classes
#' @export
Authorizer <- R6::R6Class(
  "SingletonContainer",
  portable = FALSE,
  inherit = Singleton,
  public = list(
    initialize = function(...) {
      Class <<- R6::R6Class(
        classname = "Authorizer",
        private = list(
          ..method = character(0),
          ..model = list(), # List of classes and attributes
          ..policy = data.frame(),
          ..file = character(0)
        ),

        validateClass = function(class) {
          if (exists(private$..model[[class]])) {
            return(TRUE)
          } else {
            v <- Validator0$new()
            v$notify(class = class(self)[1], method = private$..method, fieldName = "subject/object",
                     value = atttribute, level = "Error",
                     msg = paste0("Invalid class.",
                                  "See ?", class(self)[1], " for further assistance."),
                     expect = NULL)
            stop()
          }
        },

        validateAttribute = function(class, attribute) {
          if (exists(private$..model[[class]])) {
            a <- private$..model[[class]]
            if (attribute %in% a) {
              return(TRUE)
            } else {
              v <- Validator0$new()
              v$notify(class = class(self)[1], method = private$..method, fieldName = "attribute",
                       value = atttribute, level = "Error",
                       msg = paste0("Invalid class attribute.",
                                    "See ?", class(self)[1], " for further assistance."),
                       expect = NULL)
              stop()
            }
          } else {
            return(FALSE)
          }
        },

        validateType = function(type) {
          if (type %in% c("r", "w", "R", "W")) {
            return(TRUE)
          } else {
            v <- Validator0$new()
            v$notify(class = class(self)[1], method = private$..method, fieldName = "type",
                     value = atttribute, level = "Error",
                     msg = paste0("Invalid access type.",
                                  "See ?", class(self)[1], " for further assistance."),
                     expect = NULL)
            stop()
          }
        },

        public = list(

          initialize = function() {

            # Get policy file information
            c <- Constants$new()
            private$..file <- c$getPolicyFile()

            # Read policy file, if it exists
            if (exists(private$..file)) {
              authPackage <- readRDS(file = private$..file)
              private$..model <- authPackage$model
              private$..policy <- authPackage$policy
            }
          },

          getInstance = function() {
            invisible(self)
          },

          addClass = function(class, attributes) {
            private$..model[[class]] <- attributes
            authPackage <- list(
              model = private$..model,
              policy = private$..policy
            )
            saveRDS(authPackage, private$..file)
          },

          removeClass = function(class) {
            private$..model[[class]] <- NULL
            authPackage <- list(
              model = private$..model,
              policy = private$..policy
            )
            saveRDS(authPackage, private$..file)
          },

          addAttribute = function(class, attribute) {
            a <- private$..model[[class]]
            a <- c(a, attribute)
            private$..model[[class]] <- a
            authPackage <- list(
              model = private$..model,
              policy = private$..policy
            )
            saveRDS(authPackage, private$..file)
          },

          removeAttribute = function(class, attribute) {
            a <- private$..model[[class]]
            a <- a[!a %in% attribute]
            private$..model[[class]] <- a
            authPackage <- list(
              model = private$..model,
              policy = private$..policy
            )
            saveRDS(authPackage, private$..file)
          },

          addPolicy = function(subject, object, attribute, type) {
            private$..method <- "addPolicy"
            if (private$validateClass(subject) & private$..validateClass(object) &
                private$validateAttribute(attribute) &
                private$validateType(type)) {
                  policy <- data.frame(subject == subject,
                                       object == object,
                                       attribute == attribute,
                                       type == type)
                  private$..policy <- rbind(private$..policy, policy)
                  authPackage <- list(
                    model = private$..model,
                    policy = private$..policy
                  )
                  saveRDS(authPackage, private$..file)
            } else {
              return(FALSE)
            }
          },

          removePolicy = function(subject, object, attribute, type) {
            private$..method <- "removePolicy"
            if (nrow(subset(private$..policy, subject == subject &
                            object == object & attribute == attribute &
                            type == type)) == 0) {
              v <- Validator0$new()
              v$notify(class = class(self)[1], method = private$..method, fieldName = "all",
                       value = "", level = "Warn",
                       msg = paste0("Policy not found.",
                                    "See ?", class(self)[1], " for further assistance."),
                       expect = NULL)

            } else {
              private$..policy <- private$..policy[!(subject == subject &
                                                       object == object &
                                                       attribute == attribute &
                                                       type == type)]
              authPackage <- list(
                model = private$..model,
                policy = private$..policy
              )
              saveRDS(authPackage, private$..file)
              return(TRUE)
            }
          },

          authorize = function(subject, object, attribute, type) {
            if (exists(subset(private$..policy,
                              subject == subject &
                              object == object &
                              type == type))) {
              return(TRUE)
            } else {
              return(FALSE)
            }
          },

          loadPolicy = function() {
            authPackage <- readRDS(file = private$..file)
            private$..model <- authPackage$model
            private$..policy <- authPackage$policy
          }
        )
      )
      super$initialize(...)
    }
  ),lock_object = FALSE
)#$new()
