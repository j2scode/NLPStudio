.onLoad <- function(libname, pkgname) {

  # Instantiate Singleton Classes
  if (file.exists("./.StudioCache.Rdata")) {
    packageStartupMessage("\nWelcome back to the NLPStudio (Beta)!\n\n")
    nlpStudioCache <<- StudioCache$new()$getInstance()
    nlpStudio <<- NLPStudio$new()$getInstance()
    nlpStudioCache$loadCache()
    nlpStudioCache$restoreCache()
  } else {
    packageStartupMessage("\nWelcome to the NLPStudio (Beta)!")
    nlpStudioCache <<- StudioCache$new()$getInstance()
    nlpStudio <<- NLPStudio$new()$getInstance()
    nlpStudioCache$setCache("nlpStudio", nlpStudio)
  }
}
