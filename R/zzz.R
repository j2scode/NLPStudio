.onLoad <- function(libname, pkgname) {

  # Initialize Logger
  # TODO: This is for development only, remove before distributing package
  if (!dir.exists("./log")) {
    dir.create("./log")
    futile.logger::flog.threshold(INFO)
    futile.logger::flog.logger("green", INFO, appender=appender.file('./log/green.log'))
    futile.logger::flog.logger("green", Info, appender=appender.file('./log/green.log'))
    futile.logger::flog.logger("green", info, appender=appender.file('./log/green.log'))
    futile.logger::flog.logger("yellow", WARN, appender=appender.tee('./log/yellow.log'))
    futile.logger::flog.logger("yellow", Warn, appender=appender.tee('./log/yellow.log'))
    futile.logger::flog.logger("yellow", warn, appender=appender.tee('./log/yellow.log'))
    futile.logger::flog.logger("red", ERROR, appender=appender.tee('./log/red.log'))
    futile.logger::flog.logger("red", Error, appender=appender.tee('./log/red.log'))
    futile.logger::flog.logger("red", error, appender=appender.tee('./log/red.log'))

    futile.logger::flog.info("Welcome to the NLPStudio package", name = 'green')
  }

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
