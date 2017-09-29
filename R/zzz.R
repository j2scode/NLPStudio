.onLoad <- function(libname, pkgname) {

  # Get Constants Object
  c <- Constants$new()

  # Greet User
  h <- c$getHistoryFile()
  if (file.exists(h)) {
    packageStartupMessage(paste0("#=========================================================================================#"))
    packageStartupMessage(paste0("#                         Welcome back to the NLPStudio (Beta)!                           #"))
    packageStartupMessage(paste0("#=========================================================================================#"))
  } else {
    packageStartupMessage(paste0("#=========================================================================================#"))
    packageStartupMessage(paste0("#                                                                                         #"))
    packageStartupMessage(paste0("#                           Welcome to the NLPStudio (Beta)!                              #"))
    packageStartupMessage(paste0("#                                                                                         #"))
    packageStartupMessage(paste0("# Thank you for installing the NLPStudio package. To get started, a sample R script,      #"))
    packageStartupMessage(paste0("# 'myNLP.R' has been provided to illustrate some of the basic method calls for creating   #"))
    packageStartupMessage(paste0("# your first lab, sourcing a document collection and performing some basic analysis.      #"))
    packageStartupMessage(paste0("# Vignettes are also available at https://www.DataScienceSalon.org/NLPStudio. Thanks for  #"))
    packageStartupMessage(paste0("# for exploring NLPStudio.                                                                #"))
    packageStartupMessage(paste0("#                                       Data Science Salon                                #"))
    packageStartupMessage(paste0("#                                       https://www.DataScienceSalon.org/NLPStudio        #"))
    packageStartupMessage(paste0("#                                                                                         #"))
    packageStartupMessage(paste0("#=========================================================================================#"))
  }
  # Instantiate Singleton Classes
  historian <<- Historian$new()$getInstance()
  nlpStudio <<- NLPStudio$new()$getInstance()
  stateManager <<- StateManager$new()$getInstance()
}

.onAttach <- function(libname, pkgname) {
  c <- Constants$new()
  logPath <- c$getLogPath()
  if (!dir.exists(logPath)) {
    dir.create(logPath)
  }
  futile.logger::flog.threshold(INFO)
  futile.logger::flog.logger("green", INFO, appender=appender.file(file.path(logPath, "green.log")))
  futile.logger::flog.logger("yellow", WARN, appender=appender.tee(file.path(logPath, "yellow.log")))
  futile.logger::flog.logger("red", ERROR, appender=appender.tee(file.path(logPath, "red.log")))

  futile.logger::flog.info("Welcome to the NLPStudio package", name = 'green')
}
