testNLPStudio <- function() {

  init <- function() {
    if (exists("Development", envir = .GlobalEnv)) rm("Development", envir = .GlobalEnv)
    if (exists("Bart", envir = .GlobalEnv))rm("Bart", envir = .GlobalEnv)
    source("./tests/checkCache.r")
    source("./tests/logTests.r")
  }

  # Test 0: Confirm instantiation of nlpStudio
  test0 <- function() {
    test <- "test0"
    cat(paste("\r",test, " Commencing\r"))

    studio <<- nlpStudio$getStudio()
    stopifnot("NLPStudio" %in% class(nlpStudio))
    stopifnot(studio$name == "nlpStudio")
    stopifnot(studio$desc == "NLPStudio: Natural Language Processing Studio")
    stopifnot(studio$current == "None")
    stopifnot(nrow(studio$labList) == 0)
    stopifnot("POSIXct" %in% class(studio$created))
    stopifnot("POSIXct" %in% class(studio$modified))

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "initiate", note = "Successfully created nlpStudio")
    logTests(cls = cls, mthd = "getStudio", note = "Current = none, no labs")


    cat(paste("\n", test, " Completed: Success!\r"))
  }

  # Test 1: Test addLab, getLabs, listLabs, cache
  test1 <- function() {
    test <- "test1"
    cat(paste("\n\n",test, " Commencing\r"))
    Lab$new(name = "Development", desc = "Development Lab")
    nlpStudio$addLab(Development)
    studio <<- nlpStudio$getStudio()
    stopifnot(difftime(studio$modified, Sys.time(), units = "secs") < 1)

    # #GetStudio
    stopifnot("NLPStudio" %in% class(nlpStudio))
    stopifnot(studio$name == "nlpStudio")
    stopifnot(studio$desc == "NLPStudio: Natural Language Processing Studio")
    stopifnot(all.equal(studio$current,"None"))
    stopifnot("POSIXct" %in% class(studio$created))
    stopifnot("POSIXct" %in% class(studio$modified))

    # AddLab
    stopifnot(nrow(studio$labList) == 1)
    stopifnot(studio$labList$name[1] == "Development")
    stopifnot(studio$labList$desc[1] == "Development Lab")
    stopifnot("POSIXct" %in% class(studio$labList$created[1]))
    stopifnot("POSIXct" %in% class(studio$labList$modified[1]))
    stopifnot(studio$current == "None")

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "addLab", note = "Development lab, current = FALSE")
    logTests(cls = cls, mthd = "addLab", note = "Tested nlpstudio cache")
    logTests(cls = cls, mthd = "listLabs", note = "Listed a single lab successfully")
    logTests(cls = "StudioCache", mthd = "setCache", note = "Updated NLPStudio cache with new lab")
    logTests(cls = "StudioCache", mthd = "getCache", note = "Compared cache to current nlpStudio")
    cat(paste("\n", test, " Completed: Success!\r"))
  }

  # Test 2: Test addLab, getLabs, listLabs, cache 2nd lab current = TRUE
  test2 <- function() {
    test <- "test2"
    cat(paste("\n\n",test, " Commencing\r"))

    Lab$new(name = "Bart",desc = "Simpsons Lab")
    nlpStudio$addLab(Bart, current = TRUE)
    studio <<- nlpStudio$getStudio()
    stopifnot(difftime(studio$modified, Sys.time(), units = "secs") < 1)

    #GetStudio
    stopifnot("NLPStudio" %in% class(nlpStudio))
    stopifnot(studio$name == "nlpStudio")
    stopifnot(studio$desc == "NLPStudio: Natural Language Processing Studio")
    stopifnot("POSIXct" %in% class(studio$created))
    stopifnot("POSIXct" %in% class(studio$modified))

    # AddLab
    stopifnot(nrow(studio$labList) == 2)
    stopifnot(studio$labList$name[1] == "Development")
    stopifnot(studio$labList$desc[1] == "Development Lab")
    stopifnot("POSIXct" %in% class(studio$labList$created[1]))
    stopifnot("POSIXct" %in% class(studio$labList$modified[1]))

    stopifnot(studio$labList$name[2] == "Bart")
    stopifnot(studio$labList$desc[2] == "Simpsons Lab")
    stopifnot("POSIXct" %in% class(studio$labList$created[2]))
    stopifnot("POSIXct" %in% class(studio$labList$modified[2]))
    stopifnot(all.equal(studio$currentLab, Bart))

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "addLab", note = "Added 2nd lab, current = TRUE")
    logTests(cls = cls, mthd = "addLab", note = "Tested nlpstudio cache, match!")
    logTests(cls = "StudioCache", mthd = "setCache", note = "Updated NLPStudio cache with new lab")
    logTests(cls = "StudioCache", mthd = "getCache", note = "Compared cache to current nlpStudio")
    cat(paste("\n", test, " Completed: Success!\r"))
  }

  # Test 3: Test change current lab
  test3 <- function() {
    test <- "test3"
    cat(paste("\n\n",test, " Commencing\r"))

    # Attempt to change to non-existing lab: should fail, Success!
    #nlpStudio$currentLab <- "Bart"

    # Attempt to change to a function: should fail, Success
    #nlpStudio$currentLab <- logTests

    # Attempt to change to current lab: should issue warning
    nlpStudio$currentLab <- Bart

    # Successful change
    nlpStudio$currentLab <- Development
    studio <<- nlpStudio$getStudio(verbose = FALSE)
    stopifnot(identical(Development, nlpStudio$currentLab))
    stopifnot(difftime(studio$modified, Sys.time(), units = "secs") < 1)

    #GetStudio
    stopifnot("NLPStudio" %in% class(nlpStudio))
    stopifnot(studio$name == "nlpStudio")
    stopifnot(studio$desc == "NLPStudio: Natural Language Processing Studio")
    stopifnot("POSIXct" %in% class(studio$created))
    stopifnot("POSIXct" %in% class(studio$modified))

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "currentLab", note = "Validation and successful change, date modified updated")
    logTests(cls = "StudioCache", mthd = "setCache", note = "Updated NLPStudio cache with new current lab")
    logTests(cls = "StudioCache", mthd = "getCache", note = "Compared cache to current nlpStudio")
    cat(paste("\n", test, " Completed: Success!\r"))
  }

  # Test 4: Test remove lab
  test4 <- function() {
    test <- "test4"
    cat(paste("\n\n",test, " Commencing\r"))

    # Attempt to remove a non-existent lab ( a function): Success
    #nlpStudio$removeLab(logTests)

    # Attempt to remove a current lab Success!
    # nlpStudio$removeLab(Development)

    # Successfuly remove Bart
    nlpStudio$removeLab("Bart")
    stopifnot(exists("Bart") == FALSE)
    labs <<- nlpStudio$listlabs()
    labs <<- labs[labs$name == "Bart"]
    stopifnot(nrow(labs) == 0)
    stopifnot(difftime(studio$modified, Sys.time(), units = "secs") < 1)

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "removeLab", note = "Tested validation and removal of lab")
    logTests(cls = "StudioCache", mthd = "setCache", note = "Updated NLPStudio cache with removed lab")
    logTests(cls = "StudioCache", mthd = "getCache", note = "Compared cache to current nlpStudio")
    cat(paste("\n", test, " Completed: Success!\r"))
  }


init()
test0()
test1()
test2()
test3()
test4()
# test5()
# test6()
# test7()
}

cls <- "NLPStudio"
labsDir <- "./Labs"
archiveDir <- "./Archive"
nlpStudioFile <- "./NLPStudio.Rdata"
cacheFile <- "./.StudioCache.Rdata"

base::unlink(labsDir, recursive = FALSE)
base::unlink(archiveDir, recursive = TRUE)
base::unlink(nlpStudioFile)
base::unlink(cacheFile)

devtools::load_all()
testNLPStudio()
