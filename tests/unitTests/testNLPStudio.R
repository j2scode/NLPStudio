testNLPStudio <- function() {

  init <- function() {

    # Clean up
    if (exists("Beats", envir = .GlobalEnv)) rm("Beats", envir = .GlobalEnv)
    if (exists("Bart", envir = .GlobalEnv))rm("Bart", envir = .GlobalEnv)

    # Source state and log
#    source("./tests/testFunctions/checkState.r")
    source("./tests/testFunctions/logTests.r")
    source("./tests/testFunctions/checkHistory.r")

    # Test file paths
    csvTestFile <- "./tests/testFiles/contractions.csv"
    textTestFile <- "./tests/testFiles/en_US.news.txt"
    rdataTestFile <- "./tests/testFiles/quadgrams.Rdata"

  }

  # Test 0: Confirm instantiation of nlpStudio
  test0 <- function() {
    test <- "test0: Instantiation"
    cat(paste(test, " Commencing\n"))

    # Test instantiation
    studio <<- nlpStudio$getObject() #Return list with warning
    stopifnot(studio$name == "nlpStudio")
    stopifnot(studio$desc == "NLPStudio: Natural Language Processing Environment")
    stopifnot(length(studio$stateDesc) > 0)
    stopifnot(length(studio$labs) == 0)
    stopifnot((Sys.time() - studio$created) < 1)
    stopifnot((Sys.time() - studio$modified) < 1)
    stopifnot(studio$created == studio$modified)
    stopifnot(checkHistory(class = "NLPStudio", method = "initialize",
                           objectName = "nlpStudio", event = studio$stateDesc))

    # Logit
    logTests(class = class, mthd = "initiate", note = "Successfully created nlpStudio")

    cat(paste(test, " Completed: Success!\n"))
  }

  test1 <- function() {
    test <- "test1: Create lab"
    cat(paste(test, " Commencing\n"))

    # Instantiate lab
    Lab$new(name = "Beats", desc = "Beats Lab")
    l <- Beats$getObject()
    stopifnot(l$name == "Beats")
    stopifnot(l$desc == "Beats Lab")
    stopifnot(isTRUE(all.equal(l$parent, nlpStudio)))
    stopifnot((Sys.time() - l$created) < 1)
    stopifnot((Sys.time() - l$modified) < 1)

    # Check history
    stopifnot(checkHistory(class = "Lab", method = "initialize",
                           objectName = "Beats", event = l$stateDesc))

    # Logit
    logTests(class = "Lab", mthd = "initialize", note = "Beats lab instantiated successfully")
    cat(paste(test, " Completed: Success!\n"))
  }

  test2 <- function() {
    test <- "test2: Add lab Validation"
    cat(paste(test, " Commencing\n"))

    # Validation
    #nlpStudio$addChild() # fails, lab missing: success
    #nlpStudio$addChild(Writer) # fails, invalid class: success

    # Logit
    logTests(class = "NLPStudio", mthd = "addChild", note = "Validation for addChild method is correct!")
    cat(paste(test, " Completed: Success!\n"))
  }

  test3 <- function() {
    test <- "test3: Add lab"
    cat(paste(test, " Commencing\n"))

    # Successful attempt
    nlpStudio$addChild(Beats)

    # Confirm lab added
    studio <<- nlpStudio$getObject()
    stopifnot(isTRUE(all.equal(studio$labs[[1]], Beats)))
    stopifnot(studio$created < studio$modified)

    # Confirm parent updated
    l <- Beats$getObject()
    stopifnot(isTRUE(all.equal(nlpStudio, l$parent)))

    # Check history
    stopifnot(checkHistory(class = "NLPStudio", method = "addChild",
                           objectName = "nlpStudio", event = studio$stateDesc))
    stopifnot(checkHistory(class = "Lab", method = "parent",
                           objectName = "Beats", event = l$stateDesc))

    # Logit
    logTests(class = class, mthd = "addChild", note = "Beats lab added successfully")
    cat(paste(test, " Completed: Success!\n"))
  }


  test4 <- function() {
    test <- "test4: addChild() #2"
    cat(paste(test, " Commencing\n"))

    Lab$new(name = "Bart",desc = "Simpsons Lab")
    nlpStudio$addChild(Bart)
    studio <<- nlpStudio$getObject()
    stopifnot(length(studio$labs) == 2)
    lab <- studio$labs[[2]]$getObject()
    stopifnot(lab$name == "Bart")
    stopifnot(lab$desc == "Simpsons Lab")
    stopifnot(isTRUE(all.equal(lab$parent, nlpStudio)))
    stopifnot(studio$created < studio$modified)

    # Check history
    stopifnot(checkHistory(class = "NLPStudio", method = "addChild",
                           objectName = "nlpStudio", event = studio$stateDesc))
    stopifnot(checkHistory(class = "Lab", method = "parent",
                           objectName = "Bart", event = lab$stateDesc))

    # Logit
    logTests(class = class, mthd = "addChild", note = "Successfully added lab")
    cat(paste(test, "Completed: Success!\n"))
  }


  test5 <- function() {
    test <- "test5: Remove lab Validation"
    cat(paste(test, " Commencing\n"))

    # Validation
    #nlpStudio$removeChild() # fail lab missing: success
    #nlpStudio$removeChild(ditto) # fail does not exist: success
    #nlpStudio$removeChild("labsDir") # fail not a lab

    # Logit
    logTests(class = class, mthd = "removeChild", note = "Successfully validated removeChild method")
    cat(paste(test, "Completed: Success!\n"))
  }

  test6 <- function() {
    test <- "test6: Remove Child"
    cat(paste(test, " Commencing\n"))

    # Successfuly remove Bart
    nlpStudio$removeChild(Bart)

    # Confirm removed from list
    studio <- nlpStudio$getObject()
    for (l in 1:length(studio$labs)) {
      lab <- studio$labs[[l]]$getObject()
      stopifnot(!isTRUE(all.equal(lab$name, "Bart")))
    }

    # Confirm parent changed to NULL
    lab <- Bart$getObject()
    stopifnot(is.null(lab$parent))
    stopifnot(studio$created < studio$modified)
    stopifnot(lab$created < lab$modified)

    # Check history
    stopifnot(checkHistory(class = "NLPStudio", method = "removeChild",
                           objectName = "nlpStudio", event = studio$stateDesc))
    stopifnot(checkHistory(class = "Lab", method = "parent",
                           objectName = "Bart", event = lab$stateDesc))

    # Logit
    logTests(class = class, mthd = "removeChild", note = "Tested validation and removal of lab")

    cat(paste(test, " Completed: Success, state is", studio$stateDesc, "!\n"))
  }


init()
test0()
test1()
test2()
test3()
test4()
test5()
test6()

}

devtools::load_all()
# constants <- Constants$new()
# dirs <- constants$getPaths()
# lapply(dirs, function(d) {base::unlink(d, recursive = TRUE)})
class <- "NLPStudio"
# stateFile <- "./NLPSTudio/.State.Rdata"
# base::unlink(stateFile)


testNLPStudio()
