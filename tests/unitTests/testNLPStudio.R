testNLPStudio <- function() {

  init <- function() {

    # Clean up
    if (exists("Beats", envir = .GlobalEnv)) rm("Beats", envir = .GlobalEnv)
    if (exists("Bart", envir = .GlobalEnv))rm("Bart", envir = .GlobalEnv)

    # Source state and log
#    source("./tests/testFunctions/checkState.r")
    source("./tests/testFunctions/logTests.r")

    # Test file paths
    csvTestFile <- "./tests/testFiles/contractions.csv"
    textTestFile <- "./tests/testFiles/en_US.news.txt"
    rdataTestFile <- "./tests/testFiles/quadgrams.Rdata"

  }

  # Test 0: Confirm instantiation of nlpStudio
  test0 <- function() {
    test <- "test0: Instantiation"
    cat(paste("\n",test, " Commencing\r"))

    # Logit
    logTests(class = class, mthd = "initiate", note = "Successfully created nlpStudio")
    logTests(class = class, mthd = "initiate", note = "Create and modified dates initialized correctly")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test1 = function() {
    test <- "test1: getObject() No labs"
    cat(paste("\n",test, " Commencing\r"))

    # Test instantiation and getObect
    studio <<- nlpStudio$getObject() #Return list with warning
    stopifnot(studio$name == "nlpStudio")
    stopifnot(studio$desc == "NLPStudio: Natural Language Processing Studio")
    stopifnot(length(studio$stateDesc) > 0)
    stopifnot(length(studio$labs) == 0)
    stopifnot((Sys.time() - studio$created) < 1)
    stopifnot((Sys.time() - studio$modified) < 1)

    # Logit
    logTests(class = class, mthd = "getObject()", note = "Successfully returned list")

    cat(paste("\n", test, " Completed: Success, state is", studio$stateDesc, "!\n"))
  }

  test2 <- function() {
    test <- "test2: Add labs"
    cat(paste("\n",test, " Commencing\r"))

    # Instantiate lab
    Lab$new(name = "Beats", desc = "Beats Lab")
    l <- Beats$getObject()
    stopifnot(l$name == "Beats")
    stopifnot(l$desc == "Beats Lab")
    stopifnot(isTRUE(all.equal(l$parent, nlpStudio)))
    stopifnot(l$parentName == "nlpStudio")
    stopifnot((Sys.time() - l$created) < 1)
    stopifnot((Sys.time() - l$modified) < 1)


    # Validation
    #nlpStudio$addChild() # fails, lab missing: success
    #nlpStudio$addChild(Writer) # fails, invalid class: success

    # Successful attempt
    nlpStudio$addChild(Beats)

    # Confirm modified date updated
    studio <<- nlpStudio$getObject()
    stopifnot((Sys.time() - studio$created) > 0.15)
    stopifnot((Sys.time() - studio$modified) < 0.5)

    # Logit
    logTests(class = class, mthd = "addChild", note = "Beats lab added successfully")
    logTests(class = class, mthd = "addChild", note = "Date modified updated correctly")


    cat(paste("\n", test, " Completed: Success, state is", studio$stateDesc, "!\n"))
  }


  test3 <- function() {
    test <- "test3: getObject() with Lab"
    cat(paste("\n",test, " Commencing\r"))

    # Test getObject list format
    studio <<- nlpStudio$getObject()
    stopifnot(studio$name == "nlpStudio")
    stopifnot(studio$desc == "NLPStudio: Natural Language Processing Studio")
    stopifnot(length(studio$stateDesc) >0 )
    stopifnot((Sys.time() - studio$created) > .05)
    stopifnot((Sys.time() - studio$modified) < 1)

    stopifnot(length(studio$labs) == 1)
    lab <- studio$labs[[1]]$getObject()
    stopifnot(lab$name == "Beats")
    stopifnot(lab$desc == "Beats Lab")
    stopifnot(lab$parentName == "nlpStudio")
    stopifnot((Sys.time() - lab$created) < 1)
    stopifnot((Sys.time() - lab$modified) < 1)

    # Logit
    logTests(class = class, mthd = "getObject(type = 'list')", note = "Successfully returned list type")
    logTests(class = class, mthd = "addChild", note = "Successfully added lab")

    cat(paste("\n", test, " Completed: Success, state is", studio$stateDesc, "!\n"))
  }

  test4 <- function() {
    test <- "test4: addChild() #2"
    cat(paste("\n",test, " Commencing\r"))

    Lab$new(name = "Bart",desc = "Simpsons Lab")
    nlpStudio$addChild(Bart)

    # Confirm modified date updated
    studio <<- nlpStudio$getObject()
    stopifnot((Sys.time() - studio$created) > 0.2)
    stopifnot((Sys.time() - studio$modified) < 2)

    # Logit
    logTests(class = class, mthd = "addChild(enter = 'TRUE')", note = "Successfully added lab")
    logTests(class = class, mthd = "addChild(enter = 'TRUE')", note = "Date modified updated correctly")

    cat(paste("\n", test, " Completed: Success, state is", studio$stateDesc, "!\n"))
  }

  test5 <- function() {
    test <- "test5: getObject() with two labs"
    cat(paste("\n",test, " Commencing\r"))

    # Test getObject list format
    studio <<- nlpStudio$getObject()
    stopifnot(studio$name == "nlpStudio")
    stopifnot(studio$desc == "NLPStudio: Natural Language Processing Studio")

    stopifnot(length(studio$labs) == 2)
    beats <- studio$labs[[1]]$getObject()
    stopifnot(beats$name == "Beats")
    stopifnot(beats$desc == "Beats Lab")
    stopifnot(beats$parentName == "nlpStudio")

    bart <- studio$labs[[2]]$getObject()
    stopifnot(bart$name == "Bart")
    stopifnot(bart$desc == "Simpsons Lab")
    stopifnot(bart$parentName == "nlpStudio")

    # Logit
    logTests(class = class, mthd = "getObject('list')", note = "Successfully returned nlpStudio list with two labs")
    logTests(class = class, mthd = "addChild", note = "Successfully added 2nd lab")

    cat(paste("\n", test, " Completed: Success, state is", studio$stateDesc, "!\n"))

  }


  test6 <- function() {
    test <- "test6: Remove lab"
    cat(paste("\n",test, " Commencing\r"))

    # Validation
    #nlpStudio$removeChild() # fail lab missing: success
    #nlpStudio$removeChild(ditto) # fail does not exist: success
    #nlpStudio$removeChild("labsDir") # fail not a lab

    # Successfuly remove Bart
    nlpStudio$removeChild(Bart)
    stopifnot(exists("Bart") == FALSE)
    stopifnot(exists("Beats") == TRUE)

    # Confirm date modified updated correctly
    studio <- nlpStudio$getObject()
    stopifnot((Sys.time() - studio$created) > 1)
    stopifnot((Sys.time() - studio$modified) < 5)

    # # Confirm removed from list
    labs <<- Bart$getObject()
    for (l in 1:length(labs)) {
      stopifnot(!isTRUE(all.equal(labs[[l]]$name, "Bart")))
    }

    # Logit
    logTests(class = class, mthd = "removeChild", note = "Tested validation and removal of lab")
    logTests(class = class, mthd = "removeChild", note = "Tested archive of files and inventory of archives")

    cat(paste("\n", test, " Completed: Success, state is", studio$stateDesc, "!\n"))
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

constants <- Constants$new()
dirs <- constants$getPaths()
lapply(dirs, function(d) {base::unlink(d, recursive = TRUE)})
class <- "NLPStudio"
# stateFile <- "./NLPSTudio/.State.Rdata"
# base::unlink(stateFile)

devtools::load_all()
testNLPStudio()
